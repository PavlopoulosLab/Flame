# syntax=docker/dockerfile:1
# =============================================================================
# Flame 3.0 — web app + REST API in one image
#   docker build -t pavlopouloslab/flame:3.0 .
#   docker run -d -p 8000:8000 -v flame-data:/data pavlopouloslab/flame:3.0
# Then open http://localhost:8000 (API: /api/v1, docs: /__docs__/).
# To serve under a subpath instead (e.g. example.com/flame), add
# --build-arg FLAME_BASE_PATH=/flame/ and have the reverse proxy strip that
# prefix before forwarding to this container.
# =============================================================================

ARG R_VERSION=4.6.2
# Set to a subpath (e.g. "/flame/") to serve the app behind a reverse proxy
# that strips that prefix before forwarding to this container; it's baked into
# the built asset paths and the router's basename, so it can't be changed at
# container run time. Leave unset ("/") for a normal, root-served deployment.
ARG FLAME_BASE_PATH=/

# ---- Stage 1: build the web app (React + TypeScript) -------------------------
FROM node:22.12.0-bookworm-slim AS web
ARG FLAME_BASE_PATH
WORKDIR /src/web
COPY web/package.json web/package-lock.json ./
RUN npm ci --no-audit --no-fund
COPY web/ ./
RUN FLAME_BASE_PATH=$FLAME_BASE_PATH npm run build

# ---- Stage 2: R runtime with the plumber API ---------------------------------
FROM rocker/r-ver:${R_VERSION} AS app

LABEL org.opencontainers.image.title="Flame 3.0" \
      org.opencontainers.image.description="Functional enrichment analysis and integration of multiple gene lists: web app and REST API" \
      org.opencontainers.image.source="https://github.com/PavlopoulosLab/Flame" \
      org.opencontainers.image.licenses="GPL-3.0" \
      org.opencontainers.image.version="3.0.0"

ENV DEBIAN_FRONTEND=noninteractive \
    TZ=Etc/UTC \
    LANG=en_US.UTF-8 \
    FLAME_HOST=0.0.0.0 \
    FLAME_PORT=8000 \
    FLAME_DB=/data/flame.sqlite \
    FLAME_WORKERS=4 \
    FLAME_RETENTION_DAYS=30

# System libraries for the R packages (curl/openssl/xml for web APIs, sodium for
# plumber, glpk/gmp for igraph, libuv for the fs package pulled in by
# clusterProfiler's dependency tree, image and font libraries for
# clusterProfiler's plotting dependencies). Rust is needed only to build
# WebGestaltR from source and is removed again in the same layer.
COPY install_packages.R DESCRIPTION /tmp/flame/
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      curl ca-certificates libcurl4-openssl-dev libssl-dev libxml2-dev libsodium-dev \
      libglpk-dev libgmp3-dev zlib1g-dev libpng-dev libjpeg-dev libtiff5-dev \
      libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev libicu-dev \
      libuv1-dev cargo rustc \
 && FLAME_RUNTIME_ONLY=1 Rscript /tmp/flame/install_packages.R \
 && apt-get purge -y cargo rustc && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/* /tmp/flame /tmp/Rtmp* /root/.cache /root/.cargo

# Application code (the R package, the API and the built web app).
WORKDIR /opt/flame
COPY DESCRIPTION NAMESPACE ./
COPY R/ R/
COPY inst/ inst/
COPY api/ api/
COPY --from=web /src/web/dist/ web/dist/

# Run as an unprivileged user; workspaces live in the /data volume.
RUN useradd --system --create-home --home-dir /home/flame --uid 10001 flame \
 && mkdir -p /data && chown -R flame:flame /data /home/flame
USER flame
VOLUME ["/data"]
EXPOSE 8000

HEALTHCHECK --interval=30s --timeout=5s --start-period=90s --retries=3 \
  CMD curl -fsS "http://127.0.0.1:${FLAME_PORT}/api/v1/health" || exit 1

CMD ["Rscript", "/opt/flame/api/run.R"]
