# Start the Flame API (and, if built, the web app on the same origin).
#   Rscript api/run.R                     dev: http://127.0.0.1:8000
# Environment:
#   FLAME_PORT, FLAME_HOST, FLAME_DB (SQLite path), FLAME_WORKERS,
#   FLAME_RETENTION_DAYS (default 30), FLAME_MAX_LISTS (default 10),
#   FLAME_PUBLIC_URL, FLAME_SMTP_URL / FLAME_SMTP_FROM (optional email notice)
# Production: run behind an HTTPS reverse proxy on port 443.

args <- commandArgs(trailingOnly = FALSE)
scriptPath <- sub("^--file=", "", args[grepl("^--file=", args)])
FLAME_ROOT <- if (length(scriptPath) == 1) {
  normalizePath(file.path(dirname(scriptPath), ".."))
} else {
  normalizePath(getwd())
}

suppressMessages(suppressWarnings(
  pkgload::load_all(FLAME_ROOT, export_all = TRUE, helpers = FALSE, quiet = TRUE)))

source(file.path(FLAME_ROOT, "api", "helpers.R"))
source(file.path(FLAME_ROOT, "api", "jobs.R"))

invisible(db())
purge_expired_workspaces()
later::later(function() {
  hourly <- function() { try(purge_expired_workspaces(), silent = TRUE); later::later(hourly, 3600) }
  hourly()
}, 3600)
if (!identical(Sys.getenv("FLAME_NO_WORKERS"), "1")) start_job_workers()

port <- as.integer(Sys.getenv("FLAME_PORT", "8000"))
host <- Sys.getenv("FLAME_HOST", "127.0.0.1")

pr <- plumber::pr(file.path(FLAME_ROOT, "api", "plumber.R"))
routeDir <- file.path(FLAME_ROOT, "api", "routes")
for (f in sort(list.files(routeDir, pattern = "\\.R$", full.names = TRUE))) {
  pr <- plumber::pr_mount(pr, paste0("/api/", sub("\\.R$", "", basename(f))), plumber::pr(f))
}

# ---- Web app (production build) on the same origin --------------------------
dist <- file.path(FLAME_ROOT, "web", "dist")
CSP <- paste(
  "default-src 'self'", "script-src 'self'", "style-src 'self' 'unsafe-inline'",
  "img-src 'self' data: blob:", "font-src 'self' data:", "connect-src 'self'",
  "worker-src 'self' blob:", "frame-ancestors 'none'", "base-uri 'self'",
  "form-action 'self'", "object-src 'none'", sep = "; ")

if (dir.exists(dist)) {
  pr <- plumber::pr_static(pr, "/assets", file.path(dist, "assets"))
  # Static root files (favicon, logo, theme-init.js, examples) and SPA fallback.
  pr <- plumber::pr_filter(pr, "spa", function(req, res) {
    path <- req$PATH_INFO
    if (startsWith(path, "/api/") || startsWith(path, "/assets/") || startsWith(path, "/__docs__")) {
      return(plumber::forward())
    }
    file <- normalizePath(file.path(dist, sub("^/", "", path)), mustWork = FALSE)
    if (nzchar(path) && path != "/" && startsWith(file, dist) && file.exists(file) && !dir.exists(file)) {
      res$setHeader("Content-Type", mime::guess_type(file))
      res$setHeader("Cache-Control", "public, max-age=3600")
      res$body <- readBin(file, "raw", file.info(file)$size)
      return(res)
    }
    res$setHeader("Content-Type", "text/html; charset=utf-8")
    res$setHeader("Content-Security-Policy", CSP)
    res$setHeader("Cache-Control", "no-cache")
    # Read per request so a fresh `npm run build` is picked up without a restart.
    res$body <- paste(readLines(file.path(dist, "index.html"), warn = FALSE), collapse = "\n")
    res
  })
}

message("Flame listening on http://", host, ":", port)
plumber::pr_run(pr, host = host, port = port, docs = TRUE, quiet = TRUE)
