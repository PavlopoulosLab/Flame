# =============================================================================
# FLAME REST API (plumber) — root router
# =============================================================================
# api/run.R builds this router and mounts api/routes/<name>.R at /api/v1/<name>
# (the versioned public API) and at /api/<name> (used by the web app).
# Workspace identity: header `X-Flame-Workspace: <32 hex>` (no cookies).
# =============================================================================

library(plumber)

#* @apiTitle Flame 3.0 API
#* @apiDescription Functional and literature enrichment of multiple gene lists. Free and open to all users. Create a workspace with POST /api/workspace and send its id in the X-Flame-Workspace header on every other call.
#* @apiVersion 3.0.0

.rate <- new.env()

# The client address for the rate limit. X-Forwarded-For is set by whoever sends the
# request, so it is only used when the request comes from the reverse proxy on this
# machine (or FLAME_TRUST_PROXY=1), and then its last entry, the one the proxy added.
client_address <- function(req) {
  remote <- req$REMOTE_ADDR %||% "local"
  xff <- req$HTTP_X_FORWARDED_FOR %||% ""
  trusted <- remote %in% c("127.0.0.1", "::1", "::ffff:127.0.0.1") || identical(Sys.getenv("FLAME_TRUST_PROXY"), "1")
  if (!trusted || !nzchar(xff)) return(paste0("ip:", remote))
  hops <- trimws(strsplit(xff, ",")[[1]])
  paste0("ip:", utils::tail(hops[nzchar(hops)], 1) %||% remote)
}

# ---- Security headers, body-size limit and a simple per-IP rate limit -------
#* @filter guard
function(req, res) {
  res$setHeader("X-Content-Type-Options", "nosniff")
  res$setHeader("Referrer-Policy", "strict-origin-when-cross-origin")
  res$setHeader("X-Frame-Options", "DENY")
  res$setHeader("Cross-Origin-Opener-Policy", "same-origin")
  res$setHeader("Permissions-Policy", "camera=(), microphone=(), geolocation=()")
  if (startsWith(req$PATH_INFO %||% "", "/api/")) res$setHeader("Cache-Control", "no-store")

  # Mounted routers serve their root at "/api[/v1]/<name>/": accept it without the slash too.
  if (grepl("^/api(/v1)?/[a-z-]+$", req$PATH_INFO %||% "") && !grepl("/health$", req$PATH_INFO))
    req$PATH_INFO <- paste0(req$PATH_INFO, "/")

  len <- suppressWarnings(as.numeric(req$CONTENT_LENGTH %||% "0"))
  isImport <- grepl("^/api(/v1)?/workspace/import/?$", req$PATH_INFO %||% "")
  isExpression <- grepl("^/api(/v1)?/inputs/expression/", req$PATH_INFO %||% "")
  isTable <- grepl("^/api(/v1)?/inputs/(table/prepare|volcano/points|reduction/points)/?$", req$PATH_INFO %||% "")
  limit <- if (isImport) FLAME_LIMITS$maxSessionBytes else if (isExpression) EXPRESSION_MAX_BYTES + 1024^2
           else if (isTable) TABLE_INPUT_MAX_BYTES + 1024^2 else 2.5 * 1024^2
  if (!is.na(len) && len > limit) {
    res$status <- 413
    return(list(error = if (isImport) sprintf("The session file is larger than the %d MB limit.", round(limit / 1024^2))
                        else if (isExpression) sprintf("The expression table is larger than the %d MB limit.", round(EXPRESSION_MAX_BYTES / 1024^2))
                        else if (isTable) sprintf("The table is larger than the %d MB limit.", round(TABLE_INPUT_MAX_BYTES / 1024^2))
                        else "Request is larger than the 2 MB limit."))
  }

  # API requests per minute per client address (FLAME_RATE_LIMIT, default 600 — generous
  # for the UI's polling; 0 turns the limit off, e.g. for local test runs).
  rateLimit <- suppressWarnings(as.numeric(Sys.getenv("FLAME_RATE_LIMIT", "600")))
  if (is.na(rateLimit)) rateLimit <- 600
  if (rateLimit > 0 && startsWith(req$PATH_INFO %||% "", "/api/")) {
    ip <- client_address(req)
    minute <- floor(as.numeric(Sys.time()) / 60)
    # Counters live for one minute: start afresh when the minute changes, so the table
    # stays small without letting anyone reset the other clients' counters.
    if (!identical(.rate$.minute, minute)) { rm(list = ls(.rate, all.names = TRUE), envir = .rate); .rate$.minute <- minute }
    n <- (.rate[[ip]] %||% 0) + 1
    .rate[[ip]] <- n
    if (n > rateLimit) {
      res$status <- 429
      return(list(error = "Too many requests. Please slow down and try again in a minute."))
    }
  }
  plumber::forward()
}

#* Health check
#* @get /api/health
#* @serializer unboxedJSON
function() {
  list(status = "ok", version = as.character(utils::packageVersion("flame")))
}

#* Health check: server status and version
#* @get /api/v1/health
#* @serializer unboxedJSON
function() {
  list(status = "ok", version = as.character(utils::packageVersion("flame")), api = "v1")
}
