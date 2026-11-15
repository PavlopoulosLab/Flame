# =============================================================================
# FLAME REST API (plumber) — root router
# =============================================================================
# api/run.R builds this router and mounts api/routes/<name>.R at /api/<name>.
# Workspace identity: header `X-Flame-Workspace: <32 hex>` (no cookies).
# =============================================================================

library(plumber)

#* @apiTitle Flame 3.0 API
#* @apiDescription Functional and literature enrichment of multiple gene lists. Free and open to all users, no login required. Create a workspace with POST /api/workspace and send its id in the X-Flame-Workspace header on every other call.
#* @apiVersion 3.0.0

.rate <- new.env()

# ---- Security headers, body-size limit and a simple per-IP rate limit -------
#* @filter guard
function(req, res) {
  res$setHeader("X-Content-Type-Options", "nosniff")
  res$setHeader("Referrer-Policy", "strict-origin-when-cross-origin")
  res$setHeader("X-Frame-Options", "DENY")
  res$setHeader("Cross-Origin-Opener-Policy", "same-origin")
  res$setHeader("Permissions-Policy", "camera=(), microphone=(), geolocation=()")
  if (startsWith(req$PATH_INFO %||% "", "/api/")) res$setHeader("Cache-Control", "no-store")

  # Mounted routers serve their root at "/api/<name>/": accept "/api/<name>" too.
  if (grepl("^/api/[a-z-]+$", req$PATH_INFO %||% "")) req$PATH_INFO <- paste0(req$PATH_INFO, "/")

  len <- suppressWarnings(as.numeric(req$CONTENT_LENGTH %||% "0"))
  if (!is.na(len) && len > 2.5 * 1024^2) {
    res$status <- 413
    return(list(error = "Request is larger than the 2 MB limit."))
  }

  # 600 API requests per minute per client address (generous for the UI's polling).
  if (startsWith(req$PATH_INFO %||% "", "/api/")) {
    ip <- req$HTTP_X_FORWARDED_FOR %||% req$REMOTE_ADDR %||% "local"
    ip <- trimws(strsplit(ip, ",")[[1]][1])
    minute <- floor(as.numeric(Sys.time()) / 60)
    key <- paste(ip, minute)
    n <- (.rate[[key]] %||% 0) + 1
    .rate[[key]] <- n
    if (length(ls(.rate)) > 5000) rm(list = ls(.rate), envir = .rate)
    if (n > 600) {
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
