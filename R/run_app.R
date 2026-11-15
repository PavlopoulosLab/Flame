#' Run Flame
#'
#' Starts the Flame server: the REST API and, when built (`npm run build` in
#' `web/`), the web application on the same port.
#'
#' @param port Port number. Default 8000.
#' @param host Interface to listen on. Default "127.0.0.1"; use "0.0.0.0" in
#'   containers (put an HTTPS reverse proxy in front for public deployments).
#' @param workers Number of background enrichment workers.
#'
#' @return Invisible exit status of the server process.
#'
#' @examples
#' \dontrun{
#' run_flame()
#' }
#'
#' @export
run_flame <- function(port = 8000, host = "127.0.0.1", workers = 4) {
  root <- getFlameRoot()
  script <- file.path(root, "api", "run.R")
  if (!file.exists(script)) {
    stop("Could not find api/run.R under ", root, call. = FALSE)
  }
  env <- c(sprintf("FLAME_PORT=%s", port), sprintf("FLAME_HOST=%s", host),
           sprintf("FLAME_WORKERS=%s", workers))
  invisible(system2(file.path(R.home("bin"), "Rscript"), shQuote(script), env = env))
}
