# =============================================================================
# FLAME core: progress reporting from background jobs
# =============================================================================
# A job runs in a worker process with option flame.progress_file set to a file
# path. flame_progress() writes the latest step there (atomically); the API
# process reads it while the job runs. Outside a job it does nothing.
# =============================================================================

flame_progress <- function(fraction, message) {
  f <- getOption("flame.progress_file")
  if (is.null(f) || !nzchar(f)) return(invisible(FALSE))
  tmp <- paste0(f, ".tmp")
  ok <- tryCatch({
    writeLines(jsonlite::toJSON(list(fraction = max(0, min(1, fraction)), message = substr(as.character(message), 1, 200),
                                     at = as.numeric(Sys.time())), auto_unbox = TRUE), tmp)
    file.rename(tmp, f)
  }, error = function(e) FALSE)
  invisible(ok)
}

#' Latest progress report of a job, or NULL
read_progress <- function(f) {
  if (is.null(f) || !file.exists(f)) return(NULL)
  tryCatch(jsonlite::fromJSON(readLines(f, warn = FALSE)), error = function(e) NULL)
}
