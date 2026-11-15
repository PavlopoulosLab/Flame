# =============================================================================
# FLAME Enrichment Session Registry
# =============================================================================
# Plain in-memory store of EnrichmentSession objects, keyed by session id.
# Used by build_comparison() to hand a set of runs to CombinationSession.
# (Persistence across requests is the API's job: api/helpers.R, SQLite.)
# =============================================================================

EnrichmentSessionRegistry <- R6::R6Class(
  "EnrichmentSessionRegistry",

  public = list(
    initialize = function() {
      private$.sessions <- list()
    },

    #' Add a session (replaces an existing session with the same id)
    add = function(session) {
      if (!inherits(session, "EnrichmentSession")) {
        stop("Can only add EnrichmentSession objects to the registry")
      }
      private$.sessions[[session$id]] <- session
      invisible(self)
    },

    get = function(id) private$.sessions[[id]],

    exists = function(id) id %in% names(private$.sessions),

    getAll = function() private$.sessions,

    remove = function(id) {
      private$.sessions[[id]] <- NULL
      invisible(self)
    },

    count = function() length(private$.sessions)
  ),

  private = list(
    .sessions = NULL
  )
)
