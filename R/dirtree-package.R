#' dirtree: directory-style taxonomy trees on ggplot axes
#'
#' @importFrom rlang %||%
#' @importFrom cli cli_abort
#' @keywords internal
"_PACKAGE"

# cli-formatted abort with the caller's context (profile § 9). The glue in
# `message` is interpolated in the frame that calls .dt_abort (where the
# referenced locals live); `call` only locates the error for the user.
.dt_abort <- function(message, ..., call = rlang::caller_env()) {
  cli::cli_abort(message, ..., call = call, .envir = rlang::caller_env())
}
