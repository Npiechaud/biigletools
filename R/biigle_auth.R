# Internal helper to resolve BIIGLE credentials from function args or environment.
# Environment variables:
#   BIIGLE_LOGIN
#   BIIGLE_TOKEN
.biigle_get_credentials <- function(login = NULL, token = NULL) {
  login <- if (is.null(login) || !nzchar(login)) Sys.getenv("BIIGLE_LOGIN") else login
  token <- if (is.null(token) || !nzchar(token)) Sys.getenv("BIIGLE_TOKEN") else token

  if (!nzchar(login)) {
    stop(
      "Missing Biigle login. Provide `login=` or set BIIGLE_LOGIN in your environment.",
      call. = FALSE
    )
  }
  if (!nzchar(token)) {
    stop(
      "Missing Biigle token. Provide `token=` or set BIIGLE_TOKEN in your environment.",
      call. = FALSE
    )
  }

  list(login = login, token = token)
}
