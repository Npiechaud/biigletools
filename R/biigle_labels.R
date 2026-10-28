# Internal helper for authenticated GET + JSON parsing.
.biigle_get <- function(endpoint,
                        base_url = "https://biigle.de/api/v1",
                        login = NULL,
                        token = NULL) {
  creds <- .biigle_get_credentials(login = login, token = token)

  res <- httr::GET(
    url = paste0(base_url, endpoint),
    httr::authenticate(creds$login, creds$token, type = "basic"),
    httr::accept_json()
  )

  httr::stop_for_status(res)

  jsonlite::fromJSON(
    httr::content(res, as = "text", encoding = "UTF-8"),
    simplifyDataFrame = TRUE
  )
}

#' Check whether a label exists in project label trees
#'
#' Queries all label trees attached to a BIIGLE project and checks whether
#' a given label (by numeric ID or exact name, case-insensitive) exists.
#'
#' @param project_id Numeric/integer-like BIIGLE project ID.
#' @param label Label ID (numeric/integer-like) or label name (character).
#' @param base_url BIIGLE API base URL.
#'   Default: `"https://biigle.de/api/v1"`.
#' @param login BIIGLE login/email. If `NULL`, uses `BIIGLE_LOGIN`.
#' @param token BIIGLE API token. If `NULL`, uses `BIIGLE_TOKEN`.
#'
#' @return A list with:
#' \describe{
#'   \item{found}{Logical. `TRUE` if one or more matches were found.}
#'   \item{matches}{`data.frame` of matches or `NULL` if none. Columns:
#'   `project_id`, `label_tree_id`, `label_tree_name`, `label_id`, `label_name`.}
#' }
#'
#' @export
label_in_project <- function(project_id,
                             label,
                             base_url = "https://biigle.de/api/v1",
                             login = NULL,
                             token = NULL) {
  if (missing(project_id) || is.null(project_id) || !nzchar(as.character(project_id))) {
    stop("`project_id` is required.", call. = FALSE)
  }
  if (missing(label) || is.null(label) || !nzchar(as.character(label))) {
    stop("`label` is required (ID or name).", call. = FALSE)
  }

  label_is_id <- is.numeric(label) || (is.character(label) && grepl("^[0-9]+$", trimws(label)))
  label_id <- if (label_is_id) as.numeric(label) else NA_real_
  label_name <- if (!label_is_id) tolower(trimws(as.character(label))) else NA_character_

  trees <- .biigle_get(
    endpoint = paste0("/projects/", project_id, "/label-trees"),
    base_url = base_url,
    login = login,
    token = token
  )

  trees_df <- as.data.frame(trees, stringsAsFactors = FALSE)
  if (nrow(trees_df) == 0) {
    return(list(found = FALSE, matches = NULL))
  }
  if (!"id" %in% names(trees_df)) {
    stop("Unexpected API response: project label-tree list has no `id` column.", call. = FALSE)
  }

  hit_rows <- list()
  k <- 1L

  for (i in seq_len(nrow(trees_df))) {
    tree_id <- trees_df$id[i]
    tree_name <- if ("name" %in% names(trees_df)) trees_df$name[i] else NA_character_

    tree <- .biigle_get(
      endpoint = paste0("/label-trees/", tree_id),
      base_url = base_url,
      login = login,
      token = token
    )

    labs <- tree$labels
    if (is.null(labs)) next

    labs_df <- as.data.frame(labs, stringsAsFactors = FALSE)
    if (nrow(labs_df) == 0) next
    if (!all(c("id", "name") %in% names(labs_df))) next

    is_match <- if (label_is_id) {
      suppressWarnings(as.numeric(labs_df$id) == label_id)
    } else {
      tolower(trimws(as.character(labs_df$name))) == label_name
    }

    is_match[is.na(is_match)] <- FALSE

    if (any(is_match)) {
      hit_rows[[k]] <- data.frame(
        project_id = as.character(project_id),
        label_tree_id = rep(tree_id, sum(is_match)),
        label_tree_name = rep(tree_name, sum(is_match)),
        label_id = labs_df$id[is_match],
        label_name = labs_df$name[is_match],
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
  }

  matches <- if (length(hit_rows) > 0) do.call(rbind, hit_rows) else NULL
  list(found = !is.null(matches) && nrow(matches) > 0, matches = matches)
}
