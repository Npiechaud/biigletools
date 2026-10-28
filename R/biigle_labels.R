# Reusable Biigle API helpers for label-tree checks
# Requires credentials in environment/.Rprofile:
#   BiigleDElogin
#   BiigleDEtoken

library(httr)
library(jsonlite)

.biigle_get <- function(endpoint,
                        base_url = "https://biigle.de/api/v1",
                        login = BiigleDElogin,
                        token = BiigleDEtoken) {
  res <- GET(
    url = paste0(base_url, endpoint),
    authenticate(login, token, type = "basic"),
    accept_json()
  )
  stop_for_status(res)
  fromJSON(content(res, as = "text", encoding = "UTF-8"), simplifyDataFrame = TRUE)
}

#' Check whether a label exists in any label tree attached to a project
#'
#' @param project_id Numeric/integer Biigle project ID
#' @param label Label ID (numeric) or label name (character)
#' @param base_url Biigle API base URL
#' @param login Biigle login (email), default from BiigleDElogin
#' @param token Biigle API token, default from BiigleDEtoken
#'
#' @return A list with:
#'   - found: TRUE/FALSE
#'   - matches: data.frame of matching labels (or NULL)
#' @export
label_in_project <- function(project_id,
                             label,
                             base_url = "https://biigle.de/api/v1",
                             login = BiigleDElogin,
                             token = BiigleDEtoken) {

  if (missing(project_id) || is.null(project_id) || project_id == "") {
    stop("project_id is required.")
  }
  if (missing(label) || is.null(label) || (is.character(label) && trimws(label) == "")) {
    stop("label is required (ID or name).")
  }

  trees <- .biigle_get(
    endpoint = paste0("/projects/", project_id, "/label-trees"),
    base_url = base_url, login = login, token = token
  )

  if (is.null(trees) || nrow(as.data.frame(trees)) == 0) {
    return(list(found = FALSE, matches = NULL))
  }

  hit_rows <- list()
  k <- 1L

  for (i in seq_len(nrow(trees))) {
    tree_id <- trees$id[i]
    tree_name <- if ("name" %in% names(trees)) trees$name[i] else NA_character_

    tree <- .biigle_get(
      endpoint = paste0("/label-trees/", tree_id),
      base_url = base_url, login = login, token = token
    )

    labs <- tree$labels
    if (is.null(labs) || nrow(as.data.frame(labs)) == 0) next

    is_match <- if (is.numeric(label)) {
      labs$id == label
    } else {
      tolower(trimws(labs$name)) == tolower(trimws(as.character(label)))
    }

    if (any(is_match, na.rm = TRUE)) {
      hit_rows[[k]] <- data.frame(
        project_id = project_id,
        label_tree_id = tree_id,
        label_tree_name = tree_name,
        label_id = labs$id[is_match],
        label_name = labs$name[is_match],
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
  }

  matches <- if (length(hit_rows) > 0) do.call(rbind, hit_rows) else NULL
  list(found = !is.null(matches) && nrow(matches) > 0, matches = matches)
}