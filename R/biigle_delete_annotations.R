# Delete Biigle label assignments from a CSV or data.frame
# Accepts either:
#   - annotation_label_id / annotations_label_id
#   - image_label_id
#
# Endpoints used:
#   /image-annotation-labels/{id}
#   /image-labels/{id}
#   /video-annotation-labels/{id}
#   /video-labels/{id}

# Internal helper for authenticated DELETE request.
.biigle_delete <- function(endpoint,
                           base_url = "https://biigle.de/api/v1",
                           login = NULL,
                           token = NULL) {
  creds <- .biigle_get_credentials(login = login, token = token)

  res <- httr::DELETE(
    url = paste0(base_url, endpoint),
    httr::authenticate(creds$login, creds$token, type = "basic"),
    httr::accept_json()
  )

  list(
    status = httr::status_code(res),
    ok = httr::status_code(res) %in% c(200, 204),
    text = tryCatch(
      httr::content(res, as = "text", encoding = "UTF-8"),
      error = function(e) NA_character_
    )
  )
}

#' Delete one BIIGLE label assignment
#'
#' @param id Numeric/integer-like ID of the label assignment to delete.
#' @param type One of `"image"` or `"video"`.
#' @param kind One of `"annotation_label"` or `"image_label"`.
#' @param base_url BIIGLE API base URL.
#' @param login BIIGLE login/email. If `NULL`, uses `BIIGLE_LOGIN`.
#' @param token BIIGLE API token. If `NULL`, uses `BIIGLE_TOKEN`.
#' @param verbose Logical; print progress message.
#'
#' @return A list with fields:
#' \describe{
#'   \item{status}{HTTP status code}
#'   \item{ok}{Logical; `TRUE` for status 200/204}
#'   \item{text}{Response body as text (if available)}
#' }
#' @export
delete_one_biigle_label <- function(id,
                                    type = "image",
                                    kind = "annotation_label",
                                    base_url = "https://biigle.de/api/v1",
                                    login = NULL,
                                    token = NULL,
                                    verbose = TRUE) {
  if (missing(id) || is.null(id) || !nzchar(as.character(id))) {
    stop("`id` is required.", call. = FALSE)
  }

  type <- match.arg(type, c("image", "video"))
  kind <- match.arg(kind, c("annotation_label", "image_label"))

  id_num <- suppressWarnings(as.numeric(id))
  if (is.na(id_num)) {
    stop("`id` must be numeric/integer-like.", call. = FALSE)
  }

  prefix <- if (type == "video") "video" else "image"

  endpoint <- if (kind == "annotation_label") {
    paste0("/", prefix, "-annotation-labels/", id_num)
  } else {
    paste0("/", prefix, "-labels/", id_num)
  }

  res <- .biigle_delete(
    endpoint = endpoint,
    base_url = base_url,
    login = login,
    token = token
  )

  if (isTRUE(verbose)) {
    message(
      "DELETE ", endpoint, " -> ", res$status,
      " (", if (isTRUE(res$ok)) "OK" else "FAILED", ")"
    )
  }

  res
}

#' Delete a list of BIIGLE label assignments from a CSV or data.frame
#'
#' @param input Either a data.frame or path to a CSV file.
#' @param id_column Name of ID column. If `NULL`, auto-detects one of:
#'   `"annotation_label_id"`, `"annotations_label_id"`, `"image_label_id"`.
#' @param type_column Optional column containing `"image"` / `"video"`.
#'   If missing, defaults to `"image"`.
#' @param kind_column Optional column containing
#'   `"annotation_label"` / `"image_label"`.
#'   If missing, inferred from `id_column`.
#' @param dry_run If `TRUE`, no DELETE requests are sent.
#' @param confirm_required If `TRUE`, ask for typed confirmations in interactive sessions.
#' @param base_url BIIGLE API base URL.
#' @param login BIIGLE login/email. If `NULL`, uses `BIIGLE_LOGIN`.
#' @param token BIIGLE API token. If `NULL`, uses `BIIGLE_TOKEN`.
#'
#' @return
#' - If cancelled during confirmation: `invisible(NULL)`
#' - If `dry_run = TRUE`: invisible data.frame preview of rows to delete
#' - Otherwise: data.frame with one result row per deletion attempt
#'
#' @export
delete_biigle_label_list <- function(input,
                                     id_column = NULL,
                                     type_column = NULL,
                                     kind_column = NULL,
                                     dry_run = FALSE,
                                     confirm_required = TRUE,
                                     base_url = "https://biigle.de/api/v1",
                                     login = NULL,
                                     token = NULL) {
  # Resolve credentials early so user gets fast feedback
  # (also validates BIIGLE_LOGIN / BIIGLE_TOKEN if not passed explicitly).
  creds <- .biigle_get_credentials(login = login, token = token)

  # 1) Load input
  if (is.character(input) && length(input) == 1L && file.exists(input)) {
    df <- readr::read_csv(input, show_col_types = FALSE)
  } else if (is.data.frame(input)) {
    df <- input
  } else {
    stop("`input` must be a data.frame or a CSV file path.", call. = FALSE)
  }

  # 2) Detect ID column
  if (is.null(id_column)) {
    candidates <- c("annotation_label_id", "annotations_label_id", "image_label_id")
    found <- intersect(candidates, names(df))
    if (length(found) == 0) {
      stop(
        "No ID column found. Expected one of: ",
        paste(candidates, collapse = ", "),
        call. = FALSE
      )
    }
    id_column <- found[1]
  }

  if (!id_column %in% names(df)) {
    stop("ID column '", id_column, "' not found.", call. = FALSE)
  }

  # 3) Optional columns
  if (is.null(type_column) && "type" %in% names(df)) type_column <- "type"
  if (is.null(kind_column) && "kind" %in% names(df)) kind_column <- "kind"

  # 4) Build working fields
  df$id_value <- trimws(as.character(df[[id_column]]))

  if (!is.null(type_column) && type_column %in% names(df)) {
    df$item_type <- tolower(trimws(as.character(df[[type_column]])))
  } else {
    df$item_type <- "image"
  }

  if (!is.null(kind_column) && kind_column %in% names(df)) {
    df$item_kind <- tolower(trimws(as.character(df[[kind_column]])))
  } else {
    df$item_kind <- if (grepl("annotation", id_column, fixed = TRUE)) {
      "annotation_label"
    } else {
      "image_label"
    }
  }

  # 5) Filter invalid rows
  keep <- !is.na(df$id_value) & nzchar(df$id_value)
  df <- df[keep, , drop = FALSE]

  if (nrow(df) == 0) {
    stop("No valid IDs to delete.", call. = FALSE)
  }

  # Normalize type/kind
  df$item_type[!df$item_type %in% c("image", "video")] <- "image"
  inferred_kind <- if (grepl("annotation", id_column, fixed = TRUE)) {
    "annotation_label"
  } else {
    "image_label"
  }
  df$item_kind[!df$item_kind %in% c("annotation_label", "image_label")] <- inferred_kind

  # Keep only numeric/integer-like IDs
  suppressWarnings(id_num <- as.numeric(df$id_value))
  ok_id <- !is.na(id_num)
  if (!all(ok_id)) {
    warning(sum(!ok_id), " row(s) dropped because ID is not numeric/integer-like.")
    df <- df[ok_id, , drop = FALSE]
    id_num <- id_num[ok_id]
  }
  if (nrow(df) == 0) {
    stop("No numeric/integer-like IDs to delete after filtering.", call. = FALSE)
  }

  # 6) Confirmation prompts
  cat("\nYou are about to delete", nrow(df), "BIIGLE label assignment(s).\n")
  cat(
    "First IDs:",
    paste(head(df$id_value, 10), collapse = ", "),
    if (nrow(df) > 10) "..." else "",
    "\n"
  )
  cat("Types:", paste(unique(df$item_type), collapse = ", "), "\n")
  cat("Kinds:", paste(unique(df$item_kind), collapse = ", "), "\n\n")

  if (isTRUE(confirm_required)) {
    if (!interactive()) {
      stop(
        "`confirm_required = TRUE` requires an interactive session. ",
        "Set `confirm_required = FALSE` for non-interactive use.",
        call. = FALSE
      )
    }

    ans1 <- readline("Type DELETE to confirm: ")
    if (!identical(ans1, "DELETE")) {
      cat("Cancelled. Nothing deleted.\n")
      return(invisible(NULL))
    }

    ans2 <- readline("Final confirmation, type YES: ")
    if (!identical(toupper(ans2), "YES")) {
      cat("Cancelled. Nothing deleted.\n")
      return(invisible(NULL))
    }
  }

  if (isTRUE(dry_run)) {
    cat("Dry run enabled. No requests sent.\n")
    preview <- df[, c(id_column, "item_type", "item_kind"), drop = FALSE]
    return(invisible(preview))
  }

  # 7) Delete loop
  out <- data.frame(
    id = character(),
    type = character(),
    kind = character(),
    status = integer(),
    ok = logical(),
    message = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(df))) {
    id_i <- id_num[i]
    type_i <- df$item_type[i]
    kind_i <- df$item_kind[i]

    res <- tryCatch(
      delete_one_biigle_label(
        id = id_i,
        type = type_i,
        kind = kind_i,
        base_url = base_url,
        login = creds$login,
        token = creds$token,
        verbose = FALSE
      ),
      error = function(e) {
        list(status = NA_integer_, ok = FALSE, text = conditionMessage(e))
      }
    )

    out <- rbind(
      out,
      data.frame(
        id = as.character(df$id_value[i]),
        type = type_i,
        kind = kind_i,
        status = if (is.null(res$status)) NA_integer_ else as.integer(res$status),
        ok = isTRUE(res$ok),
        message = if (is.null(res$text)) "" else as.character(res$text),
        stringsAsFactors = FALSE
      )
    )

    message(
      sprintf(
        "[%d/%d] %s (%s, %s) -> %s",
        i, nrow(df), df$id_value[i], type_i, kind_i,
        if (isTRUE(res$ok)) "DELETED" else paste("FAILED", res$status)
      )
    )
  }

  out
}
