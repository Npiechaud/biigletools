# Delete Biigle label assignments from a CSV or data.frame
# Accepts either:
#   - annotation_label_id / annotations_label_id
#   - image_label_id
#
# Intended endpoints:
#   image-annotation-labels/{id}
#   image-labels/{id}
#   video-annotation-labels/{id}
#   video-labels/{id}
#
# Requires credentials in environment/.Rprofile:
#   BiigleDElogin
#   BiigleDEtoken

library(httr)
library(jsonlite)
library(readr)

biigle_delete <- function(endpoint,
                          base_url = "https://biigle.de/api/v1",
                          login = BiigleDElogin,
                          token = BiigleDEtoken) {
  res <- DELETE(
    url = paste0(base_url, endpoint),
    authenticate(login, token, type = "basic"),
    accept_json()
  )
  
  list(
    status = status_code(res),
    ok = status_code(res) %in% c(200, 204),
    text = tryCatch(content(res, as = "text", encoding = "UTF-8"),
                    error = function(e) NA_character_)
  )
}


#' Delete one Biigle label assignment
#'
#' @param id Numeric ID of the label assignment to delete
#' @param type One of: "image", "video"
#' @param kind One of: "annotation_label", "image_label"
#' @param verbose Print progress
#' @return list(status, ok, text)
#' @export
delete_one_biigle_label <- function(id,
                                    type = "image",
                                    kind = "annotation_label",
                                    base_url = "https://biigle.de/api/v1",
                                    login = BiigleDElogin,
                                    token = BiigleDEtoken,
                                    verbose = TRUE) {
  if (missing(id) || is.null(id) || is.na(id) || id == "") {
    stop("id is required.")
  }
  
  type <- match.arg(type, c("image", "video"))
  kind <- match.arg(kind, c("annotation_label", "image_label"))
  
  prefix <- if (type == "video") "video" else "image"
  
  endpoint <- if (kind == "annotation_label") {
    paste0("/", prefix, "-annotation-labels/", id)
  } else {
    paste0("/", prefix, "-labels/", id)
  }
  
  res <- biigle_delete(endpoint, base_url = base_url, login = login, token = token)
  
  if (verbose) {
    message("DELETE ", endpoint, " -> ", res$status, " (", if (res$ok) "OK" else "FAILED", ")")
  }
  
  res
}

#' Delete a list of Biigle label assignments from a CSV or data.frame.
#'
#' @param input Either a data.frame or a path to a CSV file.
#' @param id_column Name of the ID column in the table. It will auto-detect:
#'   annotation_label_id, annotations_label_id, image_label_id
#' @param type_column Optional column with "image" / "video" values.
#' @param kind_column Optional column with "annotation_label" / "image_label".
#'   If missing it is inferred from the ID column name.
#' @param dry_run If TRUE, do not delete anything.
#' @param confirm_required Require a typed confirmation "DELETE".
#'
#' @return data.frame with result per row
#' @export

delete_biigle_label_list <- function(input,
                                     id_column = NULL,
                                     type_column = NULL,
                                     kind_column = NULL,
                                     dry_run = FALSE,
                                     confirm_required = TRUE,
                                     base_url = "https://biigle.de/api/v1",
                                     login = BiigleDElogin,
                                     token = BiigleDEtoken) {
  
  # 1) Load input
  if (is.character(input) && length(input) == 1L && file.exists(input)) {
    df <- read_csv(input, show_col_types = FALSE)
  } else if (is.data.frame(input)) {
    df <- input
  } else {
    stop("input must be a data.frame or a CSV file path.")
  }
  
  # 2) Detect ID column
  if (is.null(id_column)) {
    candidates <- c("annotation_label_id", "annotations_label_id", "image_label_id")
    found <- intersect(candidates, names(df))
    if (length(found) == 0) {
      stop("No ID column found. Expected one of: ",
           paste(candidates, collapse = ", "))
    }
    id_column <- found[1]
  }
  
  if (!id_column %in% names(df)) {
    stop("ID column '", id_column, "' not found.")
  }
  
  # 3) Optional columns
  if (is.null(type_column) && "type" %in% names(df)) type_column <- "type"
  if (is.null(kind_column) && "kind" %in% names(df)) kind_column <- "kind"
  
  # 4) Build working fields (no leading underscores)
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
  
  if (nrow(df) == 0) stop("No valid IDs to delete.")
  
  # Normalize type/kind
  df$item_type[!df$item_type %in% c("image", "video")] <- "image"
  inferred_kind <- if (grepl("annotation", id_column, fixed = TRUE)) "annotation_label" else "image_label"
  df$item_kind[!df$item_kind %in% c("annotation_label", "image_label")] <- inferred_kind
  
  # 6) Confirmation prompts
  cat("\nYou are about to delete", nrow(df), "Biigle label assignment(s).\n")
  cat("First IDs:", paste(head(df$id_value, 10), collapse = ", "),
      if (nrow(df) > 10) "..." else "", "\n")
  cat("Types:", paste(unique(df$item_type), collapse = ", "), "\n")
  cat("Kinds:", paste(unique(df$item_kind), collapse = ", "), "\n\n")
  
  if (confirm_required) {
    ans1 <- readline("Type DELETE to confirm: ")
    if (ans1 != "DELETE") {
      cat("Cancelled. Nothing deleted.\n")
      return(invisible(NULL))
    }
    ans2 <- readline("Final confirmation, type YES: ")
    if (toupper(ans2) != "YES") {
      cat("Cancelled. Nothing deleted.\n")
      return(invisible(NULL))
    }
  }
  
  if (dry_run) {
    cat("Dry run enabled. No requests sent.\n")
    return(invisible(df[, c(id_column, "item_type", "item_kind"), drop = FALSE]))
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
    id_i <- suppressWarnings(as.numeric(df$id_value[i]))
    type_i <- df$item_type[i]
    kind_i <- df$item_kind[i]
    
    res <- tryCatch(
      delete_one_biigle_label(
        id = id_i, type = type_i, kind = kind_i,
        base_url = base_url, login = login, token = token, verbose = FALSE
      ),
      error = function(e) list(status = NA_integer_, ok = FALSE, text = conditionMessage(e))
    )
    
    out <- rbind(
      out,
      data.frame(
        id = df$id_value[i],
        type = type_i,
        kind = kind_i,
        status = ifelse(is.null(res$status), NA_integer_, res$status),
        ok = isTRUE(res$ok),
        message = ifelse(is.null(res$text), "", res$text),
        stringsAsFactors = FALSE
      )
    )
    
    message(sprintf("[%d/%d] %s (%s, %s) -> %s",
                    i, nrow(df), df$id_value[i], type_i, kind_i,
                    if (isTRUE(res$ok)) "DELETED" else paste("FAILED", res$status)))
  }
  
  out
}

# Example usage:
# delete_biigle_label_list("labels_to_delete.csv")
#
# Or with a data.frame:
# df <- data.frame(
#   annotation_label_id = c(12345, 12346, 12347),
#   type = c("image", "image", "video")
# )
# delete_biigle_label_list(df)
#
# If you have a direct image_label_id column instead:
# df2 <- data.frame(
#   image_label_id = c(111, 222, 333),
#   type = c("image", "image", "image")
# )
# delete_biigle_label_list(df2)

# a second MAREANO example; how to delete the "no observation" label that were mistakenly assigned to images that have several "NGU Substrate" labels.
# find images that habve several "NGU Substrate" labels and then delete the "No observation" label from those images
# df_labels <- get_biigle_report(VOLUMEID, "image_label") # function to load first
# df_labels %>% 
#   filter(str_detect(label_hierarchy,"NGU Substrate"))  %>% 
#   count(image_id) %>% filter(n > 1) %>% pull(image_id)  -> ids
# # those are the labels you want to delete
# df_labels %>% filter(image_id %in% ids) %>% 
#   filter(str_detect(label_hierarchy,"NGU Substrate")) %>%  
#   filter(label_name == "No observation") -> d
# 
# delete_biigle_label_list( d )




