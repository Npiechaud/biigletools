# Report type IDs for Biigle volume reports.
# Extend this vector as needed from:
# https://biigle.de/doc/api/index.html
biigle_report_types <- c(
  csv         = 3,
  image_label = 7
)

# Internal helper: get credentials from args or environment.
# Environment variables:
#   BIIGLE_LOGIN
#   BIIGLE_TOKEN
biigle_get_credentials <- function(login = NULL, token = NULL) {
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

#' Get a Biigle volume report into R
#'
#' @param volume_id Biigle volume ID (numeric/integer-like).
#' @param type Report type:
#'   - numeric report type ID, or
#'   - character key in `biigle_report_types` (e.g. `"csv"`, `"image_label"`).
#' @param read If `TRUE` (default), read CSV file(s) into data.frame(s).
#'   If `FALSE`, return extracted file path(s).
#' @param all_csv If `TRUE`, return all CSV files from the archive (named list).
#'   If `FALSE` (default), only the first CSV is returned.
#' @param max_wait Maximum seconds to wait for report generation.
#' @param interval Seconds between polling attempts.
#' @param base_url Biigle API base URL. Default: `"https://biigle.de/api/v1"`.
#' @param login Biigle login (email). If `NULL`, uses `BIIGLE_LOGIN`.
#' @param token Biigle API token. If `NULL`, uses `BIIGLE_TOKEN`.
#' @param verbose If `TRUE`, print progress messages.
#'
#' @return
#' - If `read = TRUE` and `all_csv = FALSE`: a data.frame
#' - If `read = TRUE` and `all_csv = TRUE`: named list of data.frames
#' - If `read = FALSE` and `all_csv = FALSE`: a single file path (character)
#' - If `read = FALSE` and `all_csv = TRUE`: named list of file paths
#'
#' @export
get_biigle_report <- function(volume_id,
                              type,
                              read = TRUE,
                              all_csv = FALSE,
                              max_wait = 300,
                              interval = 2,
                              base_url = "https://biigle.de/api/v1",
                              login = NULL,
                              token = NULL,
                              verbose = TRUE) {
  # ---- validate required arguments
  if (missing(volume_id) || is.null(volume_id) || !nzchar(as.character(volume_id))) {
    stop("`volume_id` is required.", call. = FALSE)
  }
  if (missing(type) || is.null(type) || !nzchar(as.character(type))) {
    stop("`type` is required.", call. = FALSE)
  }
  if (!is.numeric(max_wait) || length(max_wait) != 1 || max_wait < 0) {
    stop("`max_wait` must be a single non-negative number.", call. = FALSE)
  }
  if (!is.numeric(interval) || length(interval) != 1 || interval <= 0) {
    stop("`interval` must be a single positive number.", call. = FALSE)
  }

  # ---- resolve credentials
  creds <- biigle_get_credentials(login = login, token = token)
  auth <- httr::authenticate(creds$login, creds$token, type = "basic")

  # ---- resolve report type name -> id
  if (is.character(type)) {
    type <- trimws(type)
    if (!type %in% names(biigle_report_types)) {
      stop(
        "Unknown report type '", type, "'. Known: ",
        paste(names(biigle_report_types), collapse = ", "),
        " (or pass a numeric type ID).",
        call. = FALSE
      )
    }
    type <- unname(biigle_report_types[[type]])
  }

  # ---- request report generation
  request_url <- paste0(base_url, "/volumes/", volume_id, "/reports")
  rp <- httr::POST(
    url = request_url,
    auth,
    httr::accept_json(),
    body = list(type = type),
    encode = "json"
  )
  httr::stop_for_status(rp)

  report_info <- jsonlite::fromJSON(
    httr::content(rp, as = "text", encoding = "UTF-8"),
    simplifyDataFrame = TRUE
  )

  if (is.null(report_info$id)) {
    stop("API response did not contain a report ID.", call. = FALSE)
  }

  report_url <- paste0(base_url, "/reports/", report_info$id)
  if (isTRUE(verbose)) {
    message("Report requested (id ", report_info$id, "). Polling until ready...")
  }

  # ---- poll report endpoint until ready (HTTP 200 with zip)
  tmp_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp_zip), add = TRUE)

  waited <- 0
  repeat {
    response <- httr::GET(
      url = report_url,
      auth,
      httr::write_disk(tmp_zip, overwrite = TRUE)
    )

    sc <- httr::status_code(response)
    if (sc == 200) break

    if (waited >= max_wait) {
      stop(
        "Report not ready after ", max_wait,
        " seconds (last HTTP status: ", sc, ").",
        call. = FALSE
      )
    }

    if (isTRUE(verbose)) {
      message("Report not ready yet (status ", sc, "). Retrying in ", interval, "s...")
    }

    Sys.sleep(interval)
    waited <- waited + interval
  }

  # ---- extract csv(s)
  tmp_dir <- tempfile("biigle_report_")
  dir.create(tmp_dir)
  # keep extracted dir only for this call unless user asks for paths
  if (isTRUE(read)) on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  zip_contents <- utils::unzip(tmp_zip, list = TRUE)
  csv_files <- zip_contents$Name[grepl("\\.csv$", zip_contents$Name, ignore.case = TRUE)]

  if (length(csv_files) == 0) {
    stop("No CSV files found in the downloaded report archive.", call. = FALSE)
  }

  if (!isTRUE(all_csv)) {
    if (length(csv_files) > 1 && isTRUE(verbose)) {
      message("Multiple CSV files found; returning the first: ", csv_files[1])
    }
    csv_files <- csv_files[1]
  }

  utils::unzip(tmp_zip, files = csv_files, exdir = tmp_dir, overwrite = TRUE)
  paths <- file.path(tmp_dir, csv_files)
  names(paths) <- csv_files

  if (!isTRUE(read)) {
    return(if (isTRUE(all_csv)) as.list(paths) else unname(paths[[1]]))
  }

  dfs <- lapply(paths, function(p) readr::read_csv(p, show_col_types = FALSE))
  if (isTRUE(all_csv)) dfs else dfs[[1]]
}
