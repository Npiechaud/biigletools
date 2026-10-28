# Reusable helper to request, download and read a Biigle volume report
# Requires credentials in .Rprofile: BiigleDElogin, BiigleDEtoken

library(httr)
library(jsonlite)
library(readr)

# Report type IDs. Only the two from your original script are listed;
# check https://biigle.de/doc/api/index.html (volume reports) and add others.
biigle_report_types <- c(
  csv         = 3,  # CSV report (per original script)
  image_label = 7   # image label report (per original script)
)

#' Get a Biigle report for a volume into R
#'
#' @param volume_id  Biigle volume ID
#' @param type       Report type: numeric ID, or a name in `biigle_report_types`
#' @param read       If TRUE (default) read CSV(s) into data frame(s);
#'                   if FALSE return the path(s) of the extracted files
#' @param all_csv    If TRUE return a named list of all CSVs in the zip;
#'                   if FALSE (default) only the first CSV
#' @param max_wait   Max seconds to wait for the report to be generated
#' @param interval   Seconds between download attempts
#' @param base_url   Biigle API base URL
#' @param login,token Credentials (default from .Rprofile)
#' @param verbose    Print progress messages
#'
#' @return A data frame (or named list of data frames / file paths)
#' @export
get_biigle_report <- function(volume_id,
                              type,
                              read      = TRUE,
                              all_csv   = FALSE,
                              max_wait  = 300,
                              interval  = 2,
                              base_url  = API_base_url,
                              login     = BiigleDElogin,
                              token     = BiigleDEtoken,
                              verbose   = TRUE) {

  if (missing(volume_id) || missing(type)) {
    stop("volume_id and type are required.")
  }

  # resolve report type name -> id
  if (is.character(type)) {
    if (!type %in% names(biigle_report_types)) {
      stop("Unknown report type '", type, "'. Known: ",
           paste(names(biigle_report_types), collapse = ", "),
           " (or pass a numeric type).")
    }
    type <- biigle_report_types[[type]]
  }

  auth <- authenticate(login, token)

  # 1. request the report
  rp <- POST(paste0(base_url, "/volumes/", volume_id, "/reports"),
             config = auth,
             body = list(type = type),
             encode = "json")
  stop_for_status(rp)
  report_info <- fromJSON(content(rp, as = "text", encoding = "UTF-8"),
                          flatten = TRUE)
  url_report <- paste0(base_url, "/reports/", report_info$id)
  if (verbose) message("Report requested (id ", report_info$id, "). Waiting...")

  # 2. poll until the report is ready
  tmp_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp_zip), add = TRUE)

  waited <- 0
  repeat {
    response <- GET(url_report, auth, write_disk(tmp_zip, overwrite = TRUE))
    if (status_code(response) == 200) break
    if (waited >= max_wait) {
      stop("Report not ready after ", max_wait, "s (last status ",
           status_code(response), ").")
    }
    if (verbose) message("Report not ready yet, retrying...")
    Sys.sleep(interval)
    waited <- waited + interval
  }

  # 3. extract CSV(s)
  tmp_dir <- tempfile("biigle_report_")
  dir.create(tmp_dir)
  zip_contents <- unzip(tmp_zip, list = TRUE)
  csv_files <- zip_contents$Name[grepl("\\.csv$", zip_contents$Name,
                                       ignore.case = TRUE)]
  if (length(csv_files) == 0) stop("No CSV file found in the zip archive.")

  if (!all_csv) {
    if (length(csv_files) > 1 && verbose) {
      message("Multiple CSVs found; loading the first: ", csv_files[1])
    }
    csv_files <- csv_files[1]
  }

  unzip(tmp_zip, files = csv_files, exdir = tmp_dir, overwrite = TRUE)
  paths <- file.path(tmp_dir, csv_files)
  names(paths) <- csv_files

  if (!read) {
    return(if (all_csv) as.list(paths) else paths[[1]])
  }

  dfs <- lapply(paths, readr::read_csv, show_col_types = FALSE)
  if (all_csv) dfs else dfs[[1]]
}