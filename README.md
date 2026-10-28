# biigletools

Utilities to interact with the [BIIGLE API](https://biigle.de/doc/api/index.html) from R.

This package is intended to provide practical helper functions for common BIIGLE workflows, including:

- requesting and loading BIIGLE volume reports
- checking label existence in project label trees
- deleting image/video label assignments (single or batch)

> Status: early development. Function names and interfaces may still evolve.

---

## Installation

### From GitHub

```r
# install.packages("pak")
pak::pak("Npiechaud/biigletools")
```

Alternative:

```r
# install.packages("remotes")
remotes::install_github("Npiechaud/biigletools")
```

### From a local clone

```r
# install.packages("devtools")
devtools::install("path/to/biigletools")
```

For development use (without reinstalling each change):

```r
devtools::load_all("path/to/biigletools")
```

---

## Requirements

- R >= 4.1 (recommended)
- BIIGLE account with API access
- Packages used by helpers:
  - `httr`
  - `jsonlite`
  - `readr`

---

## Credentials (BIIGLE_LOGIN and BIIGLE_TOKEN)

Most functions authenticate against BIIGLE API using HTTP Basic auth.

This package expects credentials via environment variables:

- `BIIGLE_LOGIN` = your BIIGLE login/email
- `BIIGLE_TOKEN` = your BIIGLE API token

### Recommended setup: `.Renviron`

Add credentials to your user-level `.Renviron` file:

```r
usethis::edit_r_environ()
```

Then add lines like:

```bash
BIIGLE_LOGIN=your.name@example.org
BIIGLE_TOKEN=your_token_here
```

Save, restart R, then verify:

```r
Sys.getenv("BIIGLE_LOGIN")
nzchar(Sys.getenv("BIIGLE_TOKEN"))
```

### Temporary setup (current R session only)

```r
Sys.setenv(
  BIIGLE_LOGIN = "your.name@example.org",
  BIIGLE_TOKEN = "your_token_here"
)
```

### Passing credentials directly (optional)

Most exported functions also allow explicit `login=` and `token=` arguments.  
This can be useful in scripts, CI, or multi-account workflows.

---

## Security notes

- **Never commit tokens** to GitHub.
- Do not place credentials in tracked files (`README`, scripts, `.R`, `.Rmd`, etc.).
- Consider adding `.Renviron` and other local secrets files to `.gitignore`.
- If a token is exposed, revoke/rotate it immediately in BIIGLE.

---

## Quick start

### 1) Load package

```r
library(biigletools)
```

### 2) Download an image label report from a volume

```r
df <- get_biigle_report(
  volume_id = 12345,
  type = "image_label"  # or numeric BIIGLE report type ID
)

head(df)
```

### 3) Check whether a label exists in a project

```r
res <- label_in_project(
  project_id = 678,
  label = "No observation"  # or numeric label ID
)

res$found
res$matches
```

### 4) Delete assignments from a data frame (careful!)

```r
to_delete <- data.frame(
  annotation_label_id = c(111, 222, 333),
  type = c("image", "image", "video")
)

out <- delete_biigle_label_list(
  input = to_delete,
  confirm_required = TRUE
)

out
```

---

## Main helper functions

- `get_biigle_report()`
- `label_in_project()`
- `delete_one_biigle_label()`
- `delete_biigle_label_list()`

---

## Development

Generate docs and run checks:

```r
devtools::document()
devtools::check()
```

Run tests (if present):

```r
devtools::test()
```

---

## API reference

BIIGLE API docs:  
https://biigle.de/doc/api/index.html

---

## Disclaimer

This package is an independent helper toolkit and is not an official BIIGLE client.
