test_that("get_biigle_report() validates its arguments", {
  withr::local_envvar(BIIGLE_LOGIN = "a@b.c", BIIGLE_TOKEN = "tok")
  expect_error(get_biigle_report(type = "csv"), "volume_id")
  expect_error(get_biigle_report(volume_id = 1, type = "nope"), "Unknown report type")
})

test_that("label_in_project() requires its arguments", {
  expect_error(label_in_project(label = "x"), "project_id")
  expect_error(label_in_project(project_id = 1), "label")
})

test_that("delete_one_biigle_label() validates id", {
  expect_error(delete_one_biigle_label(), "id")
  expect_error(delete_one_biigle_label(id = "abc"), "numeric")
})

test_that("delete_biigle_label_list() rejects bad input and missing ID columns", {
  withr::local_envvar(BIIGLE_LOGIN = "a@b.c", BIIGLE_TOKEN = "tok")
  expect_error(delete_biigle_label_list(123), "data.frame")
  expect_error(
    delete_biigle_label_list(data.frame(x = 1), confirm_required = FALSE),
    "No ID column"
  )
})

test_that("delete_biigle_label_list() dry run returns a preview", {
  withr::local_envvar(BIIGLE_LOGIN = "a@b.c", BIIGLE_TOKEN = "tok")
  df <- data.frame(annotation_label_id = c(1, 2))
  out <- suppressMessages(
    delete_biigle_label_list(df, dry_run = TRUE, confirm_required = FALSE)
  )
  expect_equal(nrow(out), 2)
})