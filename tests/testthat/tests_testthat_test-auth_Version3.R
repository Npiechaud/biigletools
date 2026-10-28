test_that(".biigle_get_credentials() uses explicit arguments", {
  creds <- biigletools:::.biigle_get_credentials(login = "a@b.c", token = "tok")
  expect_equal(creds$login, "a@b.c")
  expect_equal(creds$token, "tok")
})

test_that(".biigle_get_credentials() falls back to environment variables", {
  withr::local_envvar(BIIGLE_LOGIN = "env@b.c", BIIGLE_TOKEN = "envtok")
  creds <- biigletools:::.biigle_get_credentials()
  expect_equal(creds$login, "env@b.c")
  expect_equal(creds$token, "envtok")
})

test_that(".biigle_get_credentials() errors when credentials are missing", {
  withr::local_envvar(BIIGLE_LOGIN = "", BIIGLE_TOKEN = "")
  expect_error(biigletools:::.biigle_get_credentials(), "Missing Biigle login")
  expect_error(
    biigletools:::.biigle_get_credentials(login = "a@b.c"),
    "Missing Biigle token"
  )
})