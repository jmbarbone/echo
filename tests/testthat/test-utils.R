test_that("echo_get_level() works", {
  expect_error(echo_get_level(5))
})

test_that("sys_get_env() works", {
  e <- Sys.getenv("FOO")
  Sys.setenv(FOO = "bar")
  expect_identical(sys_get_env("FOO"), "bar")
  Sys.setenv(FOO = e)
})
