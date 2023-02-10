test_that("echo() works", {
  res <- utils::capture.output(echo({ print(1) }, level = 0))
  expect_identical(
    substr(res, 23, nchar(res)),
    c("[EXP] print(1)", "[OUT] #> [1] 1")
  )

  expect_error(
    echo(
      exprs = {
        print(NULL)
        invisible(1)
        message("message")
        warning("warning")
        stop("error")
      },
      log = NULL
    )
  )

  expect_error(
    echo(file = system.file("example-script.R", package = "echo"), log = NULL),
    "3"
  )

  expect_error(
    echo({ 1 }, file = tempfile()),
    "must be missing"
  )
})
