test_that("echo() works", {
  # nolint next: brace_linter
  res <- utils::capture.output(echo({ print(1) }, level = 0))
  obj <- substr(res, 23, nchar(res))
  exp <- c("[EXP] print(1)", "[OUT] #> [1] 1")
  expect_identical(obj, exp)

  expect_error(
    expect_output(
      expect_message(
        echo(
          expr = {
            print(NULL)
            invisible(1)
            message("message")
            warning("warning")
            stop("error")
          },
          log = NULL
        )
      )
    )
  )

  expect_output(expect_error(
    echo(file = system.file("example-script.R", package = "echo"), log = NULL),
    "3"
  ))

  expect_error(
    echo({ 1 }, file = tempfile()), # nolint: brace_linter.
    "must be missing"
  )

  # progress still produces outputs
  exprs <- expression(print(1), print(2))
  expect_silent(echo(exprs = exprs, level = "NUL", progress = FALSE))
  expect_output(echo(exprs = exprs, level = "NUL", progress = TRUE))
})
