test_that("echo works", {
  res <- utils::capture.output(echo({ print(1) }, level = 0))
  expect_identical(
    substr(res, 23, nchar(res)),
    c("[EXP] print(1)", "[OUT] #> [1] 1")
  )
})
