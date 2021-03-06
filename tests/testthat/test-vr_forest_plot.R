test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

context("report_p")

testthat::test_that("errors", {
  testthat::expect_error(
    report_p(-1),
    "p cannot be less than 0"
  )

  testthat::expect_error(
    report_p(2),
    "p cannot be greater than 1"
  )

})
