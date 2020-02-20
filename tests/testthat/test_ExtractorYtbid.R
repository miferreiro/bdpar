testthat::context("ExtractorYtbid")

testthat::test_that("initialize path type error",{
  testthat::skip_if_not_installed("tuber")
  testthat::skip_if_not_installed("rjson")
  path <- NULL

  testthat::expect_error(ExtractorYtbid$new(path),
                         "[ExtractorYtbid][initialize][Error] Checking the type of the 'path' variable: NULL",
                         fixed = TRUE)
})
