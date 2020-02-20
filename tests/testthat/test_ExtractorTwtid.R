testthat::context("ExtractorTwtid")

testthat::test_that("initialize path type error",{
  testthat::skip_if_not_installed("rtweet")
  testthat::skip_if_not_installed("rjson")
  path <- NULL
  testthat::expect_error(ExtractorTwtid$new(path),
                         "[ExtractorTwtid][initialize][Error] Checking the type of the 'path' variable: NULL",
                         fixed = TRUE)
})
