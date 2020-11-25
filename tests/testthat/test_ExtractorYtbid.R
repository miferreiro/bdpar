testthat::context("ExtractorYtbid")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize path type error",{
  testthat::skip_if_not_installed("tuber")
  testthat::skip_if_not_installed("rjson")
  path <- NULL

  testthat::expect_error(ExtractorYtbid$new(path),
                         "[ExtractorYtbid][initialize][FATAL] Checking the type of the 'path' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
