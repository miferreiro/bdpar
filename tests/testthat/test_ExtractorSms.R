testthat::context("ExtractorSms")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize",{
  testthat::skip_if_not_installed("readr")
  path <- "testFile.tsms"
  testthat::expect_silent(ExtractorSms$new(path))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize path type error",{
  testthat::skip_if_not_installed("readr")
  path <- NULL

  testthat::expect_error(ExtractorSms$new(path),
                         "[ExtractorSms][initialize][FATAL] Checking the type of the 'path' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("obtainDate",{
  testthat::skip_if_not_installed("readr")
  path <- file.path("testFiles",
                    "testExtractorSms",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$obtainDate()
  testthat::expect_equal(instance$getDate(),"")

})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("obtainSource",{
  testthat::skip_if_not_installed("readr")
  path <- file.path("testFiles",
                    "testExtractorSms",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$obtainSource()
  testthat::expect_equal(instance$.__enclos_env__$private$source,
                         "Wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us")
  testthat::expect_equal(instance$.__enclos_env__$private$data,
                         "Wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
