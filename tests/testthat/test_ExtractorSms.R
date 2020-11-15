testthat::context("ExtractorSms")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize",{
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

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("toString basic works",{

  path <- "example.tsms"

  instance <- ExtractorSms$new(path)

  testthat::expect_equal(instance$toString(),
                         "\tPath: example.tsms\n\tDate: \n\tIsValid: TRUE\n\tSource: \"\"\n\tData: \"\"\n\tFlowPipes: \n\tBanPipes: \n\tProperties: Not located\n",
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

testthat::test_that("toString works with properties",{

  path <- "example.tsms"

  instance <- ExtractorSms$new(path)

  instance$addProperties("valueExample", "propertyExample")

  testthat::expect_equal(instance$toString(),
                         "\tPath: example.tsms\n\tDate: \n\tIsValid: TRUE\n\tSource: \"\"\n\tData: \"\"\n\tFlowPipes: \n\tBanPipes: \n\tProperties: \n\t\t- propertyExample: valueExample\n",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
