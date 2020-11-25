testthat::context("ExtractorEml")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize",{

  bdpar.Options$set("extractorEML.mpaPartSelected",
                    "text/plain")

  path <- "example.eml"
  PartSelectedOnMPAlternative <- NULL
  testthat::expect_silent(ExtractorEml$new(path,
                                           PartSelectedOnMPAlternative))
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
  PartSelectedOnMPAlternative <- NULL
  testthat::expect_error(ExtractorEml$new(path,
                                          PartSelectedOnMPAlternative),
                         "[ExtractorEml][initialize][FATAL] Checking the type of the 'path' variable: NULL",
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

testthat::test_that("initialize PartSelectedOnMPAlternative type error",{

  path <- "example.eml"
  PartSelectedOnMPAlternative <- NULL

  bdpar.Options$set(key = "extractorEML.mpaPartSelected",
                    value = NULL)

  testthat::expect_error(ExtractorEml$new(path,
                                          PartSelectedOnMPAlternative),
                         "[ExtractorEml][initialize][FATAL] Part of select on .eml files is neither defined in initialize or in bdpar.Options",
                         fixed = TRUE)

  PartSelectedOnMPAlternative <- 1

  testthat::expect_error(ExtractorEml$new(path,
                                          PartSelectedOnMPAlternative),
                         "[ExtractorEml][initialize][FATAL] Checking the type of the 'PartSelectedOnMPAlternative' variable: numeric",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
# test_that("obtainDate",{
#
# })
#
# test_that("obtainSource",{
#
# })
testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("getPartSelectedOnMPAlternative",{

  bdpar.Options$set("extractorEML.mpaPartSelected",
                    "text/plain")

  path <- "example.tsms"
  PartSelectedOnMPAlternative <- NULL
  instance <- ExtractorEml$new(path,
                               PartSelectedOnMPAlternative)

  testthat::expect_equal(instance$getPartSelectedOnMPAlternative(),
                         "text/plain")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("setPartSelectedOnMPAlternative",{

  bdpar.Options$set("extractorEML.mpaPartSelected",
                    "text/plain")

  path <- "example.tsms"
  PartSelectedOnMPAlternative <- NULL
  instance <- ExtractorEml$new(path,
                               PartSelectedOnMPAlternative)

  partSelectedExpect <- "text/html"

  instance$setPartSelectedOnMPAlternative(partSelectedExpect)

  testthat::expect_equal(instance$getPartSelectedOnMPAlternative(),
                         partSelectedExpect)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("setPartSelectedOnMPAlternative PartSelectedOnMPAlternative input error",{

  bdpar.Options$set("extractorEML.mpaPartSelected",
                    "text/plain")

  path <- "example.eml"
  PartSelectedOnMPAlternative <- NULL
  instance <- ExtractorEml$new(path,
                               PartSelectedOnMPAlternative)

  partSelected <- NULL

  testthat::expect_error(instance$setPartSelectedOnMPAlternative(partSelected),
                         "[ExtractorEml][setPartSelectedOnMPAlternative][FATAL] Checking the type of the 'PartSelectedOnMPAlternative' variable: NULL",
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

testthat::test_that("toString basic works",{

  path <- "example.eml"
  PartSelectedOnMPAlternative <- NULL

  instance <- ExtractorEml$new(path = path,
                               PartSelectedOnMPAlternative = PartSelectedOnMPAlternative)

  testthat::expect_equal(instance$toString(),
                         "\tPath: example.eml\n\tDate: \n\tIsValid: TRUE\n\tSource: \"\"\n\tData: \"\"\n\tFlowPipes: \n\tBanPipes: \n\tProperties: Not located\n",
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

  path <- "example.eml"
  PartSelectedOnMPAlternative <- NULL

  instance <- ExtractorEml$new(path = path,
                               PartSelectedOnMPAlternative = PartSelectedOnMPAlternative)

  instance$addProperties("valueExample", "propertyExample")

  testthat::expect_equal(instance$toString(),
                         "\tPath: example.eml\n\tDate: \n\tIsValid: TRUE\n\tSource: \"\"\n\tData: \"\"\n\tFlowPipes: \n\tBanPipes: \n\tProperties: \n\t\t- propertyExample: valueExample\n",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
