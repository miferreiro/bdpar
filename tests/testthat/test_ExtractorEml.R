testthat::context("ExtractorEml")

testthat::setup(bdpar.Options$reset())

testthat::test_that("initialize",{

  bdpar.Options$set("eml.PartSelectedOnMPAlternative",
                    "text/plain")

  path <- "example.eml"
  PartSelectedOnMPAlternative <- NULL
  testthat::expect_silent(ExtractorEml$new(path,
                                           PartSelectedOnMPAlternative))
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("initialize path type error",{

  path <- NULL
  PartSelectedOnMPAlternative <- NULL
  testthat::expect_error(ExtractorEml$new(path,
                                          PartSelectedOnMPAlternative),
                         "[ExtractorEml][initialize][Error] Checking the type of the 'path' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("initialize PartSelectedOnMPAlternative type error",{

  path <- "example.eml"
  PartSelectedOnMPAlternative <- NULL

  bdpar.Options$set(key = "eml.PartSelectedOnMPAlternative",
                    value = NULL)

  testthat::expect_error(ExtractorEml$new(path,
                                          PartSelectedOnMPAlternative),
                         "[ExtractorEml][initialize][Error] Part of select on .eml files is neither defined in initialize or in bdpar.Options",
                         fixed = TRUE)

  PartSelectedOnMPAlternative <- 1

  testthat::expect_error(ExtractorEml$new(path,
                                          PartSelectedOnMPAlternative),
                         "[ExtractorEml][initialize][Error] Checking the type of the 'PartSelectedOnMPAlternative' variable: numeric",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
# test_that("obtainDate",{
#
# })
#
# test_that("obtainSource",{
#
# })
testthat::setup(bdpar.Options$reset())

testthat::test_that("getPartSelectedOnMPAlternative",{

  bdpar.Options$set("eml.PartSelectedOnMPAlternative",
                    "text/plain")

  path <- "example.tsms"
  PartSelectedOnMPAlternative <- NULL
  instance <- ExtractorEml$new(path,
                               PartSelectedOnMPAlternative)

  testthat::expect_equal(instance$getPartSelectedOnMPAlternative(),
                         "text/plain")
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("setPartSelectedOnMPAlternative",{

  bdpar.Options$set("eml.PartSelectedOnMPAlternative",
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

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("setPartSelectedOnMPAlternative PartSelectedOnMPAlternative input error",{

  bdpar.Options$set("eml.PartSelectedOnMPAlternative",
                    "text/plain")

  path <- "example.eml"
  PartSelectedOnMPAlternative <- NULL
  instance <- ExtractorEml$new(path,
                               PartSelectedOnMPAlternative)

  partSelected <- NULL

  testthat::expect_error(instance$setPartSelectedOnMPAlternative(partSelected),
                         "[ExtractorEml][setPartSelectedOnMPAlternative][Error] Checking the type of the 'PartSelectedOnMPAlternative' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
