testthat::context("GuessDatePipe")

testthat::test_that("initialize",{

  propertyName <- "date"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  testthat::expect_silent(GuessDatePipe$new(propertyName,
                                            alwaysBeforeDeps,
                                            notAfterDeps))
})

testthat::test_that("initialize propertyName type error",{

  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  testthat::expect_error(GuessDatePipe$new(propertyName,
                                           alwaysBeforeDeps,
                                           notAfterDeps),
                         "[GuessDatePipe][initialize][Error] Checking the type of the 'propertyName' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "date"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  testthat::expect_error(GuessDatePipe$new(propertyName,
                                           alwaysBeforeDeps,
                                           notAfterDeps),
                         "[GuessDatePipe][initialize][Error] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("initialize notAfterDeps type error",{

  propertyName <- "date"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  testthat::expect_error(GuessDatePipe$new(propertyName,
                                           alwaysBeforeDeps,
                                           notAfterDeps),
                         "[GuessDatePipe][initialize][Error] Checking the type of the 'notAfterDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("pipe",{
  testthat::skip_if_not_installed("readr")
  propertyName <- "date"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- GuessDatePipe$new(propertyName,
                            alwaysBeforeDeps,
                            notAfterDeps)

  path <- file.path("testFiles",
                    "testGuessDatePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instanceExpected <- ExtractorSms$new(path)
  instanceExpected$setDate("")
  testthat::expect_equal(pipe$pipe(instance),
                         instanceExpected)
})

testthat::test_that("pipe instance type error",{

  propertyName <- "date"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- GuessDatePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  instance <- NULL

  testthat::expect_error(pipe$pipe(instance),
                         "[GuessDatePipe][pipe][Error] Checking the type of the 'instance' variable: NULL",
                         fixed = TRUE)
})
