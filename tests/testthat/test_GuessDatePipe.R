context("GuessDatePipe")

test_that("initialize",{

  propertyName <- "date"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_silent(GuessDatePipe$new(propertyName,alwaysBeforeDeps,notAfterDeps))
})

test_that("initialize propertyName type error",{

  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(GuessDatePipe$new(propertyName,alwaysBeforeDeps,notAfterDeps),"\\[GuessDatePipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "date"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  expect_error(GuessDatePipe$new(propertyName,alwaysBeforeDeps,notAfterDeps),"\\[GuessDatePipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{

  propertyName <- "date"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  expect_error(GuessDatePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps),"\\[GuessDatePipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("pipe",{
  skip_if_not_installed("readr")
  propertyName <- "date"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- GuessDatePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testGuessDatePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instanceExpected <- ExtractorSms$new(path)
  instanceExpected$setDate("")
  instanceExpected$addFlowPipes("GuessDatePipe")
  expect_equal(pipe$pipe(instance),instanceExpected)

})

test_that("pipe Bad compatibility between Pipes.",{
  skip_if_not_installed("readr")
  propertyName <- "date"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  pipe <- GuessDatePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testGuessDatePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  expect_error(pipe$pipe(instance),"\\[GuessDatePipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{

  propertyName <- "date"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- GuessDatePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  instance <- NULL

  expect_error(pipe$pipe(instance),"\\[GuessDatePipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})


