context("StoreFileExtPipe")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize",{

  propertyName <- "extension"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  testthat::expect_silent(StoreFileExtPipe$new(propertyName,
                                               alwaysBeforeDeps,
                                               notAfterDeps))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize propertyName type error",{

  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  testthat::expect_error(StoreFileExtPipe$new(propertyName,
                                              alwaysBeforeDeps,
                                              notAfterDeps),
                         "[StoreFileExtPipe][initialize][FATAL] Checking the type of the 'propertyName' variable: NULL",
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

testthat::test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "extension"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  testthat::expect_error(StoreFileExtPipe$new(propertyName,
                                              alwaysBeforeDeps,
                                              notAfterDeps),
                         "[StoreFileExtPipe][initialize][FATAL] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
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

testthat::test_that("initialize notAfterDeps type error",{

  propertyName <- "extension"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  testthat::expect_error(StoreFileExtPipe$new(propertyName,
                                              alwaysBeforeDeps,
                                              notAfterDeps),
                         "[StoreFileExtPipe][initialize][FATAL] Checking the type of the 'notAfterDeps' variable: NULL",
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

testthat::test_that("pipe",{
  skip_if_not_installed("readr")
  propertyName <- "extension"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- StoreFileExtPipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps)

  path <- file.path("testFiles",
                    "testStoreFileExtPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)

  testthat::expect_equal(pipe$pipe(instance)$getSpecificProperty("extension"),
                         "tsms")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("pipe instance type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- StoreFileExtPipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps)

  instance <- NULL
  testthat::expect_error(pipe$pipe(instance),
                         "[StoreFileExtPipe][pipe][FATAL] Checking the type of the 'instance' variable: NULL",
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

testthat::test_that("pipe",{
  skip_if_not_installed("readr")
  propertyName <- "extension"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- StoreFileExtPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- "example/exam"

  instance <- ExtractorSms$new(path)

  testthat::expect_warning(pipe$pipe(instance),
                           "[StoreFileExtPipe][pipe][WARN] The file: example/exam has not an extension",
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

testthat::test_that("obtainExtension",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- StoreFileExtPipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps)

  path <- "example/exam.exe"
  testthat::expect_equal(pipe$obtainExtension(path),
                         "exe")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("obtainExtension path type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- StoreFileExtPipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps)

  path <- NULL
  testthat::expect_error(pipe$obtainExtension(path),
                         "[StoreFileExtPipe][obtainExtension][FATAL] Checking the type of the 'path' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
