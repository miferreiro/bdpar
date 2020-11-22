testthat::context("TeeCSVPipe")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  withData <- TRUE
  withSource <- TRUE
  outputPath <- NULL

  path <- file.path("testFiles",
                    "testTeeCSVPipe",
                    "output_tsms.csv")

  bdpar.Options$set(key = "teeCSVPipe.output.path",
                    value = path)

  testthat::expect_silent(TeeCSVPipe$new(propertyName,
                                         alwaysBeforeDeps,
                                         notAfterDeps,
                                         withData,
                                         withSource,
                                         outputPath))
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
  withData <- TRUE
  withSource <- TRUE
  outputPath <- NULL

  testthat::expect_error(TeeCSVPipe$new(propertyName,
                                        alwaysBeforeDeps,
                                        notAfterDeps,
                                        withData,
                                        withSource,
                                        outputPath),
                         "[TeeCSVPipe][initialize][FATAL] Checking the type of the 'propertyName' variable: NULL",
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

  propertyName <- ""
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  withData <- TRUE
  withSource <- TRUE
  outputPath <- NULL

  testthat::expect_error(TeeCSVPipe$new(propertyName,
                                        alwaysBeforeDeps,
                                        notAfterDeps,
                                        withData,
                                        withSource,
                                        outputPath),
                         "[TeeCSVPipe][initialize][FATAL] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
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

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  withData <- TRUE
  withSource <- TRUE
  outputPath <- NULL

  testthat::expect_error(TeeCSVPipe$new(propertyName,
                                        alwaysBeforeDeps,
                                        notAfterDeps,
                                        withData,
                                        withSource,
                                        outputPath),
                         "[TeeCSVPipe][initialize][FATAL] Checking the type of the 'notAfterDeps' variable: NULL",
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

testthat::test_that("initialize withData type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  withData <- NULL
  withSource <- TRUE
  outputPath <- NULL

  testthat::expect_error(TeeCSVPipe$new(propertyName,
                                        alwaysBeforeDeps,
                                        notAfterDeps,
                                        withData,
                                        withSource,
                                        outputPath),
                         "[TeeCSVPipe][initialize][FATAL] Checking the type of the 'withData' variable: NULL",
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

testthat::test_that("initialize withSource type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  withData <- TRUE
  withSource <- NULL
  outputPath <- NULL

  testthat::expect_error(TeeCSVPipe$new(propertyName,
                                        alwaysBeforeDeps,
                                        notAfterDeps,
                                        withData,
                                        withSource,
                                        outputPath),
                         "[TeeCSVPipe][initialize][FATAL] Checking the type of the 'withSource' variable: NULL",
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

testthat::test_that("initialize resourcesAbbreviationsPath type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  withData <- TRUE
  withSource <- TRUE
  outputPath <- NULL

  bdpar.Options$set(key = "teeCSVPipe.output.path",
                    value = NULL)

  testthat::expect_error(TeeCSVPipe$new(propertyName,
                                        alwaysBeforeDeps,
                                        notAfterDeps,
                                        withData,
                                        withSource,
                                        outputPath),
                         "[TeeCSVPipe][initialize][FATAL] Path of TeeCSVPipe output is neither defined in initialize or in bdpar.Options",
                         fixed = TRUE)

  outputPath <- 1

  testthat::expect_error(TeeCSVPipe$new(propertyName,
                                        alwaysBeforeDeps,
                                        notAfterDeps,
                                        withData,
                                        withSource,
                                        outputPath),
                         "[TeeCSVPipe][initialize][FATAL] Checking the type of the 'outputPath' variable: numeric",
                         fixed = TRUE)

  outputPath <- "example.json"

  testthat::expect_error(TeeCSVPipe$new(propertyName,
                                        alwaysBeforeDeps,
                                        notAfterDeps,
                                        withData,
                                        withSource,
                                        outputPath),
                         "[TeeCSVPipe][initialize][FATAL] Checking the extension of the file: json",
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
  testthat::skip_if_not_installed("rjson")
  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  withData <- TRUE
  withSource <- TRUE
  outputPath <- NULL

  pathOutput <- file.path("testFiles",
                          "testTeeCSVPipe",
                          "output_tsms.csv")

  bdpar.Options$set(key = "teeCSVPipe.output.path",
                    value = pathOutput)

  pipe <- TeeCSVPipe$new(propertyName,
                         alwaysBeforeDeps,
                         notAfterDeps,
                         withData,
                         withSource,
                         outputPath)

  Bdpar$new()

  path <- file.path("testFiles",
                    "testTeeCSVPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)

  testthat::expect_equal(pipe$pipe(instance),
                         instance)
  testthat::expect_equal(file.exists(pathOutput),
                         TRUE)
  #Checks pipe if the file is already exists
  testthat::expect_equal(pipe$pipe(instance),
                         instance)

  testthat::expect_equal(file.exists(pathOutput),
                         TRUE)

  file.remove(file.path("testFiles",
                        "testTeeCSVPipe",
                        "output_tsms.csv"))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
  if (file.exists(file.path("testFiles",
                            "testTeeCSVPipe",
                            "output_tsms.csv"))) {
    file.remove(file.path("testFiles",
                          "testTeeCSVPipe",
                          "output_tsms.csv"))
  }
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("pipe instance invalid",{
  testthat::skip_if_not_installed("rjson")
  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  withData <- TRUE
  withSource <- TRUE
  outputPath <- NULL

  pathOutput <- file.path("testFiles",
                          "testTeeCSVPipe",
                          "output_tsms.csv")

  bdpar.Options$set(key = "teeCSVPipe.output.path",
                    value = pathOutput)

  pipe <- TeeCSVPipe$new(propertyName,
                         alwaysBeforeDeps,
                         notAfterDeps,
                         withData,
                         withSource,
                         outputPath)

  Bdpar$new()

  path <- file.path("testFiles",
                    "testTeeCSVPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)

  instance$invalidate()
  testthat::expect_equal(pipe$pipe(instance),
                         instance)
  testthat::expect_equal(file.exists(pathOutput),
                         FALSE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
  if (file.exists(file.path("testFiles",
                            "testTeeCSVPipe",
                            "output_tsms.csv"))) {
    file.remove(file.path("testFiles",
                          "testTeeCSVPipe",
                          "output_tsms.csv"))
  }
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("pipe instance type error",{
  testthat::skip_if_not_installed("rjson")
  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  withData <- TRUE
  withSource <- TRUE
  outputPath <- NULL

  pathOutput <- file.path("testFiles",
                          "testTeeCSVPipe",
                          "output_tsms.csv")

  bdpar.Options$set(key = "teeCSVPipe.output.path",
                    value = pathOutput)

  pipe <- TeeCSVPipe$new(propertyName,
                         alwaysBeforeDeps,
                         notAfterDeps,
                         withData,
                         withSource,
                         outputPath)

  Bdpar$new()

  instance <- NULL
  testthat::expect_error(pipe$pipe(instance),
                         "[TeeCSVPipe][pipe][FATAL] Checking the type of the 'instance' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
