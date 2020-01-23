context("TeeCSVPipe")

test_that("initialize",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  withData <- TRUE
  withSource <- TRUE

  expect_silent(TeeCSVPipe$new(propertyName,alwaysBeforeDeps,notAfterDeps,withData,withSource))
})

test_that("initialize propertyName type error",{

  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  withData <- TRUE
  withSource <- TRUE

  expect_error(TeeCSVPipe$new(propertyName,alwaysBeforeDeps,notAfterDeps,withData,withSource),"\\[TeeCSVPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- ""
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  withData <- TRUE
  withSource <- TRUE

  expect_error(TeeCSVPipe$new(propertyName,alwaysBeforeDeps,notAfterDeps,withData,withSource),"\\[TeeCSVPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  withData <- TRUE
  withSource <- TRUE

  expect_error(TeeCSVPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps,withData,withSource),"\\[TeeCSVPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("initialize withData type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  withData <- NULL
  withSource <- TRUE

  expect_error(TeeCSVPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps,withData,withSource),"\\[TeeCSVPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: withData NULL")

})

test_that("initialize withSource type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  withData <- TRUE
  withSource <- NULL

  expect_error(TeeCSVPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps,withData,withSource),"\\[TeeCSVPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: withSource NULL")

})

test_that("pipe",{
  skip_if_not_installed("readr")
  skip_if_not_installed("rjson")
  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  withData <- TRUE
  withSource <- TRUE

  pipe <- TeeCSVPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, withData, withSource)

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testTeeCSVPipe",
                                              "configurations.ini"))

  path <- file.path("testFiles",
                    "testTeeCSVPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)

  expect_equal(pipe$pipe(instance), instance)
  expect_equal(file.exists("output_tsms.csv"), TRUE)
  file.remove("output_tsms.csv")
})

test_that("pipe instance invalid",{
  skip_if_not_installed("readr")
  skip_if_not_installed("rjson")
  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  withData <- TRUE
  withSource <- TRUE

  pipe <- TeeCSVPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, withData, withSource)

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testTeeCSVPipe",
                                              "configurations.ini"))

  path <- file.path("testFiles",
                    "testTeeCSVPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)

  instance$invalidate()
  expect_equal(pipe$pipe(instance), instance)
  expect_equal(file.exists("output_tsms.csv"), FALSE)

})

test_that("pipe Bad compatibility between Pipes.",{
  skip_if_not_installed("readr")
  skip_if_not_installed("rjson")
  propertyName <- ""
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  withData <- TRUE
  withSource <- TRUE

  pipe <- TeeCSVPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, withData, withSource)

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testTeeCSVPipe",
                                              "configurations.ini"))

  path <- file.path("testFiles",
                    "testTeeCSVPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  expect_error(pipe$pipe(instance),"\\[TeeCSVPipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{
  skip_if_not_installed("rjson")
  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  withData <- TRUE
  withSource <- TRUE

  pipe <- TeeCSVPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, withData, withSource)

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testTeeCSVPipe",
                                              "configurations.ini"))

  instance <- NULL
  expect_error(pipe$pipe(instance),"\\[TeeCSVPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("configurationFilePath error",{
  skip_if_not_installed("rjson")
  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  withData <- TRUE
  withSource <- TRUE

  pipe <- TeeCSVPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, withData, withSource)

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testTeeCSVPipe",
                                              "configurations_error.ini"))

  path <- file.path("testFiles",
                    "testTeeCSVPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  expect_error(pipe$pipe(instance),"\\[TeeCSVPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: outPutPath NULL")

})

test_that("configurationFilePath error",{
  skip_if_not_installed("rjson")
  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  withData <- TRUE
  withSource <- TRUE

  pipe <- TeeCSVPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, withData, withSource)

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testTeeCSVPipe",
                                              "configurations_error_ext.ini"))

  path <- file.path("testFiles",
                    "testTeeCSVPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  expect_error(pipe$pipe(instance),"\\[TeeCSVPipe\\]\\[pipe\\]\\[Error\\]
                Checking the extension of the file: outPutPath bad")

})
