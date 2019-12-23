context("MeasureLengthPipe")

test_that("initialize",{

  propertyName <- "length"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_silent(MeasureLengthPipe$new(propertyName,alwaysBeforeDeps,notAfterDeps))
})

test_that("initialize propertyName type error",{

  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(MeasureLengthPipe$new(propertyName,alwaysBeforeDeps,notAfterDeps),"\\[MeasureLengthPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "length"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  expect_error(MeasureLengthPipe$new(propertyName,alwaysBeforeDeps,notAfterDeps),"\\[MeasureLengthPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{

  propertyName <- "length"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  expect_error(MeasureLengthPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps),"\\[MeasureLengthPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

if (Sys.info()[['sysname']] %in% "Windows") {
test_that("pipe",{
  skip_if_not_installed("readr")
  propertyName <- "length"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- MeasureLengthPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testMeasureLengthPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$obtainSource()
  propertyName <- "length"
  nchar_conf <- TRUE
  instance <- pipe$pipe(instance, propertyName, nchar_conf)

  expect_equal(instance$getSpecificProperty("length"), 132)

})
}
test_that("pipe Bad compatibility between Pipes.",{
  skip_if_not_installed("readr")
  propertyName <- "length"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  pipe <- MeasureLengthPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testMeasureLengthPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  propertyName <- "length"
  nchar_conf <- TRUE
  expect_error(pipe$pipe(instance, propertyName, nchar_conf),"\\[MeasureLengthPipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{

  propertyName <- "length"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- MeasureLengthPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  instance <- NULL
  propertyName <- "length"
  nchar_conf <- TRUE
  expect_error(pipe$pipe(instance, propertyName, nchar_conf),"\\[MeasureLengthPipe\\]\\[pipe\\]\\[Error\\]
                    Checking the type of the variable: instance NULL")

})

test_that("pipe propertyName type error",{
  skip_if_not_installed("readr")
  propertyName <- "length"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- MeasureLengthPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testMeasureLengthPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  propertyName <- NULL
  nchar_conf <- TRUE
  expect_error(pipe$pipe(instance, propertyName, nchar_conf),"\\[MeasureLengthPipe\\]\\[pipe\\]\\[Error\\]
                    Checking the type of the variable: propertyName NULL")

})

test_that("pipe nchar_conf type error",{
  skip_if_not_installed("readr")
  propertyName <- "length"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- MeasureLengthPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testMeasureLengthPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  propertyName <- "length"
  nchar_conf <- NULL
  expect_error(pipe$pipe(instance, propertyName, nchar_conf),"\\[MeasureLengthPipe\\]\\[pipe\\]\\[Error\\]
                    Checking the type of the variable: nchar_conf NULL")

})
if (Sys.info()[['sysname']] %in% "Windows") {
test_that("getLength nchar <- TRUE",{

  propertyName <- "length"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- MeasureLengthPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  data <- "example"
  nchar_conf <- TRUE
  expect_equal(pipe$getLength(data, nchar_conf),7)

})
}
# if (Sys.info()[['sysname']] %in% "Windows") {
# test_that("getLength nchar <- FALSE",{
#
#   propertyName <- "length"
#   alwaysBeforeDeps <- list()
#   notAfterDeps <- list()
#   pipe <- MeasureLengthPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)
#
#   data <- "example"
#   nchar_conf <- FALSE
#
#   expect_equal(pipe$getLength(data, nchar_conf), 112)
#
# })
# }

test_that("getLength data type error",{

  propertyName <- "length"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- MeasureLengthPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  data <- NULL
  nchar <- TRUE
  expect_error(pipe$getLength(data, nchar_conf),"\\[MeasureLengthPipe\\]\\[getLength\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})

test_that("getLength nchar_conf type error",{

  propertyName <- "length"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- MeasureLengthPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  data <- "example"
  nchar_conf <- NULL
  expect_error(pipe$getLength(data, nchar_conf),"\\[MeasureLengthPipe\\]\\[getLength\\]\\[Error\\]
                Checking the type of the variable: nchar_conf NULL")

})
