testthat::context("MeasureLengthPipe")

testthat::test_that("initialize",{

  propertyName <- "length"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  nchar_conf <- TRUE

  testthat::expect_silent(MeasureLengthPipe$new(propertyName,
                                                alwaysBeforeDeps,
                                                notAfterDeps,
                                                nchar_conf))
})

testthat::test_that("initialize propertyName type error",{

  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  nchar_conf <- TRUE

  testthat::expect_error(MeasureLengthPipe$new(propertyName,
                                               alwaysBeforeDeps,
                                               notAfterDeps,
                                               nchar_conf),
                         "[MeasureLengthPipe][initialize][Error] Checking the type of the 'propertyName' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "length"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  nchar_conf <- TRUE

  testthat::expect_error(MeasureLengthPipe$new(propertyName,
                                               alwaysBeforeDeps,
                                               notAfterDeps,
                                               nchar_conf),
                         "[MeasureLengthPipe][initialize][Error] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("initialize notAfterDeps type error",{

  propertyName <- "length"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  nchar_conf <- TRUE

  testthat::expect_error(MeasureLengthPipe$new(propertyName,
                                               alwaysBeforeDeps,
                                               notAfterDeps,
                                               nchar_conf),
                         "[MeasureLengthPipe][initialize][Error] Checking the type of the 'notAfterDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("initialize nchar_conf type error",{

  propertyName <- "length"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  nchar_conf <- NULL

  testthat::expect_error(MeasureLengthPipe$new(propertyName,
                                               alwaysBeforeDeps,
                                               notAfterDeps,
                                               nchar_conf),
                         "[MeasureLengthPipe][initialize][Error] Checking the type of the 'nchar_conf' variable: NULL",
                         fixed = TRUE)
})

if (Sys.info()[['sysname']] %in% "Windows") {
testthat::test_that("pipe",{
  testthat::skip_if_not_installed("readr")
  propertyName <- "length"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  nchar_conf <- TRUE

  pipe <- MeasureLengthPipe$new(propertyName,
                                alwaysBeforeDeps,
                                notAfterDeps,
                                nchar_conf)

  path <- file.path("testFiles",
                    "testMeasureLengthPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$obtainSource()
  instance <- pipe$pipe(instance)

  testthat::expect_equal(instance$getSpecificProperty("length"),
                         132)
})
}
testthat::test_that("pipe Bad compatibility between Pipes.",{
  testthat::skip_if_not_installed("readr")
  propertyName <- "length"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  nchar_conf <- TRUE

  pipe <- MeasureLengthPipe$new(propertyName,
                                alwaysBeforeDeps,
                                notAfterDeps,
                                nchar_conf)

  path <- file.path("testFiles",
                    "testMeasureLengthPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  testthat::expect_error(pipe$pipe(instance),
                         "[MeasureLengthPipe][pipe][Error] Bad compatibility between Pipes",
                         fixed = TRUE)

})

testthat::test_that("pipe instance type error",{

  propertyName <- "length"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  nchar_conf <- TRUE
  pipe <- MeasureLengthPipe$new(propertyName,
                                alwaysBeforeDeps,
                                notAfterDeps,
                                nchar_conf)

  instance <- NULL
  testthat::expect_error(pipe$pipe(instance),
                         "[MeasureLengthPipe][pipe][Error] Checking the type of the 'instance' variable: NULL",
                         fixed = TRUE)
})

if (Sys.info()[['sysname']] %in% "Windows") {
testthat::test_that("getLength nchar <- TRUE",{

  propertyName <- "length"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  nchar_conf <- TRUE
  pipe <- MeasureLengthPipe$new(propertyName,
                                alwaysBeforeDeps,
                                notAfterDeps,
                                nchar_conf)

  data <- "example"
  nchar_conf <- TRUE
  testthat::expect_equal(pipe$getLength(data,
                                        nchar_conf),
                         7)
})
}
# if (Sys.info()[['sysname']] %in% "Windows") {
# testthat::test_that("getLength nchar <- FALSE",{
#
#   propertyName <- "length"
#   alwaysBeforeDeps <- list()
#   notAfterDeps <- list()
#   pipe <- MeasureLengthPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)
#
#   data <- "example"
#   nchar_conf <- FALSE
#
#   testthat::expect_equal(pipe$getLength(data, nchar_conf), 112)
#
# })
# }

testthat::test_that("getLength data type error",{

  propertyName <- "length"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  nchar_conf <- TRUE
  pipe <- MeasureLengthPipe$new(propertyName,
                                alwaysBeforeDeps,
                                notAfterDeps,
                                nchar_conf)

  data <- NULL
  nchar_conf <- TRUE
  testthat::expect_error(pipe$getLength(data,
                                        nchar_conf),
                         "[MeasureLengthPipe][getLength][Error] Checking the type of the 'data' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("getLength nchar_conf type error",{

  propertyName <- "length"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  nchar_conf <- TRUE
  pipe <- MeasureLengthPipe$new(propertyName,
                                alwaysBeforeDeps,
                                notAfterDeps,
                                nchar_conf)

  data <- "example"
  nchar_conf <- NULL
  testthat::expect_error(pipe$getLength(data,
                                        nchar_conf),
                         "[MeasureLengthPipe][getLength][Error] Checking the type of the 'nchar_conf' variable: NULL",
                         fixed = TRUE)
})
