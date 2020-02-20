testthat::context("File2Pipe")

testthat::test_that("initialize",{

  propertyName <- "source"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  testthat::expect_silent(File2Pipe$new(propertyName,
                                        alwaysBeforeDeps,
                                        notAfterDeps))
})

testthat::test_that("initialize propertyName type error",{

  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  testthat::expect_error(File2Pipe$new(propertyName,
                                       alwaysBeforeDeps,
                                       notAfterDeps),
                         "[File2Pipe][initialize][Error] Checking the type of the 'propertyName' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "source"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  testthat::expect_error(File2Pipe$new(propertyName,
                                       alwaysBeforeDeps,
                                       notAfterDeps),
                         "[File2Pipe][initialize][Error] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("initialize notAfterDeps type error",{
  testthat::skip_if_not_installed("readr")
  propertyName <- "source"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  testthat::expect_error(File2Pipe$new(propertyName,
                                       alwaysBeforeDeps,
                                       notAfterDeps),
                         "[File2Pipe][initialize][Error] Checking the type of the 'notAfterDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("pipe",{
  testthat::skip_if_not_installed("readr")
  propertyName <- "source"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- File2Pipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testFile2Pipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instanceExpected <- ExtractorSms$new(path)
  instanceExpected$setSource("Wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us")
  instanceExpected$setData("Wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us")
  instanceFinal <- pipe$pipe(instance)
  testthat::expect_equal(instanceFinal$getSource(),
                         instanceExpected$getSource())
  testthat::expect_equal(instanceFinal$getData(),
                         instanceExpected$getData())
})

testthat::test_that("pipe empty source",{
  testthat::skip_if_not_installed("readr")
  propertyName <- "source"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- File2Pipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testFile2Pipe",
                    "example_File2Pipe_empty.tsms")

  instance <- ExtractorSms$new(path)
  testthat::expect_warning(pipe$pipe(instance),
                           "\\[File2Pipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFile2Pipe\\/example_File2Pipe_empty\\.tsms has source empty")
})

testthat::test_that("pipe instance type error",{

  propertyName <- "source"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- File2Pipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  instance <- NULL

  testthat::expect_error(pipe$pipe(instance),
                         "[File2Pipe][pipe][Error] Checking the type of the 'instance' variable: NULL",
                         fixed = TRUE)
})
