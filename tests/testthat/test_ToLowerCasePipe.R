testthat::context("ToLowerCasePipe")

testthat::test_that("initialize",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  testthat::expect_silent(ToLowerCasePipe$new(propertyName,
                                              alwaysBeforeDeps,
                                              notAfterDeps))
})

testthat::test_that("initialize propertyName type error",{

  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  testthat::expect_error(ToLowerCasePipe$new(propertyName,
                                             alwaysBeforeDeps,
                                             notAfterDeps),
                         "[ToLowerCasePipe][initialize][Error] Checking the type of the 'propertyName' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- ""
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  testthat::expect_error(ToLowerCasePipe$new(propertyName,
                                             alwaysBeforeDeps,
                                             notAfterDeps),
                         "[ToLowerCasePipe][initialize][Error] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("initialize notAfterDeps type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  testthat::expect_error(ToLowerCasePipe$new(propertyName,
                                             alwaysBeforeDeps,
                                             notAfterDeps),
                         "[ToLowerCasePipe][initialize][Error] Checking the type of the 'notAfterDeps' variable: NULL",
                         fixed = TRUE)

})

testthat::test_that("pipe",{
  skip_if_not_installed("readr")
  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- ToLowerCasePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testToLowerCasePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("eXaMpLe")
  instance <- pipe$pipe(instance)
  testthat::expect_equal(instance$getData(),"example")

})

testthat::test_that("pipe Bad compatibility between Pipes.",{
  skip_if_not_installed("readr")
  propertyName <- ""
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  pipe <- ToLowerCasePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testToLowerCasePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  testthat::expect_error(pipe$pipe(instance),
                         "[ToLowerCasePipe][pipe][Error] Bad compatibility between Pipes",
                         fixed = TRUE)

})

testthat::test_that("pipe instance type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- ToLowerCasePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  instance <- NULL
  testthat::expect_error(pipe$pipe(instance),
                         "[ToLowerCasePipe][pipe][Error] Checking the type of the 'instance' variable: NULL",
                         fixed = TRUE)

})


testthat::test_that("toLowerCase",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- ToLowerCasePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  data <- "ExAmPlE"
  testthat::expect_equal(pipe$toLowerCase(data),"example")

})

testthat::test_that("toLowerCase data type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- ToLowerCasePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  data <- NULL
  testthat::expect_error(pipe$toLowerCase(data),
                         "[ToLowerCasePipe][toLowerCase][Error] Checking the type of the 'data' variable: NULL",
                         fixed = TRUE)

})
