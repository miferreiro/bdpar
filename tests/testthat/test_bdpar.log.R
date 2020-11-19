testthat::context("bdpar.log")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("bdpar.log logger not configured",{

  message <- "exampleMessage"
  level <- "INFO"
  className <- "exampleClassName"
  methodName <- "exampleMethodName"

  bdpar.Options$disableLog()

  testthat::expect_silent(bdpar.log(message = message,
                                    level = level,
                                    className = className,
                                    methodName = methodName))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("bdpar.log logger NULL",{

  message <- "exampleMessage"
  level <- "INFO"
  className <- "exampleClassName"
  methodName <- "exampleMethodName"

  bdpar:::.setLoggerSettings(settings = NULL)

  testthat::expect_error(bdpar.log(message = message,
                                   level = level,
                                   className = className,
                                   methodName = methodName),
                         "[bdpar.log][FATAL] Logger is not configured. Use bdpar.options$configureLog to configure its behavior",
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

testthat::test_that("bdpar.log error level parameter",{

  message <- "exampleMessage"
  className <- "exampleClassName"
  methodName <- "exampleMethodName"

  level <- NULL

  testthat::expect_error(bdpar.log(message = message,
                                   level = level,
                                   className = className,
                                   methodName = methodName),
                         "[bdpar.log][FATAL] The 'level' variable must be between these values: FATAL, ERROR, WARN, INFO or DEBUG",
                         fixed = TRUE)

  level <- 1

  testthat::expect_error(bdpar.log(message = message,
                                   level = level,
                                   className = className,
                                   methodName = methodName),
                         "[bdpar.log][FATAL] The 'level' variable must be between these values: FATAL, ERROR, WARN, INFO or DEBUG",
                         fixed = TRUE)

  level <- "wrongLevel"

  testthat::expect_error(bdpar.log(message = message,
                                   level = level,
                                   className = className,
                                   methodName = methodName),
                         "[bdpar.log][FATAL] The 'level' variable must be between these values: FATAL, ERROR, WARN, INFO or DEBUG",
                         fixed = TRUE)

})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog(threshold = "DEBUG")
})

testthat::test_that("bdpar.log works",{

  message <- "exampleMessage"

  className <- "exampleClassName"
  methodName <- "exampleMethodName"

  level <- "DEBUG"

  testthat::expect_message(bdpar.log(message = message,
                                   level = level,
                                   className = className,
                                   methodName = methodName),
                         "[exampleClassName][exampleMethodName][DEBUG] exampleMessage",
                         fixed = TRUE)

  level <- "INFO"

  testthat::expect_message(bdpar.log(message = message,
                                     level = level,
                                     className = className,
                                     methodName = methodName),
                           "[exampleClassName][exampleMethodName][INFO] exampleMessage",
                           fixed = TRUE)

  level <- "WARN"

  testthat::expect_warning(bdpar.log(message = message,
                                     level = level,
                                     className = className,
                                     methodName = methodName),
                           "[exampleClassName][exampleMethodName][WARN] exampleMessage",
                           fixed = TRUE)

  level <- "ERROR"

  testthat::expect_message(bdpar.log(message = message,
                                     level = level,
                                     className = className,
                                     methodName = methodName),
                           "[exampleClassName][exampleMethodName][ERROR] exampleMessage",
                           fixed = TRUE)

  level <- "FATAL"

  testthat::expect_error(bdpar.log(message = message,
                                   level = level,
                                   className = className,
                                   methodName = methodName),
                         "[exampleClassName][exampleMethodName][FATAL] exampleMessage",
                         fixed = TRUE)

})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
