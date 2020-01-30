testthat::context("PipeGeneric")

testthat::test_that("initialize",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)
  testthat::expect_equal(pipe$.__enclos_env__$private$propertyName,
                         propertyName)
  testthat::expect_equal(pipe$.__enclos_env__$private$alwaysBeforeDeps,
                         alwaysBeforeDeps)
  testthat::expect_equal(pipe$.__enclos_env__$private$notAfterDeps,
                         notAfterDeps)
})

testthat::test_that("initialize propertyName type error",{

  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  testthat::expect_error(PipeGeneric$new(propertyName,
                                         alwaysBeforeDeps,
                                         notAfterDeps),
                         "[PipeGeneric][initialize][Error] Checking the type of the 'propertyName' variable: NULL",
                         fixed = TRUE)

})

testthat::test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "example"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  testthat::expect_error(PipeGeneric$new(propertyName,
                                         alwaysBeforeDeps,
                                         notAfterDeps),
                         "[PipeGeneric][initialize][Error] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  testthat::expect_error(PipeGeneric$new(propertyName,
                                         alwaysBeforeDeps,
                                         notAfterDeps),
                         "[PipeGeneric][initialize][Error] Checking the type of the 'notAfterDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("pipe",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  testthat::expect_error(PipeGeneric$new(propertyName,
                                         alwaysBeforeDeps,
                                         notAfterDeps)$pipe(NULL),
                         "I am an abstract interface method",
                         fixed = TRUE)
})

testthat::test_that("getPropertyName",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)

  testthat::expect_equal(pipe$getPropertyName(),
                         propertyName)
})

testthat::test_that("getAlwaysBeforeDeps",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)
  testthat::expect_equal(pipe$getAlwaysBeforeDeps(),
                         alwaysBeforeDeps)
})

testthat::test_that("getNotAfterDeps",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)
  testthat::expect_equal(pipe$getNotAfterDeps(),
                         notAfterDeps)
})

testthat::test_that("setPropertyName",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)

  propertyNameExpected <- "exampleExpected"

  pipe$setPropertyName(propertyNameExpected)
  testthat::expect_equal(pipe$getPropertyName(),
                         propertyNameExpected)
})

testthat::test_that("setPropertyName propertyName type error",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)

  propertyNameExpected <- NULL

  testthat::expect_error(pipe$setPropertyName(propertyNameExpected),
                         "[PipeGeneric][setPropertyName][Error] Checking the type of the 'propertyName' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("setAlwaysBeforeDeps",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)

  alwaysBeforeDepsExpected <- list("pipe")

  pipe$setAlwaysBeforeDeps(alwaysBeforeDepsExpected)
  testthat::expect_equal(pipe$getAlwaysBeforeDeps(),
                         alwaysBeforeDepsExpected)
})

testthat::test_that("setAlwaysBeforeDeps alwaysBeforeDeps type error",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)

  alwaysBeforeDepsExpected <- NULL

  testthat::expect_error(pipe$setAlwaysBeforeDeps(alwaysBeforeDepsExpected),
                         "[PipeGeneric][setAlwaysBeforeDeps][Error] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("setNotAfterDeps",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)

  notAfterDeps <- list("pipe")

  pipe$setNotAfterDeps(notAfterDeps)
  testthat::expect_equal(pipe$getNotAfterDeps(),
                         notAfterDeps)
})

testthat::test_that("setNotAfterDeps notAfterDeps type error",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)

  notAfterDepsExpected <- NULL

  testthat::expect_error(pipe$setNotAfterDeps(notAfterDepsExpected),
                         "[PipeGeneric][setNotAfterDeps][Error] Checking the type of the 'notAfterDeps' variable: NULL",
                         fixed = TRUE)
})
