testthat::context("GenericPipe")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- GenericPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)
  testthat::expect_equal(pipe$.__enclos_env__$private$propertyName,
                         propertyName)
  testthat::expect_equal(pipe$.__enclos_env__$private$alwaysBeforeDeps,
                         alwaysBeforeDeps)
  testthat::expect_equal(pipe$.__enclos_env__$private$notAfterDeps,
                         notAfterDeps)
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

  testthat::expect_error(GenericPipe$new(propertyName,
                                         alwaysBeforeDeps,
                                         notAfterDeps),
                         "[GenericPipe][initialize][FATAL] Checking the type of the 'propertyName' variable: NULL",
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

  propertyName <- "example"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  testthat::expect_error(GenericPipe$new(propertyName,
                                         alwaysBeforeDeps,
                                         notAfterDeps),
                         "[GenericPipe][initialize][FATAL] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
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

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  testthat::expect_error(GenericPipe$new(propertyName,
                                         alwaysBeforeDeps,
                                         notAfterDeps),
                         "[GenericPipe][initialize][FATAL] Checking the type of the 'notAfterDeps' variable: NULL",
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

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  testthat::expect_error(GenericPipe$new(propertyName,
                                         alwaysBeforeDeps,
                                         notAfterDeps)$pipe(NULL),
                         "I am an abstract interface method",
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

testthat::test_that("getPropertyName",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- GenericPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)

  testthat::expect_equal(pipe$getPropertyName(),
                         propertyName)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("getAlwaysBeforeDeps",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- GenericPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)
  testthat::expect_equal(pipe$getAlwaysBeforeDeps(),
                         alwaysBeforeDeps)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("getNotAfterDeps",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- GenericPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)
  testthat::expect_equal(pipe$getNotAfterDeps(),
                         notAfterDeps)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("setPropertyName",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- GenericPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)

  propertyNameExpected <- "exampleExpected"

  pipe$setPropertyName(propertyNameExpected)
  testthat::expect_equal(pipe$getPropertyName(),
                         propertyNameExpected)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("setPropertyName propertyName type error",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- GenericPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)

  propertyNameExpected <- NULL

  testthat::expect_error(pipe$setPropertyName(propertyNameExpected),
                         "[GenericPipe][setPropertyName][FATAL] Checking the type of the 'propertyName' variable: NULL",
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

testthat::test_that("setAlwaysBeforeDeps",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- GenericPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)

  alwaysBeforeDepsExpected <- list("pipe")

  pipe$setAlwaysBeforeDeps(alwaysBeforeDepsExpected)
  testthat::expect_equal(pipe$getAlwaysBeforeDeps(),
                         alwaysBeforeDepsExpected)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("setAlwaysBeforeDeps alwaysBeforeDeps type error",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- GenericPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)

  alwaysBeforeDepsExpected <- NULL

  testthat::expect_error(pipe$setAlwaysBeforeDeps(alwaysBeforeDepsExpected),
                         "[GenericPipe][setAlwaysBeforeDeps][FATAL] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
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

testthat::test_that("setNotAfterDeps",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- GenericPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)

  notAfterDeps <- list("pipe")

  pipe$setNotAfterDeps(notAfterDeps)
  testthat::expect_equal(pipe$getNotAfterDeps(),
                         notAfterDeps)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("setNotAfterDeps notAfterDeps type error",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- GenericPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps)

  notAfterDepsExpected <- NULL

  testthat::expect_error(pipe$setNotAfterDeps(notAfterDepsExpected),
                         "[GenericPipe][setNotAfterDeps][FATAL] Checking the type of the 'notAfterDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
