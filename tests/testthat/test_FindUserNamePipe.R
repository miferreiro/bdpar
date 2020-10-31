testthat::context("FindUserNamePipe")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUser <- TRUE

  testthat::expect_silent(FindUserNamePipe$new(propertyName,
                                               alwaysBeforeDeps,
                                               notAfterDeps,
                                               removeUser))
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
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUser <- TRUE

  testthat::expect_error(FindUserNamePipe$new(propertyName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              removeUser),
                         "[FindUserNamePipe][initialize][FATAL] Checking the type of the 'propertyName' variable: NULL",
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
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  removeUser <- TRUE

  testthat::expect_error(FindUserNamePipe$new(propertyName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              removeUser),
                         "[FindUserNamePipe][initialize][FATAL] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
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
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  removeUser <- TRUE

  testthat::expect_error(FindUserNamePipe$new(propertyName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              removeUser),
                         "[FindUserNamePipe][initialize][FATAL] Checking the type of the 'notAfterDeps' variable: NULL",
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

testthat::test_that("initialize removeUser type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUser <- NULL

  testthat::expect_error(FindUserNamePipe$new(propertyName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              removeUser),
                         "[FindUserNamePipe][initialize][FATAL] Checking the type of the 'removeUser' variable: NULL",
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

testthat::test_that("pipe removeUser <- TRUE",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUser <- TRUE

  pipe <- FindUserNamePipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeUser)

  path <- file.path("testFiles",
                    "testUserNamePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am @example")
  instance <- pipe$pipe(instance)
  testthat::expect_equal(instance$getSpecificProperty("userName"),
                         "@example")
  testthat::expect_equal(instance$getData(),
                         "Hey I am")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("pipe removeUser <- FALSE",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUser <- FALSE

  pipe <- FindUserNamePipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeUser)

  path <- file.path("testFiles",
                    "testUserNamePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am @example")
  instance <- pipe$pipe(instance)
  testthat::expect_equal(instance$getSpecificProperty("userName"),
                         "@example")
  testthat::expect_equal(instance$getData(),
                         "Hey I am @example")
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
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUser <- TRUE

  pipe <- FindUserNamePipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeUser)

  instance <- NULL
  testthat::expect_error(pipe$pipe(instance),
                         "[FindUserNamePipe][pipe][FATAL] Checking the type of the 'instance' variable: NULL",
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

testthat::test_that("pipe empty data",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUser <- TRUE
  pipe <- FindUserNamePipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeUser)

  path <- file.path("testFiles",
                    "testFindUserNamePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("@example")
  expect_warning(pipe$pipe(instance),
                 "\\[FindUserNamePipe\\]\\[pipe\\]\\[WARN\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testFindUserNamePipe\\/testFile\\.tsms has data empty on pipe UserName")
  testthat::expect_equal(instance$getSpecificProperty("userName"),
                         "@example")
  testthat::expect_equal(instance$getData(),
                         "")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("findUserName",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUser <- TRUE
  pipe <- FindUserNamePipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeUser)

  data <- "@example"

  testthat::expect_equal(pipe$findUserName(data),
                         "@example")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("findUserName data type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUser <- TRUE

  pipe <- FindUserNamePipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeUser)

  data <- NULL

  testthat::expect_error(pipe$findUserName(data),
                         "[FindUserNamePipe][findUserName][FATAL] Checking the type of the 'data' variable: NULL",
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

testthat::test_that("removeUserName",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUser <- TRUE

  pipe <- FindUserNamePipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeUser)

  data <- "@example"

  testthat::expect_equal(pipe$removeUserName(data),
                         " ")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("removeUserName data type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUser <- TRUE

  pipe <- FindUserNamePipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeUser)

  data <- NULL

  testthat::expect_error(pipe$removeUserName(data),
                         "[FindUserNamePipe][removeUserName][FATAL] Checking the type of the 'data' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
