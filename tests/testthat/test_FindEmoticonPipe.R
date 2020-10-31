testthat::context("FindEmoticonPipe")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeEmoticons <- TRUE

  expect_silent(FindEmoticonPipe$new(propertyName,
                                     alwaysBeforeDeps,
                                     notAfterDeps,
                                     removeEmoticons))
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
  removeEmoticons <- TRUE

  testthat::expect_error(FindEmoticonPipe$new(propertyName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              removeEmoticons),
                         "[FindEmoticonPipe][initialize][FATAL] Checking the type of the 'propertyName' variable: NULL",
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
  propertyName <- "emoticon"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  removeEmoticons <- TRUE

  testthat::expect_error(FindEmoticonPipe$new(propertyName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              removeEmoticons),
                         "[FindEmoticonPipe][initialize][FATAL] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
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
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  removeEmoticons <- TRUE

  testthat::expect_error(FindEmoticonPipe$new(propertyName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              removeEmoticons),
                         "[FindEmoticonPipe][initialize][FATAL] Checking the type of the 'notAfterDeps' variable: NULL",
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

testthat::test_that("initialize removeEmoticons type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeEmoticons <- NULL

  testthat::expect_error(FindEmoticonPipe$new(propertyName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              removeEmoticons),
                         "[FindEmoticonPipe][initialize][FATAL] Checking the type of the 'removeEmoticons' variable: NULL",
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

testthat::test_that("pipe removeEmoticons <- TRUE",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeEmoticons <- TRUE

  pipe <- FindEmoticonPipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeEmoticons)

  path <- file.path("testFiles",
                    "testFindEmoticonPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am :)")
  instance <- pipe$pipe(instance)
  testthat::expect_equal(instance$getSpecificProperty("emoticon"),
                         ":)")
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

testthat::test_that("pipe removeEmoticons <- FALSE",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeEmoticons <- FALSE
  pipe <- FindEmoticonPipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeEmoticons)

  path <- file.path("testFiles",
                    "testFindEmoticonPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am :)")
  instance <- pipe$pipe(instance)
  testthat::expect_equal(instance$getSpecificProperty("emoticon"),
                         ":)")
  testthat::expect_equal(instance$getData(),
                         "Hey I am :)")

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
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeEmoticons <- TRUE

  pipe <- FindEmoticonPipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeEmoticons)

  instance <- NULL
  testthat::expect_error(pipe$pipe(instance),
                         "[FindEmoticonPipe][pipe][FATAL] Checking the type of the 'instance' variable: NULL",
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
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeEmoticons <- TRUE

  pipe <- FindEmoticonPipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeEmoticons)

  path <- file.path("testFiles",
                    "testFindEmoticonPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$obtainSource()
  testthat::expect_warning(pipe$pipe(instance),
                          "\\[FindEmoticonPipe\\]\\[pipe\\]\\[WARN\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testFindEmoticonPipe\\/testFile\\.tsms has data empty on pipe Emoticon")
  testthat::expect_equal(instance$getSpecificProperty("emoticon"),
                         ":)")
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

testthat::test_that("findEmoticon",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeEmoticons <- TRUE

  pipe <- FindEmoticonPipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeEmoticons)

  data <- ":)"

  testthat::expect_equal(pipe$findEmoticon(data),
                         ":)")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("findEmoticon data type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeEmoticons <- TRUE

  pipe <- FindEmoticonPipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeEmoticons)

  data <- NULL

  testthat::expect_error(pipe$findEmoticon(data),
                         "[FindEmoticonPipe][findEmoticon][FATAL] Checking the type of the 'data' variable: NULL",
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

testthat::test_that("removeEmoticons",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeEmoticons <- TRUE

  pipe <- FindEmoticonPipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeEmoticons)

  data <- ":)"

  testthat::expect_equal(pipe$removeEmoticon(data),
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

testthat::test_that("removeEmoticons data type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeEmoticons <- TRUE

  pipe <- FindEmoticonPipe$new(propertyName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeEmoticons)

  data <- NULL

  testthat::expect_error(pipe$removeEmoticon(data),
                         "[FindEmoticonPipe][removeEmoticon][FATAL] Checking the type of the 'data' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
