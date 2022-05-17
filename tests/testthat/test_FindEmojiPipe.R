testthat::context("FindEmojiPipe")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize",{
  testthat::skip_if_not_installed("rex")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  expect_silent(FindEmojiPipe$new(propertyName,
                                  alwaysBeforeDeps,
                                  notAfterDeps,
                                  replaceEmojis))
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
  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  testthat::expect_error(FindEmojiPipe$new(propertyName,
                                           alwaysBeforeDeps,
                                           notAfterDeps,
                                           replaceEmojis),
                         "[FindEmojiPipe][initialize][FATAL] Checking the type of the 'propertyName' variable: NULL",
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
  propertyName <- "Emojis"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  testthat::expect_error(FindEmojiPipe$new(propertyName,
                                           alwaysBeforeDeps,
                                           notAfterDeps,
                                           replaceEmojis),
                         "[FindEmojiPipe][initialize][FATAL] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
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
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  replaceEmojis <- TRUE
  testthat::expect_error(FindEmojiPipe$new(propertyName,
                                           alwaysBeforeDeps,
                                           notAfterDeps,
                                           replaceEmojis),
                         "[FindEmojiPipe][initialize][FATAL] Checking the type of the 'notAfterDeps' variable: NULL",
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

testthat::test_that("initialize replaceEmojis type error",{
  testthat::skip_if_not_installed("rex")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- NULL
  testthat::expect_error(FindEmojiPipe$new(propertyName,
                                           alwaysBeforeDeps,
                                           notAfterDeps,
                                           replaceEmojis),
                         "[FindEmojiPipe][initialize][FATAL] Checking the type of the 'replaceEmojis' variable: NULL",
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

testthat::test_that("pipe replaceEmojis <- TRUE",{
  testthat::skip_if_not_installed("rex")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  pipe <- FindEmojiPipe$new(propertyName,
                            alwaysBeforeDeps,
                            notAfterDeps,
                            replaceEmojis)

  path <- file.path("testFiles",
                    "testFindEmojiPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am \U0001f600")

  instance <- pipe$pipe(instance)
  testthat::expect_equal(instance$getSpecificProperty("Emojis"),
                         "\U0001f600")
  testthat::expect_equal(instance$getData(),
                         "Hey I am  grinning face")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("pipe replaceEmojis <- FALSE",{
  testthat::skip_if_not_installed("rex")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- FALSE
  pipe <- FindEmojiPipe$new(propertyName,
                            alwaysBeforeDeps,
                            notAfterDeps,
                            replaceEmojis)

  path <- file.path("testFiles",
                    "testFindEmojiPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am \U0001f600")
  instance <- pipe$pipe(instance)
  testthat::expect_equal(instance$getSpecificProperty("Emojis"),
                         "\U0001f600")
  testthat::expect_equal(instance$getData(),
                         "Hey I am \U0001f600")
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
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE
  pipe <- FindEmojiPipe$new(propertyName,
                            alwaysBeforeDeps,
                            notAfterDeps,
                            replaceEmojis)

  instance <- NULL
  testthat::expect_error(pipe$pipe(instance),
                         "[FindEmojiPipe][pipe][FATAL] Checking the type of the 'instance' variable: NULL",
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

testthat::test_that("findEmoji",{
  testthat::skip_if_not_installed("rex")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  pipe <- FindEmojiPipe$new(propertyName,
                            alwaysBeforeDeps,
                            notAfterDeps,
                            replaceEmojis)

  data <- "\U0001f600"
  emoji <- "\U0001f600"
  testthat::expect_equal(pipe$findEmoji(data,
                                        emoji),
                         TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("findEmoji data type error",{
  testthat::skip_if_not_installed("rex")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  pipe <- FindEmojiPipe$new(propertyName,
                            alwaysBeforeDeps,
                            notAfterDeps,
                            replaceEmojis)

  data <- NULL
  emoji <- "\U0001f600"
  testthat::expect_error(pipe$findEmoji(data, emoji),
                         "[FindEmojiPipe][findEmoji][FATAL] Checking the type of the 'data' variable: NULL",
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

testthat::test_that("findEmoji emoji type error",{
  testthat::skip_if_not_installed("rex")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  pipe <- FindEmojiPipe$new(propertyName,
                            alwaysBeforeDeps,
                            notAfterDeps,
                            replaceEmojis)

  data <- "\U0001f600"
  emoji <- NULL
  testthat::expect_error(pipe$findEmoji(data,
                                        emoji),
                         "[FindEmojiPipe][findEmoji][FATAL] Checking the type of the 'emoji' variable: NULL",
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

testthat::test_that("replaceEmoji",{
  testthat::skip_if_not_installed("rex")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  pipe <- FindEmojiPipe$new(propertyName,
                            alwaysBeforeDeps,
                            notAfterDeps,
                            replaceEmojis)

  emoji <- "\U0001f600"
  extendedEmoji <- "grinning face"
  data <- "\U0001f600"

  testthat::expect_equal(pipe$replaceEmoji(emoji,
                                           extendedEmoji,
                                           data),
                         " grinning face ")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("replaceEmoji emoji type error",{
  testthat::skip_if_not_installed("rex")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  pipe <- FindEmojiPipe$new(propertyName,
                            alwaysBeforeDeps,
                            notAfterDeps,
                            replaceEmojis)

  emoji <- NULL
  extendedEmoji <- "grinning face"
  data <- "\U0001f600"

  testthat::expect_error(pipe$replaceEmoji(emoji,
                                           extendedEmoji,
                                           data),
                         "[FindEmojiPipe][replaceEmoji][FATAL] Checking the type of the 'emoji' variable: NULL",
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

testthat::test_that("replaceEmoji extendedEmoji type error",{
  testthat::skip_if_not_installed("rex")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  pipe <- FindEmojiPipe$new(propertyName,
                            alwaysBeforeDeps,
                            notAfterDeps,
                            replaceEmojis)
  emoji <- "\U0001f600"
  extendedEmoji <- NULL
  data <- "\U0001f600"

  testthat::expect_error(pipe$replaceEmoji(emoji,
                                           extendedEmoji,
                                           data),
                         "[FindEmojiPipe][replaceEmoji][FATAL] Checking the type of the 'extendedEmoji' variable: NULL",
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

testthat::test_that("replaceEmoji data type error",{
  testthat::skip_if_not_installed("rex")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  pipe <- FindEmojiPipe$new(propertyName,
                            alwaysBeforeDeps,
                            notAfterDeps,
                            replaceEmojis)
  emoji <- "\U0001f600"
  extendedEmoji <-  "grinning face"
  data <- NULL

  testthat::expect_error(pipe$replaceEmoji(emoji,
                                           extendedEmoji,
                                           data),
                         "[FindEmojiPipe][replaceEmoji][FATAL] Checking the type of the 'data' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
