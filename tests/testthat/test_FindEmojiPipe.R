context("FindEmojiPipe")

test_that("initialize",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rtweet")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  expect_silent(FindEmojiPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, replaceEmojis))
})

test_that("initialize propertyName type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rtweet")
  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  expect_error(FindEmojiPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, replaceEmojis),"\\[FindEmojiPipe\\]\\[initialize\\]\\[Error\\]\\n                Checking the type of the variable: propertyName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rtweet")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  expect_error(FindEmojiPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, replaceEmojis),"\\[FindEmojiPipe\\]\\[initialize\\]\\[Error\\]\\n                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rtweet")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  replaceEmojis <- TRUE
  expect_error(FindEmojiPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, replaceEmojis),"\\[FindEmojiPipe\\]\\[initialize\\]\\[Error\\]\\n                Checking the type of the variable: notAfterDeps NULL")
})

test_that("initialize replaceEmojis type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rtweet")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- NULL
  expect_error(FindEmojiPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, replaceEmojis),"\\[FindEmojiPipe\\]\\[initialize\\]\\[Error\\]\\n                Checking the type of the variable: replaceEmojis NULL")
})

test_that("pipe replaceEmojis <- TRUE",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rtweet")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  pipe <- FindEmojiPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, replaceEmojis)

  path <- file.path("testFiles",
                    "testFindEmojiPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am \U0001f600")

  instance <- pipe$pipe(instance)
  expect_equal(instance$getSpecificProperty("Emojis"),"\U0001f600")
  expect_equal(instance$getData(),"Hey I am  grinning face")
})

test_that("pipe replaceEmojis <- FALSE",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rtweet")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- FALSE
  pipe <- FindEmojiPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, replaceEmojis)

  path <- file.path("testFiles",
                    "testFindEmojiPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am \U0001f600")
  instance <- pipe$pipe(instance)
  expect_equal(instance$getSpecificProperty("Emojis"),"\U0001f600")
  expect_equal(instance$getData(),"Hey I am \U0001f600")
})

test_that("pipe Bad compatibility between Pipes.",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rtweet")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  replaceEmojis <- TRUE
  pipe <- FindEmojiPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, replaceEmojis)

  path <- file.path("testFiles",
                    "testFindEmojiPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  instance$setData("Hey I am \U0001f600")
  expect_error(pipe$pipe(instance),"\\[FindEmojiPipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")
})

test_that("pipe instance type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rtweet")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE
  pipe <- FindEmojiPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, replaceEmojis)

  instance <- NULL
  expect_error(pipe$pipe(instance),"\\[FindEmojiPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("findEmoji",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rtweet")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  pipe <- FindEmojiPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, replaceEmojis)

  data <- "\U0001f600"
  emoji <- "\U0001f600"
  expect_equal(pipe$findEmoji(data, emoji),TRUE)
})

test_that("findEmoji data type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rtweet")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  pipe <- FindEmojiPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, replaceEmojis)

  data <- NULL
  emoji <- "\U0001f600"
  expect_error(pipe$findEmoji(data, emoji),"\\[FindEmojiPipe\\]\\[findEmoji\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})

test_that("findEmoji emoji type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rtweet")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  pipe <- FindEmojiPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, replaceEmojis)

  data <- "\U0001f600"
  emoji <- NULL
  expect_error(pipe$findEmoji(data, emoji),"\\[FindEmojiPipe\\]\\[findEmoji\\]\\[Error\\]
                Checking the type of the variable: emoji NULL")
})

test_that("replaceEmoji",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rtweet")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  pipe <- FindEmojiPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, replaceEmojis)

  emoji <- "\U0001f600"
  extendedEmoji <- "grinning face"
  data <- "\U0001f600"

  expect_equal(pipe$replaceEmoji(emoji, extendedEmoji, data)," grinning face ")
})

test_that("replaceEmoji emoji type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rtweet")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  pipe <- FindEmojiPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, replaceEmojis)

  emoji <- NULL
  extendedEmoji <- "grinning face"
  data <- "\U0001f600"

  expect_error(pipe$replaceEmoji(emoji, extendedEmoji, data),"\\[FindEmojiPipe\\]\\[replaceEmoji\\]\\[Error\\]
                Checking the type of the variable: emoji NULL")

})

test_that("replaceEmoji extendedEmoji type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rtweet")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  pipe <- FindEmojiPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, replaceEmojis)
  emoji <- "\U0001f600"
  extendedEmoji <- NULL
  data <- "\U0001f600"

  expect_error(pipe$replaceEmoji(emoji, extendedEmoji, data),"\\[FindEmojiPipe\\]\\[replaceEmoji\\]\\[Error\\]
                Checking the type of the variable: extendedEmoji NULL")
})

test_that("replaceEmoji data type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rtweet")
  propertyName <- "Emojis"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceEmojis <- TRUE

  pipe <- FindEmojiPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, replaceEmojis)
  emoji <- "\U0001f600"
  extendedEmoji <-  "grinning face"
  data <- NULL

  expect_error(pipe$replaceEmoji(emoji, extendedEmoji, data),"\\[FindEmojiPipe\\]\\[replaceEmoji\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})
