context("FindEmoticonPipe")

test_that("initialize",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_silent(FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps))
})

test_that("initialize propertyName type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps),"\\[FindEmoticonPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  expect_error(FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps),"\\[FindEmoticonPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  expect_error(FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps),"\\[FindEmoticonPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("pipe removeEmoticon <- TRUE",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testFindEmoticonPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am :)")
  removeEmoticon <- TRUE
  instance <- pipe$pipe(instance, removeEmoticon)
  expect_equal(instance$getSpecificProperty("emoticon"),":)")
  expect_equal(instance$getData(),"Hey I am")

})

test_that("pipe removeEmoticon <- FALSE",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testFindEmoticonPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am :)")
  removeEmoticon <- FALSE
  instance <- pipe$pipe(instance, removeEmoticon)
  expect_equal(instance$getSpecificProperty("emoticon"),":)")
  expect_equal(instance$getData(),"Hey I am :)")

})

test_that("pipe Bad compatibility between Pipes.",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  pipe <- FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testFindEmoticonPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  instance$setData("Hey I am :)")
  removeEmoticon <- TRUE
  expect_error(pipe$pipe(instance, removeEmoticon),"\\[FindEmoticonPipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  instance <- NULL
  removeEmoticon <- TRUE
  expect_error(pipe$pipe(instance, removeEmoticon),"\\[FindEmoticonPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("pipe removeEmoticon type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testFindEmoticonPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am :)")
  removeEmoticon <- NULL
  expect_error(pipe$pipe(instance, removeEmoticon),"\\[FindEmoticonPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: removeEmoticon NULL")

})

test_that("pipe empty data",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testFindEmoticonPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData(":)")
  removeEmoticon <- TRUE
  expect_warning(pipe$pipe(instance, removeEmoticon),"\\[FindEmoticonPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testFindEmoticonPipe\\/testFile\\.tsms has data empty on pipe Emoticon ")
  expect_equal(instance$getSpecificProperty("emoticon"),":)")
  expect_equal(instance$getData(),"")

})

test_that("findEmoticon",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  data <- ":)"

  expect_equal(pipe$findEmoticon(data),":)")

})

test_that("findEmoticon data type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  data <- NULL

  expect_error(pipe$findEmoticon(data),"\\[FindEmoticonPipe\\]\\[findEmoticon\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})

test_that("removeEmoticon",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  data <- ":)"

  expect_equal(pipe$removeEmoticon(data)," ")

})

test_that("removeEmoticon data type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  data <- NULL

  expect_error(pipe$removeEmoticon(data),"\\[FindEmoticonPipe\\]\\[removeEmoticon\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})
