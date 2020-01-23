context("FindEmoticonPipe")

test_that("initialize",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeEmoticons <- TRUE

  expect_silent(FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeEmoticons))
})

test_that("initialize propertyName type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeEmoticons <- TRUE

  expect_error(FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeEmoticons),"\\[FindEmoticonPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  removeEmoticons <- TRUE

  expect_error(FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeEmoticons),"\\[FindEmoticonPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  removeEmoticons <- TRUE

  expect_error(FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeEmoticons),"\\[FindEmoticonPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("initialize removeEmoticons type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeEmoticons <- NULL

  expect_error(FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeEmoticons),"\\[FindEmoticonPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: removeEmoticons NULL")

})

test_that("pipe removeEmoticons <- TRUE",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeEmoticons <- TRUE

  pipe <- FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeEmoticons)

  path <- file.path("testFiles",
                    "testFindEmoticonPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am :)")
  instance <- pipe$pipe(instance)
  expect_equal(instance$getSpecificProperty("emoticon"),":)")
  expect_equal(instance$getData(),"Hey I am")

})

test_that("pipe removeEmoticons <- FALSE",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeEmoticons <- FALSE
  pipe <- FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeEmoticons)

  path <- file.path("testFiles",
                    "testFindEmoticonPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am :)")
  instance <- pipe$pipe(instance)
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
  removeEmoticons <- TRUE

  pipe <- FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeEmoticons)

  path <- file.path("testFiles",
                    "testFindEmoticonPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  instance$setData("Hey I am :)")
  expect_error(pipe$pipe(instance),"\\[FindEmoticonPipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeEmoticons <- TRUE

  pipe <- FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeEmoticons)

  instance <- NULL
  expect_error(pipe$pipe(instance),"\\[FindEmoticonPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("pipe empty data",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeEmoticons <- TRUE

  pipe <- FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeEmoticons)

  path <- file.path("testFiles",
                    "testFindEmoticonPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$obtainSource()
  expect_warning(pipe$pipe(instance),"\\[FindEmoticonPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testFindEmoticonPipe\\/testFile\\.tsms has data empty on pipe Emoticon ")
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
  removeEmoticons <- TRUE

  pipe <- FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeEmoticons)

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
  removeEmoticons <- TRUE

  pipe <- FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeEmoticons)

  data <- NULL

  expect_error(pipe$findEmoticon(data),"\\[FindEmoticonPipe\\]\\[findEmoticon\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})

test_that("removeEmoticons",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeEmoticons <- TRUE

  pipe <- FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeEmoticons)

  data <- ":)"

  expect_equal(pipe$removeEmoticon(data)," ")

})

test_that("removeEmoticons data type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "emoticon"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeEmoticons <- TRUE

  pipe <- FindEmoticonPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeEmoticons)

  data <- NULL

  expect_error(pipe$removeEmoticon(data),"\\[FindEmoticonPipe\\]\\[removeEmoticon\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})
