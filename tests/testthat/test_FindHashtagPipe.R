context("FindHashtagPipe")

test_that("initialize",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- TRUE

  expect_silent(FindHashtagPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeHashtags))
})

test_that("initialize propertyName type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- TRUE

  expect_error(FindHashtagPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeHashtags),"\\[FindHashtagPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  removeHashtags <- TRUE

  expect_error(FindHashtagPipe$new(propertyName,alwaysBeforeDeps,notAfterDeps, removeHashtags),"\\[FindHashtagPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  removeHashtags <- TRUE

  expect_error(FindHashtagPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeHashtags),"\\[FindHashtagPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("initialize removeHashtags type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- NULL

  expect_error(FindHashtagPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeHashtags),"\\[FindHashtagPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: removeHashtags NULL")

})

test_that("pipe removeHashtags <- TRUE",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- TRUE

  pipe <- FindHashtagPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeHashtags)

  path <- file.path("testFiles",
                    "testFindHashtagPipe",
                    "testFile.tsms")
  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am #example")

  instance <- pipe$pipe(instance)
  expect_equal(instance$getSpecificProperty("hashtag"),"#example")
  expect_equal(instance$getData(),"Hey I am")

})

test_that("pipe removeHashtags <- FALSE",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- FALSE

  pipe <- FindHashtagPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeHashtags)

  path <- file.path("testFiles",
                    "testFindHashtagPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am #example")

  instance <- pipe$pipe(instance)
  expect_equal(instance$getSpecificProperty("hashtag"),"#example")
  expect_equal(instance$getData(),"Hey I am #example")

})

test_that("pipe Bad compatibility between Pipes.",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  removeHashtags <- TRUE

  pipe <- FindHashtagPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeHashtags)

  path <- file.path("testFiles",
                    "testFindHashtagPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  instance$setData("Hey I am #example")

  expect_error(pipe$pipe(instance),"\\[FindHashtagPipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- TRUE

  pipe <- FindHashtagPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeHashtags)

  instance <- NULL
  expect_error(pipe$pipe(instance),"\\[FindHashtagPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("pipe empty data",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- TRUE

  pipe <- FindHashtagPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeHashtags)

  path <- file.path("testFiles",
                    "testFindHashtagPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("#example")
  expect_warning(pipe$pipe(instance),"\\[FindHashtagPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testFindHashtagPipe\\/testFile\\.tsms has data empty on pipe Hashtag ")
  expect_equal(instance$getSpecificProperty("hashtag"),"#example")
  expect_equal(instance$getData(),"")

})

test_that("findUserName",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- TRUE

  pipe <- FindHashtagPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeHashtags)

  data <- "#example"

  expect_equal(pipe$findHashtag(data),"#example")

})

test_that("findHashtag data type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- TRUE

  pipe <- FindHashtagPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeHashtags)

  data <- NULL

  expect_error(pipe$findHashtag(data),"\\[FindHashtagPipe\\]\\[findHashtag\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})

test_that("removeHashtag",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- TRUE

  pipe <- FindHashtagPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeHashtags)

  data <- "#example"

  expect_equal(pipe$removeHashtag(data)," ")

})

test_that("removeHashtag data type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- TRUE

  pipe <- FindHashtagPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeHashtags)

  data <- NULL

  expect_error(pipe$removeHashtag(data),"\\[FindHashtagPipe\\]\\[removeHashtag\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})
