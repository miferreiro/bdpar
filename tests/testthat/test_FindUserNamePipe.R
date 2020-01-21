context("FindUserNamePipe")

test_that("initialize",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_silent(FindUserNamePipe$new(propertyName,alwaysBeforeDeps,notAfterDeps))
})

test_that("initialize propertyName type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(FindUserNamePipe$new(propertyName,alwaysBeforeDeps,notAfterDeps),"\\[FindUserNamePipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  expect_error(FindUserNamePipe$new(propertyName,alwaysBeforeDeps,notAfterDeps),"\\[FindUserNamePipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  expect_error(FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps),"\\[FindUserNamePipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("pipe removeUser <- TRUE",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testUserNamePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am @example")
  removeUser <- TRUE
  instance <- pipe$pipe(instance, removeUser)
  expect_equal(instance$getSpecificProperty("userName"),"@example")
  expect_equal(instance$getData(),"Hey I am")

})

test_that("pipe removeUser <- FALSE",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testUserNamePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am @example")
  removeUser <- FALSE
  instance <- pipe$pipe(instance, removeUser)
  expect_equal(instance$getSpecificProperty("userName"),"@example")
  expect_equal(instance$getData(),"Hey I am @example")

})

test_that("pipe Bad compatibility between Pipes.",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  pipe <- FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testUserNamePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  instance$setData("Hey I am @example")
  removeUser <- TRUE
  expect_error(pipe$pipe(instance, removeUser),"\\[FindUserNamePipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  instance <- NULL
  removeUser <- TRUE
  expect_error(pipe$pipe(instance, removeUser),"\\[FindUserNamePipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("pipe removeUser type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testUserNamePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am @example")
  removeUser <- NULL
  expect_error(pipe$pipe(instance, removeUser),"\\[FindUserNamePipe\\]\\[pipe\\]\\[Error\\]
                  Checking the type of the variable: removeUser NULL")

})

test_that("pipe empty data",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testFindUserNamePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("@example")
  removeUser <- TRUE
  expect_warning(pipe$pipe(instance, removeUser),"\\[FindUserNamePipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testFindUserNamePipe\\/testFile\\.tsms has data empty on pipe UserName ")
  expect_equal(instance$getSpecificProperty("userName"),"@example")
  expect_equal(instance$getData(),"")

})

test_that("findUserName",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  data <- "@example"

  expect_equal(pipe$findUserName(data),"@example")

})

test_that("findUserName data type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  data <- NULL

  expect_error(pipe$findUserName(data),"\\[FindUserNamePipe\\]\\[findUserName\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})

test_that("removeUserName",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  data <- "@example"

  expect_equal(pipe$removeUserName(data)," ")

})

test_that("removeUserName data type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  data <- NULL

  expect_error(pipe$removeUserName(data),"\\[FindUserNamePipe\\]\\[removeUserName\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})
