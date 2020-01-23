context("FindUserNamePipe")

test_that("initialize",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUser <- TRUE

  expect_silent(FindUserNamePipe$new(propertyName,alwaysBeforeDeps,notAfterDeps,removeUser))
})

test_that("initialize propertyName type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUser <- TRUE

  expect_error(FindUserNamePipe$new(propertyName,alwaysBeforeDeps,notAfterDeps,removeUser),"\\[FindUserNamePipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  removeUser <- TRUE

  expect_error(FindUserNamePipe$new(propertyName,alwaysBeforeDeps,notAfterDeps,removeUser),"\\[FindUserNamePipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  removeUser <- TRUE

  expect_error(FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps,removeUser),"\\[FindUserNamePipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("initialize removeUser type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUser <- NULL

  expect_error(FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUser),
  "[FindUserNamePipe][initialize][Error]
                  Checking the type of the variable: removeUser NULL", fixed = TRUE)

})

test_that("pipe removeUser <- TRUE",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUser <- TRUE

  pipe <- FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps,removeUser)

  path <- file.path("testFiles",
                    "testUserNamePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am @example")
  instance <- pipe$pipe(instance)
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
  removeUser <- FALSE

  pipe <- FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps,removeUser)

  path <- file.path("testFiles",
                    "testUserNamePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am @example")
  instance <- pipe$pipe(instance)
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
  removeUser <- TRUE
  pipe <- FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps,removeUser)

  path <- file.path("testFiles",
                    "testUserNamePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  instance$setData("Hey I am @example")
  expect_error(pipe$pipe(instance),"\\[FindUserNamePipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUser <- TRUE

  pipe <- FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps,removeUser)

  instance <- NULL
  expect_error(pipe$pipe(instance),"\\[FindUserNamePipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("pipe empty data",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "userName"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUser <- TRUE
  pipe <- FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps,removeUser)

  path <- file.path("testFiles",
                    "testFindUserNamePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("@example")
  expect_warning(pipe$pipe(instance),"\\[FindUserNamePipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testFindUserNamePipe\\/testFile\\.tsms has data empty on pipe UserName ")
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
  removeUser <- TRUE
  pipe <- FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps,removeUser)

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
  removeUser <- TRUE

  pipe <- FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps,removeUser)

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
  removeUser <- TRUE

  pipe <- FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps,removeUser)

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
  removeUser <- TRUE

  pipe <- FindUserNamePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps,removeUser)

  data <- NULL

  expect_error(pipe$removeUserName(data),"\\[FindUserNamePipe\\]\\[removeUserName\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})
