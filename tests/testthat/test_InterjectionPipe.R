context("InterjectionPipe")

test_that("initialize",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testInterjectionPipe",
                                              "configurations.ini"))

  expect_silent(InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps, removeInterjections))
})

test_that("initialize propertyName type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- NULL
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE

  expect_error(InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps, removeInterjections),"\\[InterjectionPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize propertyLanguageName type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE

  expect_error(InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps, removeInterjections),"\\[InterjectionPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyLanguageName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  removeInterjections <- TRUE

  expect_error(InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, removeInterjections),"\\[InterjectionPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  removeInterjections <- TRUE

  expect_error(InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, removeInterjections),"\\[InterjectionPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("initialize removeInterjections type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- NULL

  expect_error(InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, removeInterjections),"\\[InterjectionPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: removeInterjections NULL")

})

test_that("pipe",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testInterjectionPipe",
                                              "configurations.ini"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, removeInterjections)

  path <- file.path("testFiles",
                    "testInterjectionPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("yeah I like it")
  instance$addProperties("en","language")

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  pipe$setResourcesInterjectionsPath(path)
  instance <- pipe$pipe(instance)

  expect_equal(instance$getSpecificProperty("interjection"),c("yeah","like"))

})

test_that("pipe data empty",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testInterjectionPipe",
                                              "configurations.ini"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, removeInterjections)

  path <- file.path("testFiles",
                    "testInterjectionPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("")
  instance$addProperties("en","language")

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  pipe$setResourcesInterjectionsPath(path)

  expect_warning(pipe$pipe(instance),"\\[InterjectionPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testInterjectionPipe\\/testFile\\.tsms has data empty on pipe Interjection ")

})

test_that("pipe wihtout json file",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testInterjectionPipe",
                                              "configurations.ini"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, removeInterjections)

  path <- file.path("testFiles",
                    "testInterjectionPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("yeah I like it")
  instance$addProperties("en","language")

  expect_warning(pipe$pipe(instance),"\\[InterjectionPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testInterjectionPipe\\/testFile\\.tsms has not an interjectionsJsonFile to apply to the language ->en ")

})

test_that("pipe wihtout language property",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testInterjectionPipe",
                                              "configurations.ini"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, removeInterjections)

  path <- file.path("testFiles",
                    "testInterjectionPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("yeah I like it")

  expect_warning(pipe$pipe(instance),"\\[InterjectionPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testInterjectionPipe\\/testFile\\.tsms has not language property")

})

test_that("pipe Bad compatibility between Pipes.",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  removeInterjections <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testInterjectionPipe",
                                              "configurations.ini"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, removeInterjections)

  path <- file.path("testFiles",
                    "testInterjectionPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  instance$setData("yeah I like it")
  expect_error(pipe$pipe(instance),"\\[InterjectionPipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testInterjectionPipe",
                                              "configurations.ini"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, removeInterjections)

  instance <- NULL

  expect_error(pipe$pipe(instance),"\\[InterjectionPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("findInterjection",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps, removeInterjections)

  interjection <- "yeah"
  data <- "yeah I like it"

  expect_equal(pipe$findInterjection(data, interjection), TRUE)

})

test_that("findInterjection interjection type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps, removeInterjections)

  interjection <- NULL
  data <- "yeah I like it"

  expect_error(pipe$findInterjection(data, interjection),"\\[InterjectionPipe\\]\\[findInterjection]\\[Error\\]
                Checking the type of the variable: interjection NULL")
})

test_that("findInterjection data type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps, removeInterjections)

  interjection <- "yeah"
  data <- NULL

  expect_error(pipe$findInterjection(data, interjection),"\\[InterjectionPipe\\]\\[findInterjection\\]\\[Error\\]
                Checking the type of the variable: data NULL")
})

test_that("removeInterjection ",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps, removeInterjections)

  interjection <- "yeah"
  data <- "yeah I like it"

  expect_equal(pipe$removeInterjection(interjection, data)," I like it")

})

test_that("removeInterjection abbreviation type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps, removeInterjections)

  interjection <- NULL
  data <- "yeah I like it"

  expect_error(pipe$removeInterjection(interjection, data),"\\[InterjectionPipe\\]\\[removeInterjection\\]\\[Error\\]
                Checking the type of the variable: interjection NULL")

})

test_that("removeInterjection data type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps, removeInterjections)

  interjection <- "yeah"
  data <- NULL

  expect_error(pipe$removeInterjection(interjection, data),"\\[InterjectionPipe\\]\\[removeInterjection\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})

test_that("getPropertyLanguageName",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testInterjectionPipe",
                                              "configurations.ini"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, removeInterjections)

  expect_equal(pipe$getPropertyLanguageName(), "language")

})

test_that("getResourcesInterjectionsPath",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testInterjectionPipe",
                                              "configurations.ini"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, removeInterjections)

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json",
                    "interj.en.json")

  pipe$setResourcesInterjectionsPath(path)

  expect_equal(pipe$getResourcesInterjectionsPath(), path)

})

test_that("setResourcesInterjectionsPath",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testInterjectionPipe",
                                              "configurations.ini"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, removeInterjections)

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json",
                    "interj.en.json")


  pipe$setResourcesInterjectionsPath(path)

  expect_equal(pipe$getResourcesInterjectionsPath(), path)

})

test_that("setResourcesInterjectionsPath path type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testInterjectionPipe",
                                              "configurations.ini"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, removeInterjections)

  path <- NULL

  expect_error(pipe$setResourcesInterjectionsPath(path),"\\[InterjectionPipe\\]\\[setResourcesInterjectionsPath\\]\\[Error\\]
                Checking the type of the variable: path NULL")

})
