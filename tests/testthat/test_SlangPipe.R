context("SlangPipe")

test_that("initialize",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testSlangPipe",
                                              "configurations.ini"))

  expect_silent(SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps))
})

test_that("initialize propertyName type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- NULL
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps),"\\[SlangPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize propertyLanguageName type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "langpropname"
  propertyLanguageName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps),"\\[SlangPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyLanguageName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  expect_error(SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps),"\\[SlangPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  expect_error(SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps),"\\[SlangPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("pipe",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testSlangPipe",
                                              "configurations.ini"))


  pipe <- SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- file.path("testFiles",
                    "testSlangPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("yur name")
  instance$addProperties("en","language")
  replaceSlangs <- TRUE

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  pipe$setResourcesSlangsPath(path)
  instance <- pipe$pipe(instance, replaceSlangs)

  expect_equal(instance$getSpecificProperty("langpropname"),"yur")

})

test_that("pipe data empty",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("readr")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testSlangPipe",
                                              "configurations.ini"))


  pipe <- SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- file.path("testFiles",
                    "testSlangPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("")
  instance$addProperties("en","language")
  replaceSlangs <- TRUE

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  pipe$setResourcesSlangsPath(path)

  expect_warning(pipe$pipe(instance, replaceSlangs),"\\[SlangPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testSlangPipe\\/testFile\\.tsms has data empty on pipe Slang ")

})

test_that("pipe wihtout json file",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("readr")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testSlangPipe",
                                              "configurations.ini"))

  pipe <- SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- file.path("testFiles",
                    "testSlangPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("yur name")
  instance$addProperties("en","language")
  replaceSlangs <- TRUE

  expect_warning(pipe$pipe(instance, replaceSlangs),"\\[SlangPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testSlangPipe\\/testFile\\.tsms has not an SlangsJsonFile to apply to the language-> en ")

})

test_that("pipe wihtout language property",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("readr")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testSlangPipe",
                                              "configurations.ini"))

  pipe <- SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- file.path("testFiles",
                    "testSlangPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("yur name")
  replaceSlangs <- TRUE

  expect_warning(pipe$pipe(instance, replaceSlangs),"\\[SlangPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testSlangPipe\\/testFile\\.tsms has not language property")

})

test_that("pipe Bad compatibility between Pipes.",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("readr")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testSlangPipe",
                                              "configurations.ini"))


  pipe <- SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testSlangPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  instance$setData("yur name")
  replaceSlangs <- TRUE
  expect_error(pipe$pipe(instance, replaceSlangs),"\\[SlangPipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("readr")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testSlangPipe",
                                              "configurations.ini"))

  pipe <- SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  instance <- NULL
  replaceSlangs <- TRUE

  expect_error(pipe$pipe(instance, replaceSlangs),"\\[SlangPipe\\]\\[pipe\\]\\[Error\\]
               Checking the type of the variable: instance NULL")

})

test_that("pipe replaceSlangs type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("readr")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testSlangPipe",
                                              "configurations.ini"))

  pipe <- SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testSlangPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("yur name")
  replaceSlangs <- NULL
  expect_error(pipe$pipe(instance, replaceSlangs),"\\[SlangPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: replaceSlangs NULL")

})

test_that("findSlang",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  slang <- "yur"
  data <- "yur name"

  expect_equal(pipe$findSlang(data, slang), TRUE)

})

test_that("findSlang slang type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  slang <- NULL
  data <- "yur name"

  expect_error(pipe$findSlang(data, slang),"\\[SlangPipe\\]\\[findSlang\\]\\[Error\\]
                Checking the type of the variable: slang NULL")
})

test_that("findSlang data type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  slang <- "yur"
  data <- NULL

  expect_error(pipe$findSlang(data, slang),"\\[SlangPipe\\]\\[findSlang\\]\\[Error\\]
                Checking the type of the variable: data NULL")
})

test_that("replaceSlang ",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  slang <- "yur"
  extendedSlang <- "your"
  data <- "yur name"

  expect_equal(pipe$replaceSlang(slang, extendedSlang, data)," your  name")

})

test_that("replaceSlang slang type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  slang <- NULL
  extendedSlang <- "your"
  data <- "yur name"

  expect_error(pipe$replaceSlang(slang, extendedSlang, data),"\\[SlangPipe\\]\\[replaceSlang\\]\\[Error\\]
                Checking the type of the variable: slang NULL")

})

test_that("replaceSlang extendedSlang type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  slang <- "yur"
  extendedSlang <- NULL
  data <- "yur name"

  expect_error(pipe$replaceSlang(slang, extendedSlang, data),"\\[SlangPipe\\]\\[replaceSlang\\]\\[Error\\]
                Checking the type of the variable: extendedSlang NULL")

})

test_that("replaceSlang data type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  slang <- "yur"
  extendedSlang <- "your"
  data <- NULL

  expect_error(pipe$replaceSlang(slang, extendedSlang, data),"\\[SlangPipe\\]\\[replaceSlang\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})

test_that("getPropertyLanguageName",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testSlangPipe",
                                              "configurations.ini"))

  pipe <- SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  expect_equal(pipe$getPropertyLanguageName(), "language")

})

test_that("getResourcesSlangsPath",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testSlangPipe",
                                              "configurations.ini"))

  pipe <- SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json",
                    "slang.en.json")

  pipe$setResourcesSlangsPath(path)

  expect_equal(pipe$getResourcesSlangsPath(), path)

})

test_that("setResourcesSlangsPath",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testSlangPipe",
                                              "configurations.ini"))

  pipe <- SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json",
                    "slang.en.json")

  pipe$setResourcesSlangsPath(path)

  expect_equal(pipe$getResourcesSlangsPath(), path)

})

test_that("setResourcesSlangsPath path type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testSlangPipe",
                                              "configurations.ini"))

  pipe <- SlangPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  path <- NULL

  expect_error(pipe$setResourcesSlangsPath(path),"\\[SlangPipe\\]\\[setResourcesSlangsPath\\]\\[Error\\]
                Checking the type of the variable: path NULL")

})
