context("AbbreviationPipe")

test_that("initialize",{
  skip_if_not_installed("rjson")
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testAbbreviationPipe",
                                              "configurations.ini"))

  expect_silent(AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps))
})

test_that("initialize propertyName type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- NULL
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps), "\\[AbbreviationPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize propertyLanguageName type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps), "\\[AbbreviationPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyLanguageName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  expect_error(AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps), "\\[AbbreviationPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  expect_error(AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps), "\\[AbbreviationPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")
})

test_that("pipe",{
  skip_if_not_installed("rjson")
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testAbbreviationPipe",
                                              "configurations.ini"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- file.path("testFiles",
                    "testAbbreviationPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Admin. something")
  instance$addProperties("en","language")
  replaceAbbreviations <- TRUE

  path <- file.path("resourcesFiles",
                    "testResources",
                    "abbreviations-json")

  pipe$setResourcesAbbreviationsPath(path)
  instance <- pipe$pipe(instance, replaceAbbreviations)

  expect_equal(instance$getSpecificProperty("abbreviation"), "Admin.")

})

test_that("pipe data empty",{
  skip_if_not_installed("rjson")
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testAbbreviationPipe",
                                              "configurations.ini"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- file.path("testFiles",
                    "testAbbreviationPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("")
  instance$addProperties("en","language")
  replaceAbbreviations <- TRUE

  path <- file.path("resourcesFiles",
                    "testResources",
                    "abbreviations-json")

  pipe$setResourcesAbbreviationsPath(path)

  expect_warning(pipe$pipe(instance, replaceAbbreviations),"\\[AbbreviationPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testAbbreviationPipe\\/testFile\\.tsms has data empty on pipe Abbreviation ")

})

test_that("pipe wihtout json file",{
  skip_if_not_installed("rjson")
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testAbbreviationPipe",
                                              "configurations.ini"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- file.path("testFiles",
                    "testAbbreviationPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("D. something")
  instance$addProperties("en","language")
  replaceAbbreviations <- TRUE

  expect_warning(pipe$pipe(instance, replaceAbbreviations),"\\[AbbreviationPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testAbbreviationPipe\\/testFile\\.tsms has not an abbreviationsJsonFile to apply to the language ->en ")

})

test_that("pipe wihtout language property",{
  skip_if_not_installed("rjson")
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testAbbreviationPipe",
                                              "configurations.ini"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- file.path("testFiles",
                    "testAbbreviationPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("D. something")
  replaceAbbreviations <- TRUE

  expect_warning(pipe$pipe(instance, replaceAbbreviations),"\\[AbbreviationPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testAbbreviationPipe\\/testFile\\.tsms has not language property")

})

test_that("pipe Bad compatibility between Pipes.",{
  skip_if_not_installed("rjson")
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testAbbreviationPipe",
                                              "configurations.ini"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- file.path("testFiles",
                    "testAbbreviationPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  instance$setData("D. something")
  replaceAbbreviations <- TRUE
  expect_error(pipe$pipe(instance, replaceAbbreviations),"\\[AbbreviationPipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{
  skip_if_not_installed("rjson")
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testAbbreviationPipe",
                                              "configurations.ini"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  instance <- NULL
  replaceAbbreviations <- TRUE

  expect_error(pipe$pipe(instance, replaceAbbreviations),"\\[AbbreviationPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("pipe replaceAbbreviations type error",{
  skip_if_not_installed("rjson")
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testAbbreviationPipe",
                                              "configurations.ini"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- file.path("testFiles",
                    "testAbbreviationPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("D. something")
  replaceAbbreviations <- NULL
  expect_error(pipe$pipe(instance, replaceAbbreviations),"\\[AbbreviationPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: replaceAbbreviations NULL")

})

test_that("findAbbreviation",{
  skip_if_not_installed("rex")
  skip_if_not_installed("rjson")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testAbbreviationPipe",
                                              "configurations.ini"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  abbreviation <- "D."
  data <- "D. something"

  expect_equal(pipe$findAbbreviation(data, abbreviation), TRUE)

})

test_that("findAbbreviation abbreviation type error",{
  skip_if_not_installed("rjson")
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testAbbreviationPipe",
                                              "configurations.ini"))
  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  abbreviation <- NULL
  data <- "D. something"

  expect_error(pipe$findAbbreviation(data, abbreviation),"\\[AbbreviationPipe\\]\\[findAbbreviation\\]\\[Error\\]
                Checking the type of the variable: abbreviation NULL")

})

test_that("findAbbreviation data type error",{
  skip_if_not_installed("rjson")
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testAbbreviationPipe",
                                              "configurations.ini"))
  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  abbreviation <- "D."
  data <- NULL

  expect_error(pipe$findAbbreviation(data, abbreviation),"\\[AbbreviationPipe\\]\\[findAbbreviation\\]\\[Error\\]
                Checking the type of the variable: data NULL")
})

test_that("replaceAbbreviation ",{
  skip_if_not_installed("rjson")
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testAbbreviationPipe",
                                              "configurations.ini"))
  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  abbreviation <- "D."
  extendedAbbreviation <- "Don"
  data <- "D. something"

  expect_equal(pipe$replaceAbbreviation(abbreviation, extendedAbbreviation, data)," Don  something")

})

test_that("replaceAbbreviation abbreviation type error",{
  skip_if_not_installed("rjson")
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testAbbreviationPipe",
                                              "configurations.ini"))
  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  abbreviation <- NULL
  extendedAbbreviation <- "Don"
  data <- "D. something"

  expect_error(pipe$replaceAbbreviation(abbreviation, extendedAbbreviation, data),"\\[AbbreviationPipe\\]\\[replaceAbbreviation\\]\\[Error\\]
                Checking the type of the variable: abbreviation NULL")

})

test_that("replaceAbbreviation extendedAbbreviation type error",{
  skip_if_not_installed("rjson")
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testAbbreviationPipe",
                                              "configurations.ini"))
  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  abbreviation <- "D."
  extendedAbbreviation <- NULL
  data <- "D. something"

  expect_error(pipe$replaceAbbreviation(abbreviation, extendedAbbreviation, data),"\\[AbbreviationPipe\\]\\[replaceAbbreviation\\]\\[Error\\]
                Checking the type of the variable: extendedAbbreviation NULL")

})

test_that("replaceAbbreviation data type error",{
  skip_if_not_installed("rjson")
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testAbbreviationPipe",
                                              "configurations.ini"))
  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  abbreviation <- "D."
  extendedAbbreviation <- "Don"
  data <- NULL

  expect_error(pipe$replaceAbbreviation(abbreviation, extendedAbbreviation, data),"\\[AbbreviationPipe\\]\\[replaceAbbreviation\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})

test_that("getPropertyLanguageName",{
  skip_if_not_installed("rjson")
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testAbbreviationPipe",
                                              "configurations.ini"))
  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  expect_equal(pipe$getPropertyLanguageName(), "language")

})

test_that("getResourcesAbbreviationsPath",{
  skip_if_not_installed("rjson")
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testAbbreviationPipe",
                                              "configurations.ini"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- file.path("resourcesFiles",
                    "testResources",
                    "abbreviations-json",
                    "abbrev.en.json")

  pipe$setResourcesAbbreviationsPath(path)

  expect_equal(pipe$getResourcesAbbreviationsPath(), path)

})

test_that("setResourcesAbbreviationsPath",{
  skip_if_not_installed("rjson")
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testAbbreviationPipe",
                                              "configurations.ini"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- file.path("resourcesFiles",
                    "testResources",
                    "abbreviations-json",
                    "abbrev.en.json")

  pipe$setResourcesAbbreviationsPath(path)

  expect_equal(pipe$getResourcesAbbreviationsPath(), path)

})

test_that("setResourcesAbbreviationsPath path type error",{
  skip_if_not_installed("rjson")
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testAbbreviationPipe",
                                              "configurations.ini"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- NULL

  expect_error(pipe$setResourcesAbbreviationsPath(path),"\\[AbbreviationPipe\\]\\[setResourcesAbbreviationsPath\\]\\[Error\\]
                Checking the type of the variable: path NULL")

})
