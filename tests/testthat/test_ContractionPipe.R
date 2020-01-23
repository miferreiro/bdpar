context("ContractionPipe")

test_that("initialize",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testContractionPipe",
                                              "configurations.ini"))

  expect_silent(ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps, replaceContractions))
})

test_that("initialize propertyName type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- NULL
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE

  expect_error(ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps, replaceContractions),"\\[ContractionPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize propertyLanguageName type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE

  expect_error(ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps, replaceContractions),"\\[ContractionPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyLanguageName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  replaceContractions <- TRUE

  expect_error(ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, replaceContractions),"\\[ContractionPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  replaceContractions <- TRUE

  expect_error(ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, replaceContractions),"\\[ContractionPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("initialize replaceContractions type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- NULL

  expect_error(ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, replaceContractions),"\\[ContractionPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: replaceContractions NULL")

})

test_that("pipe",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testContractionPipe",
                                              "configurations.ini"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps , notAfterDeps, replaceContractions)

  path <- file.path("testFiles",
                    "testContractionPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("I'm tall")
  instance$addProperties("en","language")

  path <- file.path("resourcesFiles",
                    "testResources",
                    "contractions-json")

  pipe$setResourcesContractionsPath(path)
  instance <- pipe$pipe(instance)

  expect_equal(instance$getSpecificProperty("contractions"),"I'm")

})

test_that("pipe data empty",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testContractionPipe",
                                              "configurations.ini"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, replaceContractions)

  path <- file.path("testFiles",
                    "testContractionPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("")
  instance$addProperties("en","language")

  path <- file.path("resourcesFiles",
                    "testResources",
                    "contractions-json")

  pipe$setResourcesContractionsPath(path)

  expect_warning(pipe$pipe(instance),"\\[ContractionPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testContractionPipe\\/testFile\\.tsms has data empty on pipe Contractions ")

})

test_that("pipe wihtout json file",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testContractionPipe",
                                              "configurations.ini"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, replaceContractions)

  path <- file.path("testFiles",
                    "testContractionPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("I'm tall")
  instance$addProperties("en","language")

  expect_warning(pipe$pipe(instance),"\\[ContractionPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testContractionPipe\\/testFile\\.tsms has not an contractionsJsonFile to apply to the language ->en ")

})

test_that("pipe wihtout language property",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testContractionPipe",
                                              "configurations.ini"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, replaceContractions)

  path <- file.path("testFiles",
                    "testContractionPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("I'm tall")

  expect_warning(pipe$pipe(instance),"\\[ContractionPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testContractionPipe\\/testFile\\.tsms has not language property")

})

test_that("pipe Bad compatibility between Pipes.",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  replaceContractions <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testContractionPipe",
                                              "configurations.ini"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, replaceContractions)

  path <- file.path("testFiles",
                    "testContractionPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  instance$setData("I'm tall")

  expect_error(pipe$pipe(instance),"\\[ContractionPipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testContractionPipe",
                                              "configurations.ini"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, replaceContractions)

  instance <- NULL

  expect_error(pipe$pipe(instance),"\\[ContractionPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("findContraction",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps, replaceContractions)

  contraction <- "I'm"
  data <- "I'm tall"

  expect_equal(pipe$findContraction(data, contraction), TRUE)

})

test_that("findContraction abbreviation type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps, replaceContractions)

  contraction <- NULL
  data <- "I'm tall"

  expect_error(pipe$findContraction(data, contraction),"\\[ContractionPipe\\]\\[findContraction\\]\\[Error\\]
                Checking the type of the variable: contraction NULL")
})

test_that("findContraction data type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps, replaceContractions)

  contraction <- "I'm"
  data <- NULL

  expect_error(pipe$findContraction(data, contraction),"\\[ContractionPipe\\]\\[findContraction\\]\\[Error\\]
                Checking the type of the variable: data NULL")
})

test_that("replaceContraction ",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps, replaceContractions)

  contraction <- "I'm"
  extendedContraction <- "I am"
  data <- "I'm tall"

  expect_equal(pipe$replaceContraction(contraction, extendedContraction, data)," I am  tall")

})

test_that("replaceContraction abbreviation type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps, replaceContractions)

  contraction <- NULL
  extendedContraction <- "I am"
  data <- "I'm tall"

  expect_error(pipe$replaceContraction(contraction, extendedContraction, data),"\\[ContractionPipe\\]\\[replaceContraction\\]\\[Error\\]
                Checking the type of the variable: contraction NULL")

})

test_that("replaceContraction extendedContraction type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps, replaceContractions)

  contraction <- "I'm"
  extendedContraction <- NULL
  data <- "I'm tall"

  expect_error(pipe$replaceContraction(contraction, extendedContraction, data),"\\[ContractionPipe\\]\\[replaceContraction\\]\\[Error\\]
                Checking the type of the variable: extendedContraction NULL")

})

test_that("replaceContraction data type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps, replaceContractions)

  contraction <- "I'm"
  extendedContraction <- "I am"
  data <- NULL

  expect_error(pipe$replaceContraction(contraction, extendedContraction, data),"\\[ContractionPipe\\]\\[replaceContraction\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})

test_that("getPropertyLanguageName",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testContractionPipe",
                                              "configurations.ini"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, replaceContractions)

  expect_equal(pipe$getPropertyLanguageName(), "language")

})

test_that("getResourcesContractionsPath",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testContractionPipe",
                                              "configurations.ini"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, replaceContractions)

  path <- file.path("resourcesFiles",
                    "testResources",
                    "contractions-json",
                    "abbrev.en.json")

  pipe$setResourcesContractionsPath(path)

  expect_equal(pipe$getResourcesContractionsPath(), path)

})

test_that("setResourcesContractionsPath",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testContractionPipe",
                                              "configurations.ini"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, replaceContractions)

  path <- file.path("resourcesFiles",
                    "testResources",
                    "contractions-json",
                    "abbrev.en.json")

  pipe$setResourcesContractionsPath(path)

  expect_equal(pipe$getResourcesContractionsPath(), path)

})

test_that("setResourcesContractionsPath path type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testContractionPipe",
                                              "configurations.ini"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps, replaceContractions)

  path <- NULL

  expect_error(pipe$setResourcesContractionsPath(path),"\\[ContractionPipe\\]\\[setResourcesContractionsPath\\]\\[Error\\]
                Checking the type of the variable: path NULL")

})
