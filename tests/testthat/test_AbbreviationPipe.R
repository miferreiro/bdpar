context("AbbreviationPipe")

test_that("initialize",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  expect_silent(AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps))
})

test_that("initialize propertyName type error",{

  propertyName <- NULL
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps),"\\[AbbreviationPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize propertyLanguageName type error",{

  propertyName <- "abbreviation"
  propertyLanguageName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps),"\\[AbbreviationPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyLanguageName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  expect_error(AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps),"\\[AbbreviationPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  expect_error(AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps),"\\[AbbreviationPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("pipe",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("Admin. something")
  instance$addProperties("en","language")
  replaceAbbreviations <- TRUE

  path <- system.file(file.path("testResources",
                                "abbreviations-json"),
                      package = "bdpar")

  pipe$setResourcesAbbreviationsPath(path)
  instance <- pipe$pipe(instance, replaceAbbreviations)

  expect_equal(instance$getSpecificProperty("abbreviation"),"Admin.")

})

test_that("pipe data empty",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("")
  instance$addProperties("en","language")
  replaceAbbreviations <- TRUE

  path <- system.file(file.path("testResources",
                                "abbreviations-json"),
                      package = "bdpar")

  pipe$setResourcesAbbreviationsPath(path)

  expect_output(pipe$pipe(instance, replaceAbbreviations),"\\[AbbreviationPipe\\]\\[pipe\\]\\[Warning\\]  The file:  [\\:[:alnum:]\\/_-]*testFiles\\/_ham_\\/30\\.tsms  has data empty on pipe Abbreviation  ")

})

test_that("pipe wihtout json file",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("D. something")
  instance$addProperties("en","language")
  replaceAbbreviations <- TRUE

  expect_output(pipe$pipe(instance, replaceAbbreviations),"\\[AbbreviationPipe\\]\\[pipe\\]\\[Warning\\]  The file:  [\\:[:alnum:]\\/_-]*testFiles\\/_ham_\\/30\\.tsms  has not an abbreviationsJsonFile  to apply to the language -> en  ")

})

test_that("pipe wihtout language property",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("D. something")
  replaceAbbreviations <- TRUE

  expect_output(pipe$pipe(instance, replaceAbbreviations),"\\[AbbreviationPipe\\]\\[pipe\\]\\[Warning\\]  The file:  [\\:[:alnum:]\\/_-]*testFiles\\/_ham_\\/30\\.tsms  has not language property")

})

test_that("pipe Bad compatibility between Pipes.",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  instance$setData("D. something")
  replaceAbbreviations <- TRUE
  expect_error(pipe$pipe(instance, replaceAbbreviations),"\\[AbbreviationPipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  instance <- NULL
  replaceAbbreviations <- TRUE

  expect_error(pipe$pipe(instance, replaceAbbreviations),"\\[AbbreviationPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("pipe replaceAbbreviations type error",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))
  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("D. something")
  replaceAbbreviations <- NULL
  expect_error(pipe$pipe(instance, replaceAbbreviations),"\\[AbbreviationPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: replaceAbbreviations NULL")

})




test_that("findAbbreviation",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  abbreviation <- "D."
  data <- "D. something"

  expect_equal(pipe$findAbbreviation(data, abbreviation), TRUE)

})

test_that("findAbbreviation abbreviation type error",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  abbreviation <- NULL
  data <- "D. something"

  expect_error(pipe$findAbbreviation(data, abbreviation),"\\[AbbreviationPipe\\]\\[findAbbreviation\\]\\[Error\\]
                Checking the type of the variable: abbreviation NULL")
})

test_that("findAbbreviation data type error",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  abbreviation <- "D."
  data <- NULL

  expect_error(pipe$findAbbreviation(data, abbreviation),"\\[AbbreviationPipe\\]\\[findAbbreviation\\]\\[Error\\]
                Checking the type of the variable: data NULL")
})

test_that("replaceAbbreviation ",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  abbreviation <- "D."
  extendedAbbreviation <- "Don"
  data <- "D. something"

  expect_equal(pipe$replaceAbbreviation(abbreviation, extendedAbbreviation, data)," Don  something")

})

test_that("replaceAbbreviation abbreviation type error",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  abbreviation <- NULL
  extendedAbbreviation <- "Don"
  data <- "D. something"

  expect_error(pipe$replaceAbbreviation(abbreviation, extendedAbbreviation, data),"\\[AbbreviationPipe\\]\\[replaceAbbreviation\\]\\[Error\\]
                Checking the type of the variable: abbreviation NULL")

})

test_that("replaceAbbreviation extendedAbbreviation type error",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  abbreviation <- "D."
  extendedAbbreviation <- NULL
  data <- "D. something"

  expect_error(pipe$replaceAbbreviation(abbreviation, extendedAbbreviation, data),"\\[AbbreviationPipe\\]\\[replaceAbbreviation\\]\\[Error\\]
                Checking the type of the variable: extendedAbbreviation NULL")

})

test_that("replaceAbbreviation data type error",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  abbreviation <- "D."
  extendedAbbreviation <- "Don"
  data <- NULL

  expect_error(pipe$replaceAbbreviation(abbreviation, extendedAbbreviation, data),"\\[AbbreviationPipe\\]\\[replaceAbbreviation\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})

test_that("getPropertyLanguageName",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  expect_equal(pipe$getPropertyLanguageName(), "language")

})

test_that("getResourcesAbbreviationsPath",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testResources",
                                        "abbreviations-json",
                                        "abbrev.en.json"),
                              package = "bdpar")

  pipe$setResourcesAbbreviationsPath(path)

  expect_equal(pipe$getResourcesAbbreviationsPath(), path)

})

test_that("setResourcesAbbreviationsPath",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testResources",
                                "abbreviations-json",
                                "abbrev.en.json"),
                      package = "bdpar")

  pipe$setResourcesAbbreviationsPath(path)

  expect_equal(pipe$getResourcesAbbreviationsPath(), path)

})

test_that("setResourcesAbbreviationsPath path type error",{

  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- AbbreviationPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- NULL

  expect_error(pipe$setResourcesAbbreviationsPath(path),"\\[AbbreviationPipe\\]\\[setResourcesAbbreviationsPath\\]\\[Error\\]
                Checking the type of the variable: path NULL")

})
