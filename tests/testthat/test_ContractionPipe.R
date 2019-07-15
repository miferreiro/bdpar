context("ContractionPipe")

test_that("initialize",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  expect_silent(ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps))
})

test_that("initialize propertyName type error",{

  propertyName <- NULL
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps),"\\[ContractionPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize propertyLanguageName type error",{

  propertyName <- "contractions"
  propertyLanguageName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps),"\\[ContractionPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyLanguageName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  expect_error(ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps),"\\[ContractionPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  expect_error(ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps),"\\[ContractionPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("pipe",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("I'm tall")
  instance$addProperties("en","language")
  replaceContractions <- TRUE

  path <- system.file(file.path("testResources",
                                "contractions-json"),
                      package = "bdpar")

  pipe$setResourcesContractionsPath(path)
  instance <- pipe$pipe(instance, replaceContractions)

  expect_equal(instance$getSpecificProperty("contractions"),"I'm")

})

test_that("pipe data empty",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("")
  instance$addProperties("en","language")
  replaceContractions <- TRUE

  path <- system.file(file.path("testResources",
                                "contractions-json"),
                      package = "bdpar")

  pipe$setResourcesContractionsPath(path)

  expect_warning(pipe$pipe(instance, replaceContractions),"\\[ContractionPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/_ham_\\/30\\.tsms has data empty on pipe Contractions ")

})

test_that("pipe wihtout json file",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("I'm tall")
  instance$addProperties("en","language")
  replaceContractions <- TRUE

  expect_warning(pipe$pipe(instance, replaceContractions),"\\[ContractionPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/_ham_\\/30\\.tsms has not an contractionsJsonFile to apply to the language ->en ")

})

test_that("pipe wihtout language property",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("I'm tall")
  replaceContractions <- TRUE

  expect_warning(pipe$pipe(instance, replaceContractions),"\\[ContractionPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/_ham_\\/30\\.tsms has not language property")

})

test_that("pipe Bad compatibility between Pipes.",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  instance$setData("I'm tall")
  replaceContractions <- TRUE
  expect_error(pipe$pipe(instance, replaceContractions),"\\[ContractionPipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  instance <- NULL
  replaceContractions <- TRUE

  expect_error(pipe$pipe(instance, replaceContractions),"\\[ContractionPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("pipe replaceContractions type error",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))
  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("I'm tall")
  replaceContractions <- NULL
  expect_error(pipe$pipe(instance, replaceContractions),"\\[ContractionPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: replaceContractions NULL")

})




test_that("findContraction",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  contraction <- "I'm"
  data <- "I'm tall"

  expect_equal(pipe$findContraction(data, contraction), TRUE)

})

test_that("findContraction abbreviation type error",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  contraction <- NULL
  data <- "I'm tall"

  expect_error(pipe$findContraction(data, contraction),"\\[ContractionPipe\\]\\[findContraction\\]\\[Error\\]
                Checking the type of the variable: contraction NULL")
})

test_that("findContraction data type error",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  contraction <- "I'm"
  data <- NULL

  expect_error(pipe$findContraction(data, contraction),"\\[ContractionPipe\\]\\[findContraction\\]\\[Error\\]
                Checking the type of the variable: data NULL")
})

test_that("replaceContraction ",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  contraction <- "I'm"
  extendedContraction <- "I am"
  data <- "I'm tall"

  expect_equal(pipe$replaceContraction(contraction, extendedContraction, data)," I am  tall")

})

test_that("replaceContraction abbreviation type error",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  contraction <- NULL
  extendedContraction <- "I am"
  data <- "I'm tall"

  expect_error(pipe$replaceContraction(contraction, extendedContraction, data),"\\[ContractionPipe\\]\\[replaceContraction\\]\\[Error\\]
                Checking the type of the variable: contraction NULL")

})

test_that("replaceContraction extendedContraction type error",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  contraction <- "I'm"
  extendedContraction <- NULL
  data <- "I'm tall"

  expect_error(pipe$replaceContraction(contraction, extendedContraction, data),"\\[ContractionPipe\\]\\[replaceContraction\\]\\[Error\\]
                Checking the type of the variable: extendedContraction NULL")

})

test_that("replaceContraction data type error",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  contraction <- "I'm"
  extendedContraction <- "I am"
  data <- NULL

  expect_error(pipe$replaceContraction(contraction, extendedContraction, data),"\\[ContractionPipe\\]\\[replaceContraction\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})

test_that("getPropertyLanguageName",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  expect_equal(pipe$getPropertyLanguageName(), "language")

})

test_that("getResourcesContractionsPath",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testResources",
                                "contractions-json",
                                "contr.en.json"),
                      package = "bdpar")

  pipe$setResourcesContractionsPath(path)

  expect_equal(pipe$getResourcesContractionsPath(), path)

})

test_that("setResourcesContractionsPath",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testResources",
                                "contractions-json",
                                "abbrev.en.json"),
                      package = "bdpar")

  pipe$setResourcesContractionsPath(path)

  expect_equal(pipe$getResourcesContractionsPath(), path)

})

test_that("setResourcesContractionsPath path type error",{

  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- ContractionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- NULL

  expect_error(pipe$setResourcesContractionsPath(path),"\\[ContractionPipe\\]\\[setResourcesContractionsPath\\]\\[Error\\]
                Checking the type of the variable: path NULL")

})
