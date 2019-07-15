context("InterjectionPipe")

test_that("initialize",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  expect_silent(InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps))
})

test_that("initialize propertyName type error",{

  propertyName <- NULL
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps),"\\[InterjectionPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize propertyLanguageName type error",{

  propertyName <- "interjection"
  propertyLanguageName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps),"\\[InterjectionPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyLanguageName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  expect_error(InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps),"\\[InterjectionPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  expect_error(InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps),"\\[InterjectionPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("pipe",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("yeah I like it")
  instance$addProperties("en","language")
  removeInterjections <- TRUE

  path <- system.file(file.path("testResources",
                                "interjections-json"),
                      package = "bdpar")

  pipe$setResourcesInterjectionsPath(path)
  instance <- pipe$pipe(instance, removeInterjections)

  expect_equal(instance$getSpecificProperty("interjection"),c("yeah","like"))

})

test_that("pipe data empty",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("")
  instance$addProperties("en","language")
  removeInterjections <- TRUE

  path <- system.file(file.path("testResources",
                                "interjections-json"),
                      package = "bdpar")

  pipe$setResourcesInterjectionsPath(path)

  expect_warning(pipe$pipe(instance, removeInterjections),"\\[InterjectionPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/_ham_\\/30\\.tsms has data empty on pipe Interjection ")

})

test_that("pipe wihtout json file",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("yeah I like it")
  instance$addProperties("en","language")
  removeInterjections <- TRUE

  expect_warning(pipe$pipe(instance, removeInterjections),"\\[InterjectionPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/_ham_\\/30\\.tsms has not an interjectionsJsonFile to apply to the language ->en ")

})

test_that("pipe wihtout language property",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("yeah I like it")
  removeInterjections <- TRUE

  expect_warning(pipe$pipe(instance, removeInterjections),"\\[InterjectionPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/_ham_\\/30\\.tsms has not language property")

})

test_that("pipe Bad compatibility between Pipes.",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  instance$setData("yeah I like it")
  removeInterjections <- TRUE
  expect_error(pipe$pipe(instance, removeInterjections),"\\[InterjectionPipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  instance <- NULL
  removeInterjections <- TRUE

  expect_error(pipe$pipe(instance, removeInterjections),"\\[InterjectionPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("pipe removeInterjections type error",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("yeah I like it")
  removeInterjections <- NULL
  expect_error(pipe$pipe(instance, removeInterjections),"\\[InterjectionPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: removeInterjections NULL")

})

test_that("findInterjection",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  interjection <- "yeah"
  data <- "yeah I like it"

  expect_equal(pipe$findInterjection(data, interjection), TRUE)

})

test_that("findInterjection interjection type error",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  interjection <- NULL
  data <- "yeah I like it"

  expect_error(pipe$findInterjection(data, interjection),"\\[InterjectionPipe\\]\\[findInterjection]\\[Error\\]
                Checking the type of the variable: interjection NULL")
})

test_that("findInterjection data type error",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  interjection <- "yeah"
  data <- NULL

  expect_error(pipe$findInterjection(data, interjection),"\\[InterjectionPipe\\]\\[findInterjection\\]\\[Error\\]
                Checking the type of the variable: data NULL")
})

test_that("removeInterjection ",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  interjection <- "yeah"
  data <- "yeah I like it"

  expect_equal(pipe$removeInterjection(interjection, data)," I like it")

})

test_that("removeInterjection abbreviation type error",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  interjection <- NULL
  data <- "yeah I like it"

  expect_error(pipe$removeInterjection(interjection, data),"\\[InterjectionPipe\\]\\[removeInterjection\\]\\[Error\\]
                Checking the type of the variable: interjection NULL")

})

test_that("removeInterjection data type error",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  interjection <- "yeah"
  data <- NULL

  expect_error(pipe$removeInterjection(interjection, data),"\\[InterjectionPipe\\]\\[removeInterjection\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})

test_that("getPropertyLanguageName",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  expect_equal(pipe$getPropertyLanguageName(), "language")

})

test_that("getResourcesInterjectionsPath",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testResources",
                                "interjections-json",
                                "interj.en.json"),
                      package = "bdpar")

  pipe$setResourcesInterjectionsPath(path)

  expect_equal(pipe$getResourcesInterjectionsPath(), path)

})

test_that("setResourcesInterjectionsPath",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testResources",
                                "interjections-json",
                                "interj.en.json"),
                      package = "bdpar")

  pipe$setResourcesInterjectionsPath(path)

  expect_equal(pipe$getResourcesInterjectionsPath(), path)

})

test_that("setResourcesInterjectionsPath path type error",{

  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- InterjectionPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- NULL

  expect_error(pipe$setResourcesInterjectionsPath(path),"\\[InterjectionPipe\\]\\[setResourcesInterjectionsPath\\]\\[Error\\]
                Checking the type of the variable: path NULL")

})
