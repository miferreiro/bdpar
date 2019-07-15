context("StopWordPipe")

test_that("initialize",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  expect_silent(StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps))
})

test_that("initialize propertyName type error",{

  propertyName <- NULL
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps),"\\[StopWordPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize propertyLanguageName type error",{

  propertyName <- "stopWord"
  propertyLanguageName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps),"\\[StopWordPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyLanguageName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  expect_error(StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps),"\\[StopWordPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  expect_error(StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps),"\\[StopWordPipe\\]\\[initialize\\]\\[Error\\]
                 Checking the type of the variable: notAfterDeps NULL")

})

test_that("pipe",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("you want an apple")
  instance$addProperties("en","language")
  removeStopWords <- TRUE

  path <- system.file(file.path("testResources",
                                "stopwords-json"),
                      package = "bdpar")

  pipe$setResourcesStopWordsPath(path)
  instance <- pipe$pipe(instance, removeStopWords)

  expect_equal(instance$getSpecificProperty("stopWord"),c("an","want","you"))

})

test_that("pipe data empty",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("")
  instance$addProperties("en","language")
  removeStopWords <- TRUE

  path <- system.file(file.path("testResources",
                                "stopwords-json"),
                      package = "bdpar")

  pipe$setResourcesStopWordsPath(path)

  expect_warning(pipe$pipe(instance, removeStopWords),"\\[StopWordPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/_ham_\\/30\\.tsms has data empty on pipe StopWord ")

})

test_that("pipe wihtout json file",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("you want an apple")
  instance$addProperties("en","language")
  removeStopWords <- TRUE

  expect_warning(pipe$pipe(instance, removeStopWords),"\\[StopWordPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/_ham_\\/30\\.tsms has not an StopWordsJsonFile to apply to the language-> en ")

})

test_that("pipe wihtout language property",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("you want an apple")
  removeStopWords <- TRUE

  expect_warning(pipe$pipe(instance, removeStopWords),"\\[StopWordPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/_ham_\\/30\\.tsms has not language property ")

})

test_that("pipe Bad compatibility between Pipes.",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  instance$setData("you want an apple")
  removeStopWords <- TRUE
  expect_error(pipe$pipe(instance, removeStopWords),"\\[StopWordPipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  instance <- NULL
  removeStopWords <- TRUE

  expect_error(pipe$pipe(instance, removeStopWords),"\\[StopWordPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("pipe removeStopWords type error",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("you want an apple")
  removeStopWords <- NULL
  expect_error(pipe$pipe(instance, removeStopWords),"\\[StopWordPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: removeStopWords NULL")

})

test_that("findStopWord",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  stopWord <- "you"
  data <- "you want an apple"

  expect_equal(pipe$findStopWord(data, stopWord), TRUE)

})

test_that("findStopWord stopWord type error",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  stopWord <- NULL
  data <- "you want an apple"

  expect_error(pipe$findStopWord(data, stopWord),"\\[StopWordPipe\\]\\[findStopWord]\\[Error\\]
                Checking the type of the variable: stopWord NULL")
})

test_that("findStopWord data type error",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  stopWord <- "you"
  data <- NULL

  expect_error(pipe$findStopWord(data, stopWord),"\\[StopWordPipe\\]\\[findStopWord\\]\\[Error\\]
                Checking the type of the variable: data NULL")
})

test_that("removeStopWord ",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  stopWord <- "you"
  data <- "you want an apple"

  expect_equal(pipe$removeStopWord(stopWord, data)," want an apple")

})

test_that("removeStopWord stopWord type error",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  stopWord <- NULL
  data <- "you want an apple"

  expect_error(pipe$removeStopWord(stopWord, data),"\\[StopWordPipe\\]\\[removeStopWord\\]\\[Error\\]
                Checking the type of the variable: stopWord NULL")

})

test_that("removeStopWord data type error",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps, notAfterDeps)

  stopWord <- "you"
  data <- NULL

  expect_error(pipe$removeStopWord(stopWord, data),"\\[StopWordPipe\\]\\[removeStopWord\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})

test_that("getPropertyLanguageName",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  expect_equal(pipe$getPropertyLanguageName(), "language")

})

test_that("getResourcesStopWordsPath",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testResources",
                                "stopwords-json",
                                "en.json"),
                      package = "bdpar")

  pipe$setResourcesStopWordsPath(path)

  expect_equal(pipe$getResourcesStopWordsPath(), path)

})

test_that("setResourcesStopWordsPath",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- system.file(file.path("testResources",
                                "stopwords-json",
                                "en.json"),
                      package = "bdpar")

  pipe$setResourcesStopWordsPath(path)

  expect_equal(pipe$getResourcesStopWordsPath(), path)

})

test_that("setResourcesStopWordsPath path type error",{

  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  pipe <- StopWordPipe$new(propertyName, propertyLanguageName, alwaysBeforeDeps ,notAfterDeps)

  path <- NULL

  expect_error(pipe$setResourcesStopWordsPath(path),"\\[StopWordPipe\\]\\[setResourcesStopWordsPath\\]\\[Error\\]
                Checking the type of the variable: path NULL")

})
