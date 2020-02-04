testthat::context("StopWordPipe")

testthat::setup(bdpar.Options$reset())

testthat::test_that("initialize",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  testthat::expect_silent(StopWordPipe$new(propertyName,
                                           propertyLanguageName,
                                           alwaysBeforeDeps,
                                           notAfterDeps,
                                           removeStopWords,
                                           resourcesStopWordsPath))

  resourcesStopWordsPath <- path

  testthat::expect_silent(StopWordPipe$new(propertyName,
                                           propertyLanguageName,
                                           alwaysBeforeDeps,
                                           notAfterDeps,
                                           removeStopWords,
                                           resourcesStopWordsPath))
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("initialize propertyName type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- NULL
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  testthat::expect_error(StopWordPipe$new(propertyName,
                                          propertyLanguageName,
                                          alwaysBeforeDeps,
                                          notAfterDeps,
                                          removeStopWords,
                                          resourcesStopWordsPath),
                         "[StopWordPipe][initialize][Error] Checking the type of the 'propertyName' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("initialize propertyLanguageName type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  testthat::expect_error(StopWordPipe$new(propertyName,
                                          propertyLanguageName,
                                          alwaysBeforeDeps,
                                          notAfterDeps,
                                          removeStopWords,
                                          resourcesStopWordsPath),
                         "[StopWordPipe][initialize][Error] Checking the type of the 'propertyLanguageName' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("initialize alwaysBeforeDeps type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  testthat::expect_error(StopWordPipe$new(propertyName,
                                          propertyLanguageName,
                                          alwaysBeforeDeps,
                                          notAfterDeps,
                                          removeStopWords,
                                          resourcesStopWordsPath),
                         "[StopWordPipe][initialize][Error] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("initialize notAfterDeps type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  testthat::expect_error(StopWordPipe$new(propertyName,
                                          propertyLanguageName,
                                          alwaysBeforeDeps,
                                          notAfterDeps,
                                          removeStopWords,
                                          resourcesStopWordsPath),
                         "[StopWordPipe][initialize][Error] Checking the type of the 'notAfterDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("initialize removeStopWords type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- NULL
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  testthat::expect_error(StopWordPipe$new(propertyName,
                                          propertyLanguageName,
                                          alwaysBeforeDeps,
                                          notAfterDeps,
                                          removeStopWords,
                                          resourcesStopWordsPath),
                         "[StopWordPipe][initialize][Error] Checking the type of the 'removeStopWords' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("initialize resourcesStopWordsPath type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = NULL)

  testthat::expect_error(StopWordPipe$new(propertyName,
                                          propertyLanguageName,
                                          alwaysBeforeDeps,
                                          notAfterDeps,
                                          removeStopWords,
                                          resourcesStopWordsPath),
                         "[StopWordPipe][initialize][Error] Path of stop words resources is neither defined in initialize or in bdpar.Options",
                         fixed = TRUE)

  resourcesStopWordsPath <- 1

  testthat::expect_error(StopWordPipe$new(propertyName,
                                          propertyLanguageName,
                                          alwaysBeforeDeps,
                                          notAfterDeps,
                                          removeStopWords,
                                          resourcesStopWordsPath),
                         "[StopWordPipe][initialize][Error] Checking the type of the 'resourcesStopWordsPath' variable: numeric",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("pipe",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  Bdpar$new()

  pipe <- StopWordPipe$new(propertyName,
                           propertyLanguageName,
                           alwaysBeforeDeps,
                           notAfterDeps,
                           removeStopWords,
                           resourcesStopWordsPath)

  path <- file.path("testFiles",
                    "testStopWordPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("you want an apple")
  instance$addProperties("en","language")

  instance <- pipe$pipe(instance)
  testthat::expect_equal(instance$getSpecificProperty("stopWord"),
                         c("an","want","you"))
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("pipe data empty",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  Bdpar$new()

  pipe <- StopWordPipe$new(propertyName,
                           propertyLanguageName,
                           alwaysBeforeDeps,
                           notAfterDeps,
                           removeStopWords,
                           resourcesStopWordsPath)

  path <- file.path("testFiles",
                    "testStopWordPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("")
  instance$addProperties("en","language")

  testthat::expect_warning(pipe$pipe(instance),
                           "\\[StopWordPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testStopWordPipe\\/testFile\\.tsms has data empty on pipe StopWord")
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("pipe wihtout json file",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-wrong")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  Bdpar$new()

  pipe <- StopWordPipe$new(propertyName,
                           propertyLanguageName,
                           alwaysBeforeDeps,
                           notAfterDeps,
                           removeStopWords,
                           resourcesStopWordsPath)

  path <- file.path("testFiles",
                    "testStopWordPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("you want an apple")
  instance$addProperties("en","language")

  testthat::expect_warning(pipe$pipe(instance),
                           "\\[StopWordPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testStopWordPipe\\/testFile\\.tsms has not an StopWordsJsonFile to apply to the language-> en")
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("pipe wihtout language property",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  Bdpar$new()

  pipe <- StopWordPipe$new(propertyName,
                           propertyLanguageName,
                           alwaysBeforeDeps,
                           notAfterDeps,
                           removeStopWords,
                           resourcesStopWordsPath)

  path <- file.path("testFiles",
                    "testStopWordPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("you want an apple")

  testthat::expect_warning(pipe$pipe(instance),
                           "\\[StopWordPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testStopWordPipe\\/testFile\\.tsms has not language property")
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("pipe Bad compatibility between Pipes.",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  Bdpar$new()

  pipe <- StopWordPipe$new(propertyName,
                           propertyLanguageName,
                           alwaysBeforeDeps,
                           notAfterDeps,
                           removeStopWords,
                           resourcesStopWordsPath)

  path <- file.path("testFiles",
                    "testStopWordPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  instance$setData("you want an apple")

  testthat::expect_error(pipe$pipe(instance),
                         "[StopWordPipe][pipe][Error] Bad compatibility between Pipes",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("pipe instance type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  Bdpar$new()

  pipe <- StopWordPipe$new(propertyName,
                           propertyLanguageName,
                           alwaysBeforeDeps,
                           notAfterDeps,
                           removeStopWords,
                           resourcesStopWordsPath)

  instance <- NULL

  testthat::expect_error(pipe$pipe(instance),
                         "[StopWordPipe][pipe][Error] Checking the type of the 'instance' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("findStopWord",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  Bdpar$new()

  pipe <- StopWordPipe$new(propertyName,
                           propertyLanguageName,
                           alwaysBeforeDeps,
                           notAfterDeps,
                           removeStopWords,
                           resourcesStopWordsPath)

  stopWord <- "you"
  data <- "you want an apple"

  testthat::expect_equal(pipe$findStopWord(data,
                                           stopWord),
                         TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("findStopWord stopWord type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  Bdpar$new()

  pipe <- StopWordPipe$new(propertyName,
                           propertyLanguageName,
                           alwaysBeforeDeps,
                           notAfterDeps,
                           removeStopWords,
                           resourcesStopWordsPath)

  stopWord <- NULL
  data <- "you want an apple"

  testthat::expect_error(pipe$findStopWord(data,
                                           stopWord),
                         "[StopWordPipe][findStopWord][Error] Checking the type of the 'stopWord' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("findStopWord data type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  Bdpar$new()

  pipe <- StopWordPipe$new(propertyName,
                           propertyLanguageName,
                           alwaysBeforeDeps,
                           notAfterDeps,
                           removeStopWords,
                           resourcesStopWordsPath)

  stopWord <- "you"
  data <- NULL

  testthat::expect_error(pipe$findStopWord(data,
                                           stopWord),
                         "[StopWordPipe][findStopWord][Error] Checking the type of the 'data' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("removeStopWord ",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  Bdpar$new()

  pipe <- StopWordPipe$new(propertyName,
                           propertyLanguageName,
                           alwaysBeforeDeps,
                           notAfterDeps,
                           removeStopWords,
                           resourcesStopWordsPath)

  stopWord <- "you"
  data <- "you want an apple"

  testthat::expect_equal(pipe$removeStopWord(stopWord,
                                             data),
                         " want an apple")
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("removeStopWord stopWord type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  Bdpar$new()

  pipe <- StopWordPipe$new(propertyName,
                           propertyLanguageName,
                           alwaysBeforeDeps,
                           notAfterDeps,
                           removeStopWords,
                           resourcesStopWordsPath)

  stopWord <- NULL
  data <- "you want an apple"

  testthat::expect_error(pipe$removeStopWord(stopWord,
                                             data),
                         "[StopWordPipe][removeStopWord][Error] Checking the type of the 'stopWord' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("removeStopWord data type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  Bdpar$new()

  pipe <- StopWordPipe$new(propertyName,
                           propertyLanguageName,
                           alwaysBeforeDeps,
                           notAfterDeps,
                           removeStopWords,
                           resourcesStopWordsPath)

  stopWord <- "you"
  data <- NULL

  testthat::expect_error(pipe$removeStopWord(stopWord,
                                             data),
                         "[StopWordPipe][removeStopWord][Error] Checking the type of the 'data' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("getPropertyLanguageName",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  Bdpar$new()

  pipe <- StopWordPipe$new(propertyName,
                           propertyLanguageName,
                           alwaysBeforeDeps,
                           notAfterDeps,
                           removeStopWords,
                           resourcesStopWordsPath)

  testthat::expect_equal(pipe$getPropertyLanguageName(),
                         "language")
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("getResourcesStopWordsPath",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  Bdpar$new()

  pipe <- StopWordPipe$new(propertyName,
                           propertyLanguageName,
                           alwaysBeforeDeps,
                           notAfterDeps,
                           removeStopWords,
                           resourcesStopWordsPath)

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  pipe$setResourcesStopWordsPath(path)

  testthat::expect_equal(pipe$getResourcesStopWordsPath(),
                         path)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("setResourcesStopWordsPath",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  Bdpar$new()

  pipe <- StopWordPipe$new(propertyName,
                           propertyLanguageName,
                           alwaysBeforeDeps,
                           notAfterDeps,
                           removeStopWords,
                           resourcesStopWordsPath)

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  pipe$setResourcesStopWordsPath(path)

  testthat::expect_equal(pipe$getResourcesStopWordsPath(),
                         path)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("setResourcesStopWordsPath path type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "stopWord"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeStopWords <- TRUE
  resourcesStopWordsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "stopwords-json")

  bdpar.Options$set(key = "resources.stopwords.path",
                    value = path)

  Bdpar$new()

  pipe <- StopWordPipe$new(propertyName,
                           propertyLanguageName,
                           alwaysBeforeDeps,
                           notAfterDeps,
                           removeStopWords,
                           resourcesStopWordsPath)

  path <- NULL

  testthat::expect_error(pipe$setResourcesStopWordsPath(path),
                         "[StopWordPipe][setResourcesStopWordsPath][Error] Checking the type of the 'path' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
