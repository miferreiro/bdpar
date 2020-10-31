testthat::context("InterjectionPipe")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  testthat::expect_silent(InterjectionPipe$new(propertyName,
                                               propertyLanguageName,
                                               alwaysBeforeDeps,
                                               notAfterDeps,
                                               removeInterjections,
                                               resourcesInterjectionsPath))

  resourcesInterjectionsPath <- path

  testthat::expect_silent(InterjectionPipe$new(propertyName,
                                               propertyLanguageName,
                                               alwaysBeforeDeps,
                                               notAfterDeps,
                                               removeInterjections,
                                               resourcesInterjectionsPath))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize propertyName type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- NULL
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  testthat::expect_error(InterjectionPipe$new(propertyName,
                                              propertyLanguageName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              removeInterjections,
                                              resourcesInterjectionsPath),
                         "[InterjectionPipe][initialize][FATAL] Checking the type of the 'propertyName' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize propertyLanguageName type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  testthat::expect_error(InterjectionPipe$new(propertyName,
                                              propertyLanguageName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              removeInterjections,
                                              resourcesInterjectionsPath),
                         "[InterjectionPipe][initialize][FATAL] Checking the type of the 'propertyLanguageName' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize alwaysBeforeDeps type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  testthat::expect_error(InterjectionPipe$new(propertyName,
                                              propertyLanguageName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              removeInterjections,
                                              resourcesInterjectionsPath),
                         "[InterjectionPipe][initialize][FATAL] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize notAfterDeps type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  testthat::expect_error(InterjectionPipe$new(propertyName,
                                              propertyLanguageName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              removeInterjections,
                                              resourcesInterjectionsPath),
                         "[InterjectionPipe][initialize][FATAL] Checking the type of the 'notAfterDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize removeInterjections type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- NULL
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  testthat::expect_error(InterjectionPipe$new(propertyName,
                                              propertyLanguageName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              removeInterjections,
                                              resourcesInterjectionsPath),
                         "[InterjectionPipe][initialize][FATAL] Checking the type of the 'removeInterjections' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize resourcesInterjectionsPath type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  bdpar.Options$set(key = "resources.interjections.path",
                    value = NULL)

  testthat::expect_error(InterjectionPipe$new(propertyName,
                                              propertyLanguageName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              removeInterjections,
                                              resourcesInterjectionsPath),
                         "[InterjectionPipe][initialize][FATAL] Path of interjections resources is neither defined in initialize or in bdpar.Options",
                         fixed = TRUE)

  resourcesInterjectionsPath <- 1

  testthat::expect_error(InterjectionPipe$new(propertyName,
                                              propertyLanguageName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              removeInterjections,
                                              resourcesInterjectionsPath),
                         "[InterjectionPipe][initialize][FATAL] Checking the type of the 'resourcesInterjectionsPath' variable: numeric",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("pipe",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  Bdpar$new()

  pipe <- InterjectionPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeInterjections,
                               resourcesInterjectionsPath)

  path <- file.path("testFiles",
                    "testInterjectionPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("yeah I like it")
  instance$addProperties("en","language")

  pipe$pipe(instance)
  testthat::expect_equal(instance$getSpecificProperty("interjection"), c("yeah", "like"))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("pipe data empty",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  Bdpar$new()

  pipe <- InterjectionPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeInterjections,
                               resourcesInterjectionsPath)

  path <- file.path("testFiles",
                    "testInterjectionPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("")
  instance$addProperties("en","language")

  testthat::expect_warning(pipe$pipe(instance),
                           "\\[InterjectionPipe\\]\\[pipe\\]\\[WARN\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testInterjectionPipe\\/testFile\\.tsms has data empty on pipe Interjection")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("pipe wihtout json file",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-wrong")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  Bdpar$new()

  pipe <- InterjectionPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeInterjections,
                               resourcesInterjectionsPath)

  path <- file.path("testFiles",
                    "testInterjectionPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("yeah I like it")
  instance$addProperties("en","language")

  testthat::expect_warning(pipe$pipe(instance),
                           "\\[InterjectionPipe\\]\\[pipe\\]\\[WARN\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testInterjectionPipe\\/testFile\\.tsms has not an interjectionsJsonFile to apply to the language ->en")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("pipe wihtout language property",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  Bdpar$new()

  pipe <- InterjectionPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeInterjections,
                               resourcesInterjectionsPath)

  path <- file.path("testFiles",
                    "testInterjectionPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("yeah I like it")

  testthat::expect_warning(pipe$pipe(instance),
                           "\\[InterjectionPipe\\]\\[pipe\\]\\[WARN\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testInterjectionPipe\\/testFile\\.tsms has not language property")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("pipe instance type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  Bdpar$new()

  pipe <- InterjectionPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeInterjections,
                               resourcesInterjectionsPath)

  instance <- NULL

  testthat::expect_error(pipe$pipe(instance),
                         "[InterjectionPipe][pipe][FATAL] Checking the type of the 'instance' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("findInterjection",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  Bdpar$new()

  pipe <- InterjectionPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeInterjections,
                               resourcesInterjectionsPath)

  interjection <- "yeah"
  data <- "yeah I like it"

  testthat::expect_equal(pipe$findInterjection(data,
                                               interjection),
                         TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("findInterjection interjection type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  Bdpar$new()

  pipe <- InterjectionPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeInterjections,
                               resourcesInterjectionsPath)

  interjection <- NULL
  data <- "yeah I like it"

  testthat::expect_error(pipe$findInterjection(data,
                                               interjection),
                         "[InterjectionPipe][findInterjection][FATAL] Checking the type of the 'interjection' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("findInterjection data type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  Bdpar$new()

  pipe <- InterjectionPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeInterjections,
                               resourcesInterjectionsPath)

  interjection <- "yeah"
  data <- NULL

  testthat::expect_error(pipe$findInterjection(data,
                                               interjection),
                         "[InterjectionPipe][findInterjection][FATAL] Checking the type of the 'data' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("removeInterjection ",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  Bdpar$new()

  pipe <- InterjectionPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeInterjections,
                               resourcesInterjectionsPath)

  interjection <- "yeah"
  data <- "yeah I like it"

  testthat::expect_equal(pipe$removeInterjection(interjection,
                                                 data),
                         " I like it")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("removeInterjection abbreviation type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  Bdpar$new()

  pipe <- InterjectionPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeInterjections,
                               resourcesInterjectionsPath)

  interjection <- NULL
  data <- "yeah I like it"

  testthat::expect_error(pipe$removeInterjection(interjection,
                                                 data),
                         "[InterjectionPipe][removeInterjection][FATAL] Checking the type of the 'interjection' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("removeInterjection data type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  Bdpar$new()

  pipe <- InterjectionPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeInterjections,
                               resourcesInterjectionsPath)

  interjection <- "yeah"
  data <- NULL

  testthat::expect_error(pipe$removeInterjection(interjection,
                                                 data),
                         "[InterjectionPipe][removeInterjection][FATAL] Checking the type of the 'data' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("getPropertyLanguageName",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  Bdpar$new()

  pipe <- InterjectionPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeInterjections,
                               resourcesInterjectionsPath)

  testthat::expect_equal(pipe$getPropertyLanguageName(),
                         "language")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("getResourcesInterjectionsPath",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  Bdpar$new()

  pipe <- InterjectionPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeInterjections,
                               resourcesInterjectionsPath)

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  pipe$setResourcesInterjectionsPath(path)

  testthat::expect_equal(pipe$getResourcesInterjectionsPath(),
                         path)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("setResourcesInterjectionsPath",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  Bdpar$new()

  pipe <- InterjectionPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeInterjections,
                               resourcesInterjectionsPath)

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  pipe$setResourcesInterjectionsPath(path)

  testthat::expect_equal(pipe$getResourcesInterjectionsPath(),
                         path)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("setResourcesInterjectionsPath path type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "interjection"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeInterjections <- TRUE
  resourcesInterjectionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "interjections-json")

  bdpar.Options$set(key = "resources.interjections.path",
                    value = path)

  Bdpar$new()

  pipe <- InterjectionPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               removeInterjections,
                               resourcesInterjectionsPath)

  path <- NULL

  testthat::expect_error(pipe$setResourcesInterjectionsPath(path),
                         "[InterjectionPipe][setResourcesInterjectionsPath][FATAL] Checking the type of the 'path' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
