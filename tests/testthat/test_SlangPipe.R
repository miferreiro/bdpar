testthat::context("SlangPipe")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  testthat::expect_silent(SlangPipe$new(propertyName,
                                        propertyLanguageName,
                                        alwaysBeforeDeps,
                                        notAfterDeps,
                                        replaceSlangs,
                                        resourcesSlangsPath))

  resourcesSlangsPath <- path

  testthat::expect_silent(SlangPipe$new(propertyName,
                                        propertyLanguageName,
                                        alwaysBeforeDeps,
                                        notAfterDeps,
                                        replaceSlangs,
                                        resourcesSlangsPath))
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
  testthat::skip_if_not_installed("rjson")
  propertyName <- NULL
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  testthat::expect_error(SlangPipe$new(propertyName,
                                       propertyLanguageName,
                                       alwaysBeforeDeps,
                                       notAfterDeps,
                                       replaceSlangs,
                                       resourcesSlangsPath),
                         "[SlangPipe][initialize][FATAL] Checking the type of the 'propertyName' variable: NULL",
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
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  testthat::expect_error(SlangPipe$new(propertyName,
                                       propertyLanguageName,
                                       alwaysBeforeDeps,
                                       notAfterDeps,
                                       replaceSlangs,
                                       resourcesSlangsPath),
                         "[SlangPipe][initialize][FATAL] Checking the type of the 'propertyLanguageName' variable: NULL",
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
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  testthat::expect_error(SlangPipe$new(propertyName,
                                       propertyLanguageName,
                                       alwaysBeforeDeps,
                                       notAfterDeps,
                                       replaceSlangs,
                                       resourcesSlangsPath),
                         "[SlangPipe][initialize][FATAL] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
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
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  testthat::expect_error(SlangPipe$new(propertyName,
                                       propertyLanguageName,
                                       alwaysBeforeDeps,
                                       notAfterDeps,
                                       replaceSlangs,
                                       resourcesSlangsPath),
                         "[SlangPipe][initialize][FATAL] Checking the type of the 'notAfterDeps' variable: NULL",
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

testthat::test_that("initialize replaceSlangs type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- NULL
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  testthat::expect_error(SlangPipe$new(propertyName,
                                       propertyLanguageName,
                                       alwaysBeforeDeps,
                                       notAfterDeps,
                                       replaceSlangs,
                                       resourcesSlangsPath),
                         "[SlangPipe][initialize][FATAL] Checking the type of the 'replaceSlangs' variable: NULL",
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

testthat::test_that("initialize resourcesSlangsPath type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  bdpar.Options$set(key = "resources.slangs.path",
                    value = NULL)

  testthat::expect_error(SlangPipe$new(propertyName,
                                       propertyLanguageName,
                                       alwaysBeforeDeps,
                                       notAfterDeps,
                                       replaceSlangs,
                                       resourcesSlangsPath),
                         "[SlangPipe][initialize][FATAL] Path of slangs resources is neither defined in initialize or in bdpar.Options",
                         fixed = TRUE)

  resourcesSlangsPath <- 1

  testthat::expect_error(SlangPipe$new(propertyName,
                                       propertyLanguageName,
                                       alwaysBeforeDeps,
                                       notAfterDeps,
                                       replaceSlangs,
                                       resourcesSlangsPath),
                         "[SlangPipe][initialize][FATAL] Checking the type of the 'resourcesSlangsPath' variable: numeric",
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
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  pipe <- SlangPipe$new(propertyName,
                        propertyLanguageName,
                        alwaysBeforeDeps,
                        notAfterDeps,
                        replaceSlangs,
                        resourcesSlangsPath)

  path <- file.path("testFiles",
                    "testSlangPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("yur name")
  instance$addProperties("en","language")

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  pipe$setResourcesSlangsPath(path)
  instance <- pipe$pipe(instance)

  testthat::expect_equal(instance$getSpecificProperty("langpropname"),
                         "yur")
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
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  pipe <- SlangPipe$new(propertyName,
                        propertyLanguageName,
                        alwaysBeforeDeps,
                        notAfterDeps,
                        replaceSlangs,
                        resourcesSlangsPath)

  path <- file.path("testFiles",
                    "testSlangPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("")
  instance$addProperties("en","language")

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  pipe$setResourcesSlangsPath(path)

  testthat::expect_warning(pipe$pipe(instance),
                           "\\[SlangPipe\\]\\[pipe\\]\\[WARN\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testSlangPipe\\/testFile\\.tsms has data empty on pipe Slang")

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
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-wrong")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  pipe <- SlangPipe$new(propertyName,
                        propertyLanguageName,
                        alwaysBeforeDeps,
                        notAfterDeps,
                        replaceSlangs,
                        resourcesSlangsPath)

  path <- file.path("testFiles",
                    "testSlangPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("yur name")
  instance$addProperties("en","language")

  testthat::expect_warning(pipe$pipe(instance),
                           "\\[SlangPipe\\]\\[pipe\\]\\[WARN\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testSlangPipe\\/testFile\\.tsms has not an SlangsJsonFile to apply to the language-> en")

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
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  pipe <- SlangPipe$new(propertyName,
                        propertyLanguageName,
                        alwaysBeforeDeps,
                        notAfterDeps,
                        replaceSlangs,
                        resourcesSlangsPath)

  path <- file.path("testFiles",
                    "testSlangPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("yur name")

  testthat::expect_warning(pipe$pipe(instance),
                           "\\[SlangPipe\\]\\[pipe\\]\\[WARN\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testSlangPipe\\/testFile\\.tsms has not language property")

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
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  pipe <- SlangPipe$new(propertyName,
                        propertyLanguageName,
                        alwaysBeforeDeps,
                        notAfterDeps,
                        replaceSlangs,
                        resourcesSlangsPath)

  instance <- NULL

  testthat::expect_error(pipe$pipe(instance),
                         "[SlangPipe][pipe][FATAL] Checking the type of the 'instance' variable: NULL",
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

testthat::test_that("findSlang",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  pipe <- SlangPipe$new(propertyName,
                        propertyLanguageName,
                        alwaysBeforeDeps,
                        notAfterDeps,
                        replaceSlangs,
                        resourcesSlangsPath)

  slang <- "yur"
  data <- "yur name"

  testthat::expect_equal(pipe$findSlang(data,
                                        slang),
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

testthat::test_that("findSlang slang type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  pipe <- SlangPipe$new(propertyName,
                        propertyLanguageName,
                        alwaysBeforeDeps,
                        notAfterDeps,
                        replaceSlangs,
                        resourcesSlangsPath)

  slang <- NULL
  data <- "yur name"

  testthat::expect_error(pipe$findSlang(data,
                                        slang),
                         "[SlangPipe][findSlang][FATAL] Checking the type of the 'slang' variable: NULL",
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

testthat::test_that("findSlang data type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  pipe <- SlangPipe$new(propertyName,
                        propertyLanguageName,
                        alwaysBeforeDeps,
                        notAfterDeps,
                        replaceSlangs,
                        resourcesSlangsPath)

  slang <- "yur"
  data <- NULL

  testthat::expect_error(pipe$findSlang(data,
                                        slang),
                         "[SlangPipe][findSlang][FATAL] Checking the type of the 'data' variable: NULL",
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

testthat::test_that("replaceSlang ",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  pipe <- SlangPipe$new(propertyName,
                        propertyLanguageName,
                        alwaysBeforeDeps,
                        notAfterDeps,
                        replaceSlangs,
                        resourcesSlangsPath)

  slang <- "yur"
  extendedSlang <- "your"
  data <- "yur name"

  testthat::expect_equal(pipe$replaceSlang(slang,
                                           extendedSlang,
                                           data),
                         " your  name")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("replaceSlang slang type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  pipe <- SlangPipe$new(propertyName,
                        propertyLanguageName,
                        alwaysBeforeDeps,
                        notAfterDeps,
                        replaceSlangs,
                        resourcesSlangsPath)

  slang <- NULL
  extendedSlang <- "your"
  data <- "yur name"

  testthat::expect_error(pipe$replaceSlang(slang,
                                           extendedSlang,
                                           data),
                         "[SlangPipe][replaceSlang][FATAL] Checking the type of the 'slang' variable: NULL",
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

testthat::test_that("replaceSlang extendedSlang type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  pipe <- SlangPipe$new(propertyName,
                        propertyLanguageName,
                        alwaysBeforeDeps,
                        notAfterDeps,
                        replaceSlangs,
                        resourcesSlangsPath)

  slang <- "yur"
  extendedSlang <- NULL
  data <- "yur name"

  testthat::expect_error(pipe$replaceSlang(slang,
                                           extendedSlang,
                                           data),
                         "[SlangPipe][replaceSlang][FATAL] Checking the type of the 'extendedSlang' variable: NULL",
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

testthat::test_that("replaceSlang data type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  pipe <- SlangPipe$new(propertyName,
                        propertyLanguageName,
                        alwaysBeforeDeps,
                        notAfterDeps,
                        replaceSlangs,
                        resourcesSlangsPath)

  slang <- "yur"
  extendedSlang <- "your"
  data <- NULL

  testthat::expect_error(pipe$replaceSlang(slang,
                                           extendedSlang,
                                           data),
                         "[SlangPipe][replaceSlang][FATAL] Checking the type of the 'data' variable: NULL",
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
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  pipe <- SlangPipe$new(propertyName,
                        propertyLanguageName,
                        alwaysBeforeDeps,
                        notAfterDeps,
                        replaceSlangs,
                        resourcesSlangsPath)

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

testthat::test_that("getResourcesSlangsPath",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  pipe <- SlangPipe$new(propertyName,
                        propertyLanguageName,
                        alwaysBeforeDeps,
                        notAfterDeps,
                        replaceSlangs,
                        resourcesSlangsPath)

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  pipe$setResourcesSlangsPath(path)

  testthat::expect_equal(pipe$getResourcesSlangsPath(),
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

testthat::test_that("setResourcesSlangsPath",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  pipe <- SlangPipe$new(propertyName,
                        propertyLanguageName,
                        alwaysBeforeDeps,
                        notAfterDeps,
                        replaceSlangs,
                        resourcesSlangsPath)

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json",
                    "slang.en.json")

  pipe$setResourcesSlangsPath(path)

  testthat::expect_equal(pipe$getResourcesSlangsPath(),
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

testthat::test_that("setResourcesSlangsPath path type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "langpropname"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceSlangs <- TRUE
  resourcesSlangsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "slangs-json")

  bdpar.Options$set(key = "resources.slangs.path",
                    value = path)

  pipe <- SlangPipe$new(propertyName,
                        propertyLanguageName,
                        alwaysBeforeDeps,
                        notAfterDeps,
                        replaceSlangs,
                        resourcesSlangsPath)

  path <- NULL

  testthat::expect_error(pipe$setResourcesSlangsPath(path),
                         "[SlangPipe][setResourcesSlangsPath][FATAL] Checking the type of the 'path' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
