testthat::context("AbbreviationPipe")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize",{
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rex")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = path)

  testthat::expect_silent(AbbreviationPipe$new(propertyName,
                                               propertyLanguageName,
                                               alwaysBeforeDeps,
                                               notAfterDeps,
                                               replaceAbbreviations,
                                               resourcesAbbreviationsPath))

  resourcesAbbreviationsPath <- path

  testthat::expect_silent(AbbreviationPipe$new(propertyName,
                                               propertyLanguageName,
                                               alwaysBeforeDeps,
                                               notAfterDeps,
                                               replaceAbbreviations,
                                               resourcesAbbreviationsPath))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
  bdpar.Options$set("verbose", TRUE)
})

testthat::test_that("initialize propertyName type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  propertyName <- NULL
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = path)

  testthat::expect_error(AbbreviationPipe$new(propertyName,
                                              propertyLanguageName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              replaceAbbreviations,
                                              resourcesAbbreviationsPath),
                         "[AbbreviationPipe][initialize][FATAL] Checking the type of the 'propertyName' variable: NULL",
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
  propertyName <- "abbreviation"
  propertyLanguageName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = path)

  testthat::expect_error(AbbreviationPipe$new(propertyName,
                                              propertyLanguageName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              replaceAbbreviations,
                                              resourcesAbbreviationsPath),
                         "[AbbreviationPipe][initialize][FATAL] Checking the type of the 'propertyLanguageName' variable: NULL",
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
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = path)

  testthat::expect_error(AbbreviationPipe$new(propertyName,
                                              propertyLanguageName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              replaceAbbreviations,
                                              resourcesAbbreviationsPath),
                         "[AbbreviationPipe][initialize][FATAL] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
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
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = path)

  testthat::expect_error(AbbreviationPipe$new(propertyName,
                                              propertyLanguageName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              replaceAbbreviations,
                                              resourcesAbbreviationsPath),
                         "[AbbreviationPipe][initialize][FATAL] Checking the type of the 'notAfterDeps' variable: NULL",
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

testthat::test_that("initialize replaceAbbreviations type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- NULL
  resourcesAbbreviationsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = path)

  testthat::expect_error(AbbreviationPipe$new(propertyName,
                                              propertyLanguageName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              replaceAbbreviations,
                                              resourcesAbbreviationsPath),
                         "[AbbreviationPipe][initialize][FATAL] Checking the type of the 'replaceAbbreviations' variable: NULL",
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

testthat::test_that("initialize resourcesAbbreviationsPath type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = NULL)
  testthat::expect_error(AbbreviationPipe$new(propertyName,
                                              propertyLanguageName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              replaceAbbreviations,
                                              resourcesAbbreviationsPath),
                         "[AbbreviationPipe][initialize][FATAL] Path of abbreviations resources is neither defined in initialize or in bdpar.Options",
                         fixed = TRUE)

  resourcesAbbreviationsPath <- 1

  testthat::expect_error(AbbreviationPipe$new(propertyName,
                                              propertyLanguageName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              replaceAbbreviations,
                                              resourcesAbbreviationsPath),
                         "[AbbreviationPipe][initialize][FATAL] Checking the type of the 'resourcesAbbreviationsPath' variable: numeric",
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
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rex")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- AbbreviationPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               replaceAbbreviations,
                               resourcesAbbreviationsPath)

  path <- file.path("testFiles",
                    "testAbbreviationPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Admin. something")
  instance$addProperties("en","language")

  instance <- pipe$pipe(instance)

  testthat::expect_equal(instance$getSpecificProperty("abbreviation"),
                         "Admin.")
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
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rex")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- AbbreviationPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               replaceAbbreviations,
                               resourcesAbbreviationsPath)

  path <- file.path("testFiles",
                    "testAbbreviationPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("")
  instance$addProperties("en","language")

  testthat::expect_warning(pipe$pipe(instance),
                           "\\[AbbreviationPipe\\]\\[pipe\\]\\[WARN\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testAbbreviationPipe\\/testFile\\.tsms has data empty on pipe Abbreviation")
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
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rex")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "abbreviations-wrong")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- AbbreviationPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               replaceAbbreviations,
                               resourcesAbbreviationsPath)

  path <- file.path("testFiles",
                    "testAbbreviationPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("D. something")
  instance$addProperties("en","language")

  testthat::expect_warning(pipe$pipe(instance),
                           "\\[AbbreviationPipe\\]\\[pipe\\]\\[WARN\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testAbbreviationPipe\\/testFile\\.tsms has not an abbreviationsJsonFile to apply to the language ->en")
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
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rex")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- AbbreviationPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               replaceAbbreviations,
                               resourcesAbbreviationsPath)

  path <- file.path("testFiles",
                    "testAbbreviationPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("D. something")

  testthat::expect_warning(pipe$pipe(instance),
                           "\\[AbbreviationPipe\\]\\[pipe\\]\\[WARN\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testAbbreviationPipe\\/testFile\\.tsms has not language property")
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
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rex")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- AbbreviationPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               replaceAbbreviations,
                               resourcesAbbreviationsPath)
  instance <- NULL

  testthat::expect_error(pipe$pipe(instance),
                         "[AbbreviationPipe][pipe][FATAL] Checking the type of the 'instance' variable: NULL",
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

testthat::test_that("findAbbreviation",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- AbbreviationPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               replaceAbbreviations,
                               resourcesAbbreviationsPath)

  abbreviation <- "D."
  data <- "D. something"

  testthat::expect_equal(pipe$findAbbreviation(data,
                                               abbreviation),
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

testthat::test_that("findAbbreviation abbreviation type error",{
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rex")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- AbbreviationPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               replaceAbbreviations,
                               resourcesAbbreviationsPath)
  abbreviation <- NULL
  data <- "D. something"

  testthat::expect_error(pipe$findAbbreviation(data,
                                     abbreviation),
                         "[AbbreviationPipe][findAbbreviation][FATAL] Checking the type of the 'abbreviation' variable: NULL",
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

testthat::test_that("findAbbreviation data type error",{
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rex")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- AbbreviationPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               replaceAbbreviations,
                               resourcesAbbreviationsPath)

  abbreviation <- "D."
  data <- NULL

  testthat::expect_error(pipe$findAbbreviation(data, abbreviation),
                         "[AbbreviationPipe][findAbbreviation][FATAL] Checking the type of the 'data' variable: NULL",
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

testthat::test_that("replaceAbbreviation ",{
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rex")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- AbbreviationPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               replaceAbbreviations,
                               resourcesAbbreviationsPath)

  abbreviation <- "D."
  extendedAbbreviation <- "Don"
  data <- "D. something"

  testthat::expect_equal(pipe$replaceAbbreviation(abbreviation,
                                                  extendedAbbreviation,
                                                  data),
                         " Don  something")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("replaceAbbreviation abbreviation type error",{
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rex")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- AbbreviationPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               replaceAbbreviations,
                               resourcesAbbreviationsPath)

  abbreviation <- NULL
  extendedAbbreviation <- "Don"
  data <- "D. something"

  testthat::expect_error(pipe$replaceAbbreviation(abbreviation,
                                                  extendedAbbreviation,
                                                  data),
                         "[AbbreviationPipe][replaceAbbreviation][FATAL] Checking the type of the 'abbreviation' variable: NULL",
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

testthat::test_that("replaceAbbreviation extendedAbbreviation type error",{
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rex")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- AbbreviationPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               replaceAbbreviations,
                               resourcesAbbreviationsPath)

  abbreviation <- "D."
  extendedAbbreviation <- NULL
  data <- "D. something"

  testthat::expect_error(pipe$replaceAbbreviation(abbreviation,
                                                  extendedAbbreviation,
                                                  data),
                         "[AbbreviationPipe][replaceAbbreviation][FATAL] Checking the type of the 'extendedAbbreviation' variable: NULL",
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

testthat::test_that("replaceAbbreviation data type error",{
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rex")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- AbbreviationPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               replaceAbbreviations,
                               resourcesAbbreviationsPath)

  abbreviation <- "D."
  extendedAbbreviation <- "Don"
  data <- NULL

  testthat::expect_error(pipe$replaceAbbreviation(abbreviation,
                                                  extendedAbbreviation,
                                                  data),
                         "[AbbreviationPipe][replaceAbbreviation][FATAL] Checking the type of the 'data' variable: NULL",
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
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rex")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- AbbreviationPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               replaceAbbreviations,
                               resourcesAbbreviationsPath)

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

testthat::test_that("getResourcesAbbreviationsPath",{
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rex")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- AbbreviationPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               replaceAbbreviations,
                               resourcesAbbreviationsPath)

  path <- file.path("resourcesFiles",
                    "testResources",
                    "abbreviations-json")

  pipe$setResourcesAbbreviationsPath(path)

  testthat::expect_equal(pipe$getResourcesAbbreviationsPath(),
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

testthat::test_that("setResourcesAbbreviationsPath",{
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rex")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- AbbreviationPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               replaceAbbreviations,
                               resourcesAbbreviationsPath)

  path <- file.path("resourcesFiles",
                    "testResources",
                    "abbreviations-json")

  pipe$setResourcesAbbreviationsPath(path)

  testthat::expect_equal(pipe$getResourcesAbbreviationsPath(),
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

testthat::test_that("setResourcesAbbreviationsPath path type error",{
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rex")
  propertyName <- "abbreviation"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceAbbreviations <- TRUE
  resourcesAbbreviationsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "abbreviations-json")

  bdpar.Options$set(key = "resources.abbreviations.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- AbbreviationPipe$new(propertyName,
                               propertyLanguageName,
                               alwaysBeforeDeps,
                               notAfterDeps,
                               replaceAbbreviations,
                               resourcesAbbreviationsPath)

  path <- NULL

  testthat::expect_error(pipe$setResourcesAbbreviationsPath(path),
                         "[AbbreviationPipe][setResourcesAbbreviationsPath][FATAL] Checking the type of the 'path' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
