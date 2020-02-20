testthat::context("ContractionPipe")

testthat::setup(bdpar.Options$reset())

test_that("initialize",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = path)

  testthat::expect_silent(ContractionPipe$new(propertyName,
                                              propertyLanguageName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              replaceContractions,
                                              resourcesContractionsPath))

  resourcesContractionsPath <- path

  testthat::expect_silent(ContractionPipe$new(propertyName,
                                              propertyLanguageName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              replaceContractions,
                                              resourcesContractionsPath))
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
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = path)

  testthat::expect_error(ContractionPipe$new(propertyName,
                                             propertyLanguageName,
                                             alwaysBeforeDeps,
                                             notAfterDeps,
                                             replaceAbbreviations,
                                             resourcesAbbreviationsPath),
                         "[ContractionPipe][initialize][Error] Checking the type of the 'propertyName' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("initialize propertyLanguageName type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = path)

  testthat::expect_error(ContractionPipe$new(propertyName,
                                             propertyLanguageName,
                                             alwaysBeforeDeps,
                                             notAfterDeps,
                                             replaceContractions,
                                             resourcesContractionsPath),
                         "[ContractionPipe][initialize][Error] Checking the type of the 'propertyLanguageName' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("initialize alwaysBeforeDeps type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = path)

  testthat::expect_error(ContractionPipe$new(propertyName,
                                             propertyLanguageName,
                                             alwaysBeforeDeps,
                                             notAfterDeps,
                                             replaceContractions,
                                             resourcesContractionsPath),
                         "[ContractionPipe][initialize][Error] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("initialize notAfterDeps type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = path)

  testthat::expect_error(ContractionPipe$new(propertyName,
                                             propertyLanguageName,
                                             alwaysBeforeDeps,
                                             notAfterDeps,
                                             replaceContractions,
                                             resourcesContractionsPath),
                         "[ContractionPipe][initialize][Error] Checking the type of the 'notAfterDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("initialize replaceContractions type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- NULL
  resourcesContractionsPath <- NULL

  path <- file.path("resourcesFiles",
                    "testResources",
                    "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = path)

  testthat::expect_error(ContractionPipe$new(propertyName,
                                             propertyLanguageName,
                                             alwaysBeforeDeps,
                                             notAfterDeps,
                                             replaceContractions,
                                             resourcesContractionsPath),
                         "[ContractionPipe][initialize][Error] Checking the type of the 'replaceContractions' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("initialize resourcesContractionsPath type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  bdpar.Options$set(key = "resources.contractions.path",
                    value = NULL)

  testthat::expect_error(ContractionPipe$new(propertyName,
                                             propertyLanguageName,
                                             alwaysBeforeDeps,
                                             notAfterDeps,
                                             replaceContractions,
                                             resourcesContractionsPath),
                         "[ContractionPipe][initialize][Error] Path of contractions resources is neither defined in initialize or in bdpar.Options",
                         fixed = TRUE)

  resourcesContractionsPath <- 1

  testthat::expect_error(ContractionPipe$new(propertyName,
                                             propertyLanguageName,
                                             alwaysBeforeDeps,
                                             notAfterDeps,
                                             replaceContractions,
                                             resourcesContractionsPath),
                         "[ContractionPipe][initialize][Error] Checking the type of the 'resourcesContractionsPath' variable: numeric",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("pipe",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- ContractionPipe$new(propertyName,
                              propertyLanguageName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              replaceContractions,
                              resourcesContractionsPath)

  path <- file.path("testFiles",
                    "testContractionPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("I'm tall")
  instance$addProperties("en","language")

  instance <- pipe$pipe(instance)

  testthat::expect_equal(instance$getSpecificProperty("contractions"),"I'm")
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("pipe data empty",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- ContractionPipe$new(propertyName,
                              propertyLanguageName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              replaceContractions,
                              resourcesContractionsPath)

  path <- file.path("testFiles",
                    "testContractionPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("")
  instance$addProperties("en","language")

  testthat::expect_warning(pipe$pipe(instance),
                           "\\[ContractionPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testContractionPipe\\/testFile\\.tsms has data empty on pipe Contractions")

})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("pipe wihtout json file",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "contractions-wrong")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- ContractionPipe$new(propertyName,
                              propertyLanguageName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              replaceContractions,
                              resourcesContractionsPath)

  path <- file.path("testFiles",
                    "testContractionPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("I'm tall")
  instance$addProperties("en","language")

  testthat::expect_warning(pipe$pipe(instance),
                           "\\[ContractionPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testContractionPipe\\/testFile\\.tsms has not an contractionsJsonFile to apply to the language ->en")
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("pipe wihtout language property",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- ContractionPipe$new(propertyName,
                              propertyLanguageName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              replaceContractions,
                              resourcesContractionsPath)

  path <- file.path("testFiles",
                    "testContractionPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("I'm tall")

  testthat::expect_warning(pipe$pipe(instance),
                           "\\[ContractionPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testContractionPipe\\/testFile\\.tsms has not language property")
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("pipe instance type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- ContractionPipe$new(propertyName,
                              propertyLanguageName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              replaceContractions,
                              resourcesContractionsPath)

  instance <- NULL

  testthat::expect_error(pipe$pipe(instance),
                         "[ContractionPipe][pipe][Error] Checking the type of the 'instance' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("findContraction",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- ContractionPipe$new(propertyName,
                              propertyLanguageName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              replaceContractions,
                              resourcesContractionsPath)

  contraction <- "I'm"
  data <- "I'm tall"

  testthat::expect_equal(pipe$findContraction(data,
                                              contraction),
                         TRUE)

})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("findContraction abbreviation type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- ContractionPipe$new(propertyName,
                              propertyLanguageName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              replaceContractions,
                              resourcesContractionsPath)

  contraction <- NULL
  data <- "I'm tall"

  testthat::expect_error(pipe$findContraction(data,
                                              contraction),
                         "[ContractionPipe][findContraction][Error] Checking the type of the 'contraction' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("findContraction data type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- ContractionPipe$new(propertyName,
                              propertyLanguageName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              replaceContractions,
                              resourcesContractionsPath)

  contraction <- "I'm"
  data <- NULL

  testthat::expect_error(pipe$findContraction(data, contraction),
                         "[ContractionPipe][findContraction][Error] Checking the type of the 'data' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("replaceContraction ",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- ContractionPipe$new(propertyName,
                              propertyLanguageName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              replaceContractions,
                              resourcesContractionsPath)

  contraction <- "I'm"
  extendedContraction <- "I am"
  data <- "I'm tall"

  testthat::expect_equal(pipe$replaceContraction(contraction,
                                                 extendedContraction,
                                                 data),
                         " I am  tall")
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("replaceContraction contraction type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- ContractionPipe$new(propertyName,
                              propertyLanguageName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              replaceContractions,
                              resourcesContractionsPath)

  contraction <- NULL
  extendedContraction <- "I am"
  data <- "I'm tall"

  testthat::expect_error(pipe$replaceContraction(contraction,
                                                 extendedContraction,
                                                 data),
                         "[ContractionPipe][replaceContraction][Error] Checking the type of the 'contraction' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("replaceContraction extendedContraction type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- ContractionPipe$new(propertyName,
                              propertyLanguageName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              replaceContractions,
                              resourcesContractionsPath)

  contraction <- "I'm"
  extendedContraction <- NULL
  data <- "I'm tall"

  testthat::expect_error(pipe$replaceContraction(contraction,
                                                 extendedContraction,
                                                 data),
                         "[ContractionPipe][replaceContraction][Error] Checking the type of the 'extendedContraction' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("replaceContraction data type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- ContractionPipe$new(propertyName,
                              propertyLanguageName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              replaceContractions,
                              resourcesContractionsPath)

  contraction <- "I'm"
  extendedContraction <- "I am"
  data <- NULL

  testthat::expect_error(pipe$replaceContraction(contraction,
                                                 extendedContraction,
                                                 data),
                         "[ContractionPipe][replaceContraction][Error] Checking the type of the 'data' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("getPropertyLanguageName",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- ContractionPipe$new(propertyName,
                              propertyLanguageName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              replaceContractions,
                              resourcesContractionsPath)

  testthat::expect_equal(pipe$getPropertyLanguageName(),
                         "language")
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("getResourcesContractionsPath",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- ContractionPipe$new(propertyName,
                              propertyLanguageName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              replaceContractions,
                              resourcesContractionsPath)

  path <- file.path("resourcesFiles",
                    "testResources",
                    "contractions-json")

  pipe$setResourcesContractionsPath(path)

  testthat::expect_equal(pipe$getResourcesContractionsPath(),
                         path)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("setResourcesContractionsPath",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- ContractionPipe$new(propertyName,
                              propertyLanguageName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              replaceContractions,
                              resourcesContractionsPath)

  path <- file.path("resourcesFiles",
                    "testResources",
                    "contractions-json")

  pipe$setResourcesContractionsPath(path)

  testthat::expect_equal(pipe$getResourcesContractionsPath(),
                         path)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("setResourcesContractionsPath path type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("rjson")
  propertyName <- "contractions"
  propertyLanguageName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  replaceContractions <- TRUE
  resourcesContractionsPath <- NULL

  pathResources <- file.path("resourcesFiles",
                             "testResources",
                             "contractions-json")

  bdpar.Options$set(key = "resources.contractions.path",
                    value = pathResources)

  Bdpar$new()

  pipe <- ContractionPipe$new(propertyName,
                              propertyLanguageName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              replaceContractions,
                              resourcesContractionsPath)

  path <- NULL

  testthat::expect_error(pipe$setResourcesContractionsPath(path),
                         "[ContractionPipe][setResourcesContractionsPath][Error] Checking the type of the 'path' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
