testthat::context("GuessLanguagePipe")

testthat::test_that("initialize",{

  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  languageTwitter <- FALSE

  testthat::expect_silent(GuessLanguagePipe$new(propertyName,
                                                alwaysBeforeDeps,
                                                notAfterDeps,
                                                languageTwitter))
})

testthat::test_that("initialize propertyName type error",{

  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  languageTwitter <- FALSE

  testthat::expect_error(GuessLanguagePipe$new(propertyName,
                                               alwaysBeforeDeps,
                                               notAfterDeps,
                                               languageTwitter),
                         "[GuessLanguagePipe][initialize][Error] Checking the type of the 'propertyName' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "language"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  languageTwitter <- FALSE

  testthat::expect_error(GuessLanguagePipe$new(propertyName,
                                               alwaysBeforeDeps,
                                               notAfterDeps,
                                               languageTwitter),
                         "[GuessLanguagePipe][initialize][Error] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("initialize notAfterDeps type error",{

  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  languageTwitter <- FALSE

  testthat::expect_error(GuessLanguagePipe$new(propertyName,
                                               alwaysBeforeDeps,
                                               notAfterDeps,
                                               languageTwitter),
                         "[GuessLanguagePipe][initialize][Error] Checking the type of the 'notAfterDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("initialize languageTwitter type error",{

  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  languageTwitter <- NULL

  testthat::expect_error(GuessLanguagePipe$new(propertyName,
                                               alwaysBeforeDeps,
                                               notAfterDeps,
                                               languageTwitter),
                         "[GuessLanguagePipe][initialize][Error] Checking the type of the 'languageTwitter' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("pipe",{
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("cld2")
  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  languageTwitter <- TRUE

  pipe <- GuessLanguagePipe$new(propertyName,
                                alwaysBeforeDeps,
                                notAfterDeps,
                                languageTwitter)

  path <- system.file(file.path("testFiles",
                                "_ham_",
                                "30.tsms"),
                      package = "bdpar")

  path <- file.path("testFiles",
                    "testGuessLanguagePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setSpecificProperty("extension","tsms")
  instance$obtainSource()
  instance <- pipe$pipe(instance)
  testthat::expect_equal(instance$getSpecificProperty("language"),
                         "en")
})

testthat::test_that("pipe no detect language",{
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("cld2")
  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  languageTwitter <- TRUE

  pipe <- GuessLanguagePipe$new(propertyName,
                                alwaysBeforeDeps,
                                notAfterDeps,
                                languageTwitter)

  path <- file.path("testFiles",
                    "testGuessLanguagePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setSpecificProperty("extension","tsms")
  instance$setData("try")

  testthat::expect_warning(pipe$pipe(instance),
                           "\\[GuessLanguagePipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testGuessLanguagePipe\\/testFile\\.tsms has a null language")

})

testthat::test_that("pipe instance type error",{

  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  languageTwitter <- TRUE
  pipe <- GuessLanguagePipe$new(propertyName,
                                alwaysBeforeDeps,
                                notAfterDeps,
                                languageTwitter)

  instance <- NULL
  testthat::expect_error(pipe$pipe(instance),
                         "[GuessLanguagePipe][pipe][Error] Checking the type of the 'instance' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("getLanguage",{
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("cld2")
  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  languageTwitter <- TRUE

  pipe <- GuessLanguagePipe$new(propertyName,
                                alwaysBeforeDeps,
                                notAfterDeps,
                                languageTwitter)

  data <- "This text is an English example to detecte the language"

  testthat::expect_equal(pipe$getLanguage(data),
                         "en")
})

testthat::test_that("getLanguage data input error",{
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("cld2")
  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  languageTwitter <- TRUE

  pipe <- GuessLanguagePipe$new(propertyName,
                                alwaysBeforeDeps,
                                notAfterDeps,
                                languageTwitter)

  data <- NULL

  testthat::expect_error(pipe$getLanguage(data),
                         "[GuessLanguagePipe][getLanguage][Error] Checking the type of the 'data' variable: NULL",
                         fixed = TRUE)
})
