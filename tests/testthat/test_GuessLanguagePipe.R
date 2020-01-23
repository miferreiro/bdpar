context("GuessLanguagePipe")

test_that("initialize",{

  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  languageTwitter <- FALSE

  expect_silent(GuessLanguagePipe$new(propertyName,alwaysBeforeDeps,notAfterDeps, languageTwitter))
})

test_that("initialize propertyName type error",{

  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  languageTwitter <- FALSE

  expect_error(GuessLanguagePipe$new(propertyName,alwaysBeforeDeps,notAfterDeps, languageTwitter),"\\[GuessLanguagePipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "language"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  languageTwitter <- FALSE

  expect_error(GuessLanguagePipe$new(propertyName,alwaysBeforeDeps,notAfterDeps, languageTwitter),"\\[GuessLanguagePipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{

  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  languageTwitter <- FALSE

  expect_error(GuessLanguagePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, languageTwitter),"\\[GuessLanguagePipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("initialize languageTwitter type error",{

  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  languageTwitter <- NULL

  expect_error(GuessLanguagePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, languageTwitter),
  "[GuessLanguagePipe][initialize][Error]
                Checking the type of the variable: languageTwitter NULL", fixed = TRUE)

})

test_that("pipe",{
  skip_if_not_installed("readr")
  skip_if_not_installed("cld2")
  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  languageTwitter <- TRUE

  pipe <- GuessLanguagePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, languageTwitter)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  path <- file.path("testFiles",
                    "testGuessLanguagePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setSpecificProperty("extension","tsms")
  instance$obtainSource()
  instance <- pipe$pipe(instance)
  expect_equal(instance$getSpecificProperty("language"),"en")

})

test_that("pipe no detect language",{
  skip_if_not_installed("readr")
  skip_if_not_installed("cld2")
  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  languageTwitter <- TRUE

  pipe <- GuessLanguagePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, languageTwitter)

  path <- file.path("testFiles",
                    "testGuessLanguagePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setSpecificProperty("extension","tsms")
  instance$setData("try")

  expect_warning(pipe$pipe(instance),"\\[GuessLanguagePipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testGuessLanguagePipe\\/testFile\\.tsms has a null language")

})

test_that("pipe Bad compatibility between Pipes.",{
  skip_if_not_installed("readr")
  propertyName <- "language"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  languageTwitter <- TRUE

  pipe <- GuessLanguagePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, languageTwitter)

  path <- file.path("testFiles",
                    "testGuessLanguagePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)

  instance$addBanPipes("pipeExample")
  expect_error(pipe$pipe(instance),"\\[GuessLanguagePipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{

  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  languageTwitter <- TRUE
  pipe <- GuessLanguagePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, languageTwitter)

  instance <- NULL
  expect_error(pipe$pipe(instance),"\\[GuessLanguagePipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("getLanguage",{
  skip_if_not_installed("readr")
  skip_if_not_installed("cld2")
  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  languageTwitter <- TRUE

  pipe <- GuessLanguagePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, languageTwitter)

  data <- "This text is an English example to detecte the language"

  expect_equal(pipe$getLanguage(data), "en")

})

test_that("getLanguage data input error",{
  skip_if_not_installed("readr")
  skip_if_not_installed("cld2")
  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  languageTwitter <- TRUE

  pipe <- GuessLanguagePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, languageTwitter)

  data <- NULL

  expect_error(pipe$getLanguage(data),"\\[GuessLanguagePipe\\]\\[getLanguage\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})
