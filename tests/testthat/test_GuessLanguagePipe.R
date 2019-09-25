context("GuessLanguagePipe")

test_that("initialize",{

  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_silent(GuessLanguagePipe$new(propertyName,alwaysBeforeDeps,notAfterDeps))
})

test_that("initialize propertyName type error",{

  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(GuessLanguagePipe$new(propertyName,alwaysBeforeDeps,notAfterDeps),"\\[GuessLanguagePipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "language"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  expect_error(GuessLanguagePipe$new(propertyName,alwaysBeforeDeps,notAfterDeps),"\\[GuessLanguagePipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{

  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  expect_error(GuessLanguagePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps),"\\[GuessLanguagePipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("pipe",{

  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- GuessLanguagePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  path <- file.path("testFiles",
                    "testGuessLanguagePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setSpecificProperty("extension","tsms")
  instance$obtainSource()
  languageTwitter <- TRUE
  instance <- pipe$pipe(instance, languageTwitter)
  expect_equal(instance$getSpecificProperty("language"),"en")

})

test_that("pipe no detect language",{

  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- GuessLanguagePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testGuessLanguagePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setSpecificProperty("extension","tsms")
  instance$setData("try")
  languageTwitter <- TRUE

  expect_warning(pipe$pipe(instance, languageTwitter),"\\[GuessLanguagePipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testGuessLanguagePipe\\/testFile\\.tsms has a null language")

})

test_that("pipe Bad compatibility between Pipes.",{

  propertyName <- "language"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  pipe <- GuessLanguagePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testGuessLanguagePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  languageTwitter <- TRUE

  instance$addBanPipes("pipeExample")
  expect_error(pipe$pipe(instance, languageTwitter),"\\[GuessLanguagePipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{

  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- GuessLanguagePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  instance <- NULL
  languageTwitter <- TRUE
  expect_error(pipe$pipe(instance, languageTwitter),"\\[GuessLanguagePipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("pipe languageTwitter type error",{

  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- GuessLanguagePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testGuessLanguagePipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  languageTwitter <- NULL
  expect_error(pipe$pipe(instance, languageTwitter),"\\[GuessLanguagePipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: languageTwitter NULL")

})

test_that("getLanguage",{

  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- GuessLanguagePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  data <- "This text is an English example to detecte the language"

  expect_equal(pipe$getLanguage(data), "en")

})

test_that("getLanguage data input error",{

  propertyName <- "language"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- GuessLanguagePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  data <- NULL

  expect_error(pipe$getLanguage(data),"\\[GuessLanguagePipe\\]\\[getLanguage\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})
