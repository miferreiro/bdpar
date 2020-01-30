testthat::context("FindHashtagPipe")

testthat::test_that("initialize",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- TRUE

  testthat::expect_silent(FindHashtagPipe$new(propertyName,
                                              alwaysBeforeDeps,
                                              notAfterDeps,
                                              removeHashtags))
})

testthat::test_that("initialize propertyName type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- TRUE

  testthat::expect_error(FindHashtagPipe$new(propertyName,
                                             alwaysBeforeDeps,
                                             notAfterDeps,
                                             removeHashtags),
                         "[FindHashtagPipe][initialize][Error] Checking the type of the 'propertyName' variable: NULL",
                        fixed = TRUE)
})

testthat::test_that("initialize alwaysBeforeDeps type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  removeHashtags <- TRUE

  testthat::expect_error(FindHashtagPipe$new(propertyName,
                                             alwaysBeforeDeps,
                                             notAfterDeps,
                                             removeHashtags),
                         "[FindHashtagPipe][initialize][Error] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("initialize notAfterDeps type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  removeHashtags <- TRUE

  testthat::expect_error(FindHashtagPipe$new(propertyName,
                                             alwaysBeforeDeps,
                                             notAfterDeps,
                                             removeHashtags),
                         "[FindHashtagPipe][initialize][Error] Checking the type of the 'notAfterDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("initialize removeHashtags type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- NULL

  testthat::expect_error(FindHashtagPipe$new(propertyName,
                                             alwaysBeforeDeps,
                                             notAfterDeps,
                                             removeHashtags),
                         "[FindHashtagPipe][initialize][Error] Checking the type of the 'removeHashtags' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("pipe removeHashtags <- TRUE",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- TRUE

  pipe <- FindHashtagPipe$new(propertyName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              removeHashtags)

  path <- file.path("testFiles",
                    "testFindHashtagPipe",
                    "testFile.tsms")
  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am #example")

  instance <- pipe$pipe(instance)
  testthat::expect_equal(instance$getSpecificProperty("hashtag"),
                         "#example")
  testthat::expect_equal(instance$getData(),
                         "Hey I am")
})

testthat::test_that("pipe removeHashtags <- FALSE",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- FALSE

  pipe <- FindHashtagPipe$new(propertyName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              removeHashtags)

  path <- file.path("testFiles",
                    "testFindHashtagPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("Hey I am #example")

  instance <- pipe$pipe(instance)
  testthat::expect_equal(instance$getSpecificProperty("hashtag"),
                         "#example")
  testthat::expect_equal(instance$getData(),
                         "Hey I am #example")
})

testthat::test_that("pipe Bad compatibility between Pipes.",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  removeHashtags <- TRUE

  pipe <- FindHashtagPipe$new(propertyName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              removeHashtags)

  path <- file.path("testFiles",
                    "testFindHashtagPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  instance$setData("Hey I am #example")

  testthat::expect_error(pipe$pipe(instance),
                         "[FindHashtagPipe][pipe][Error] Bad compatibility between Pipes",
                         fixed = TRUE)
})

testthat::test_that("pipe instance type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- TRUE

  pipe <- FindHashtagPipe$new(propertyName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              removeHashtags)

  instance <- NULL
  testthat::expect_error(pipe$pipe(instance),
                         "[FindHashtagPipe][pipe][Error] Checking the type of the 'instance' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("pipe empty data",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- TRUE

  pipe <- FindHashtagPipe$new(propertyName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              removeHashtags)

  path <- file.path("testFiles",
                    "testFindHashtagPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("#example")
  expect_warning(pipe$pipe(instance),
                 "\\[FindHashtagPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testFindHashtagPipe\\/testFile\\.tsms has data empty on pipe Hashtag")
  testthat::expect_equal(instance$getSpecificProperty("hashtag"),
                         "#example")
  testthat::expect_equal(instance$getData(),
                         "")
})

testthat::test_that("findUserName",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- TRUE

  pipe <- FindHashtagPipe$new(propertyName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              removeHashtags)

  data <- "#example"

  testthat::expect_equal(pipe$findHashtag(data),
                         "#example")
})

testthat::test_that("findHashtag data type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- TRUE

  pipe <- FindHashtagPipe$new(propertyName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              removeHashtags)

  data <- NULL

  testthat::expect_error(pipe$findHashtag(data),
                         "[FindHashtagPipe][findHashtag][Error] Checking the type of the 'data' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("removeHashtag",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- TRUE

  pipe <- FindHashtagPipe$new(propertyName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              removeHashtags)

  data <- "#example"

  testthat::expect_equal(pipe$removeHashtag(data),
                         " ")
})

testthat::test_that("removeHashtag data type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "hashtag"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeHashtags <- TRUE

  pipe <- FindHashtagPipe$new(propertyName,
                              alwaysBeforeDeps,
                              notAfterDeps,
                              removeHashtags)

  data <- NULL

  testthat::expect_error(pipe$removeHashtag(data),
                         "[FindHashtagPipe][removeHashtag][Error] Checking the type of the 'data' variable: NULL",
                         fixed = TRUE)

})
