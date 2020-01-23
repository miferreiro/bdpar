context("FindUrlPipe")

test_that("initialize",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  expect_silent(FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns))
})

test_that("initialize propertyName type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  expect_error(FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns),"\\[FindUrlPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  expect_error(FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns),"\\[FindUrlPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  expect_error(FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns),"\\[FindUrlPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("initialize removeUrls type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- NULL
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  expect_error(FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns),"\\[FindUrlPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: removeUrls NULL")

})

test_that("initialize URLPatterns type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPatterns <- NULL
  namesURLPatterns <- list("UrlPattern")

  expect_error(FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns),"\\[FindUrlPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: URLPatterns NULL")

})

test_that("initialize namesURLPatterns type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- NULL

  expect_error(FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns),
  "[FindUrlPipe][initialize][Error]
                 Checking the type of the variable: namesURLPatterns NULL", fixed = TRUE)

})

test_that("pipe removeUrl <- TRUE",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns)

  path <- file.path("testFiles",
                    "testFindUrlPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("example www.google.com")
  instance <- pipe$pipe(instance)
  expect_equal(instance$getSpecificProperty("URLs"),c(UrlPattern = c("www.google.com")))
  expect_equal(instance$getData(),"example")

})
test_that("pipe removeUrl <- FALSE",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- FALSE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns)

  path <- file.path("testFiles",
                    "testFindUrlPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("www.google.com")
  instance <- pipe$pipe(instance)
  expect_equal(instance$getSpecificProperty("URLs"),c(UrlPattern = c("www.google.com")))
  expect_equal(instance$getData(),"www.google.com")

})

test_that("pipe Bad compatibility between Pipes.",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns)

  path <- file.path("testFiles",
                    "testFindUrlPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("www.google.com")
  instance$addBanPipes("pipeExample")
  expect_error(pipe$pipe(instance),"\\[FindUrlPipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns)

  path <- file.path("testFiles",
                    "testFindUrlPipe",
                    "testFile.tsms")

  instance <- NULL

  expect_error(pipe$pipe(instance),"\\[FindUrlPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("pipe empty data",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns)

  path <- file.path("testFiles",
                    "testFindUrlPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("www.google.com")
  expect_warning(pipe$pipe(instance),"\\[FindUrlPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testFindUrlPipe\\/testFile\\.tsms has data empty on pipe Url ")
  expect_equal(instance$getSpecificProperty("URLs"),c(UrlPattern = c("www.google.com")))
  expect_equal(instance$getData(),"")

})


test_that("findUrl",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns)

  pattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  data <- "www.google.com"

  expect_equal(pipe$findUrl(pattern, data),"www.google.com")

})

test_that("findUrl pattern type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns)

  pattern <- NULL
  data <- "www.google.com"

  expect_error(pipe$findUrl(pattern, data),"\\[FindUrlPipe\\]\\[findUrl\\]\\[Error\\]
                Checking the type of the variable: pattern NULL")

})

test_that("findUrl data type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns)

  pattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  data <- NULL

  expect_error(pipe$findUrl(pattern, data),"\\[FindUrlPipe\\]\\[findUrl\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})

test_that("removeUrl",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns)

  pattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  data <- "www.google.com"

  expect_equal(pipe$removeUrl(pattern, data)," ")

})

test_that("removeUrl pattern type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns)

  pattern <- NULL
  data <- "www.google.com"

  expect_error(pipe$removeUrl(pattern, data),"\\[FindUrlPipe\\]\\[removeUrl\\]\\[Error\\]
                Checking the type of the variable: pattern NULL")

})

test_that("removeUrl data type error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns)

  pattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  data <- NULL

  expect_error(pipe$removeUrl(pattern, data),"\\[FindUrlPipe\\]\\[removeUrl\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})

test_that("putNamesURLPattern",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns)

  pipe$setNamesURLPatterns(list("URL","EMAIL"))
  resultOfURLPatterns <- list(c("www.google.com"),c("mm@gmail.com"))
  expect_equal(pipe$putNamesURLPattern(resultOfURLPatterns),list(URL = c("www.google.com"),"EMAIL" = c("mm@gmail.com")))

})

test_that("putNamesURLPattern resultOfURLPatterns input error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns)
  pipe$setNamesURLPatterns(list("URL","EMAIL"))
  resultOfURLPatterns <- NULL

  expect_error(pipe$putNamesURLPattern(resultOfURLPatterns),"\\[FindUrlPipe\\]\\[putNamesURLPattern\\]\\[Error\\]
                Checking the type of the variable: resultOfURLPatterns NULL")

})

test_that("getURLPatterns",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns)

  expect_equal(pipe$getURLPatterns(),URLPatterns)

})

test_that("setURLPatterns",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns)

  URLPatternsExpect <- list("[:alnum:]*","[-]+")

  pipe$setURLPatterns(URLPatternsExpect)

  expect_equal(pipe$getURLPatterns(), URLPatternsExpect)

})

test_that("setURLPatterns namesURLPatterns input error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns)

  URLPatterns <- NULL

  expect_error(pipe$setURLPatterns(URLPatterns),"\\[FindUrlPipe\\]\\[setURLPatterns\\]\\[Error\\]
                Checking the type of the variable: URLPatterns NULL")

})

test_that("getNamesURLPatterns",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns)

  expect_equal(pipe$getNamesURLPatterns(),namesURLPatterns)

})

test_that("setNamesURLPatterns",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns)

  namesURLPatternsExpect <- list("URL","EMAIL")

  pipe$setNamesURLPatterns(namesURLPatternsExpect)

  expect_equal(pipe$getNamesURLPatterns(), namesURLPatternsExpect)

})

test_that("setNamesURLPatterns namesURLPatterns input error",{
  skip_if_not_installed("rex")
  skip_if_not_installed("textutils")
  skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps, removeUrls, URLPatterns, namesURLPatterns)

  namesURLPatterns <- NULL

  expect_error(pipe$setNamesURLPatterns(namesURLPatterns),"\\[FindUrlPipe\\]\\[setNamesURLPatterns\\]\\[Error\\]
                Checking the type of the variable: namesURLPatterns NULL")

})
