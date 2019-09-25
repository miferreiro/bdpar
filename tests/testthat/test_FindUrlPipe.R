context("FindUrlPipe")

test_that("initialize",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_silent(FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps))
})

test_that("initialize propertyName type error",{

  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps),"\\[FindUrlPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  expect_error(FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps),"\\[FindUrlPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  expect_error(FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps),"\\[FindUrlPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("pipe removeUrl <- FALSE",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testFindUrlPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("example www.google.com")
  removeUrl <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")
  instance <- pipe$pipe(instance, removeUrl, URLPatterns, namesURLPatterns)
  expect_equal(instance$getSpecificProperty("URLs"),c(UrlPattern = c("www.google.com")))
  expect_equal(instance$getData(),"example")

})
test_that("pipe removeUrl <- FALSE",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testFindUrlPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("www.google.com")
  removeUrl <- FALSE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")
  instance <- pipe$pipe(instance, removeUrl, URLPatterns, namesURLPatterns)
  expect_equal(instance$getSpecificProperty("URLs"),c(UrlPattern = c("www.google.com")))
  expect_equal(instance$getData(),"www.google.com")

})

test_that("pipe Bad compatibility between Pipes.",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testFindUrlPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("www.google.com")
  instance$addBanPipes("pipeExample")
  removeUrl <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")
  expect_error(pipe$pipe(instance, removeUrl, URLPatterns, namesURLPatterns),"\\[FindUrlPipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testFindUrlPipe",
                    "testFile.tsms")

  instance <- NULL

  removeUrl <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")
  expect_error(pipe$pipe(instance, removeUrl, URLPatterns, namesURLPatterns),"\\[FindUrlPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("pipe removeUrl type error",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testFindUrlPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("www.google.com")
  removeUrl <- NULL
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")
  expect_error(pipe$pipe(instance, removeUrl, URLPatterns, namesURLPatterns),"\\[FindUrlPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: removeUrl NULL")

})

test_that("pipe URLPatterns type error",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testFindUrlPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("www.google.com")
  removeUrl <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- NULL
  namesURLPatterns <- list("UrlPattern")
  expect_error(pipe$pipe(instance, removeUrl, URLPatterns, namesURLPatterns),"\\[FindUrlPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: URLPatterns NULL")

})

test_that("pipe namesURLPatterns type error",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testFindUrlPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("www.google.com")
  removeUrl <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- NULL
  expect_error(pipe$pipe(instance, removeUrl, URLPatterns, namesURLPatterns),"\\[FindUrlPipe\\]\\[pipe\\]\\[Error\\]
                 Checking the type of the variable: namesURLPatterns NULL")

})

test_that("pipe empty data",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testFindUrlPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("www.google.com")
  removeUrl <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")
  expect_warning(pipe$pipe(instance, removeUrl, URLPatterns, namesURLPatterns),"\\[FindUrlPipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testFindUrlPipe\\/testFile\\.tsms has data empty on pipe Url ")
  expect_equal(instance$getSpecificProperty("URLs"),c(UrlPattern = c("www.google.com")))
  expect_equal(instance$getData(),"")

})


test_that("findUrl",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  pattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  data <- "www.google.com"

  expect_equal(pipe$findUrl(pattern, data),"www.google.com")

})

test_that("findUrl pattern type error",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  pattern <- NULL
  data <- "www.google.com"

  expect_error(pipe$findUrl(pattern, data),"\\[FindUrlPipe\\]\\[findUrl\\]\\[Error\\]
                Checking the type of the variable: pattern NULL")

})

test_that("findUrl data type error",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  pattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  data <- NULL

  expect_error(pipe$findUrl(pattern, data),"\\[FindUrlPipe\\]\\[findUrl\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})

test_that("removeUrl",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  pattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  data <- "www.google.com"

  expect_equal(pipe$removeUrl(pattern, data)," ")

})

test_that("removeUrl pattern type error",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  pattern <- NULL
  data <- "www.google.com"

  expect_error(pipe$removeUrl(pattern, data),"\\[FindUrlPipe\\]\\[removeUrl\\]\\[Error\\]
                Checking the type of the variable: pattern NULL")

})

test_that("removeUrl data type error",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  pattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  data <- NULL

  expect_error(pipe$removeUrl(pattern, data),"\\[FindUrlPipe\\]\\[removeUrl\\]\\[Error\\]
                Checking the type of the variable: data NULL")

})

test_that("putNamesURLPattern",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)
  pipe$setNamesURLPatterns(list("URL","EMAIL"))
  resultOfURLPatterns <- list(c("www.google.com"),c("mm@gmail.com"))
  expect_equal(pipe$putNamesURLPattern(resultOfURLPatterns),list(URL = c("www.google.com"),"EMAIL" = c("mm@gmail.com")))

})

test_that("putNamesURLPattern resultOfURLPatterns input error",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)
  pipe$setNamesURLPatterns(list("URL","EMAIL"))
  resultOfURLPatterns <- NULL

  expect_error(pipe$putNamesURLPattern(resultOfURLPatterns),"\\[FindUrlPipe\\]\\[putNamesURLPattern\\]\\[Error\\]
                Checking the type of the variable: resultOfURLPatterns NULL")

})

test_that("getURLPatterns",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  expect_equal(pipe$getURLPatterns(),list())

})

test_that("setURLPatterns",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  URLPatternsExpect <- list("[:alnum:]*","[-]+")

  pipe$setURLPatterns(URLPatternsExpect)

  expect_equal(pipe$getURLPatterns(), URLPatternsExpect)

})

test_that("setURLPatterns namesURLPatterns input error",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  URLPatterns <- NULL

  expect_error(pipe$setURLPatterns(URLPatterns),"\\[FindUrlPipe\\]\\[setURLPatterns\\]\\[Error\\]
                Checking the type of the variable: URLPatterns NULL")

})

test_that("getNamesURLPatterns",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  expect_equal(pipe$getNamesURLPatterns(),list())

})

test_that("setNamesURLPatterns",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  namesURLPatternsExpect <- list("URL","EMAIL")

  pipe$setNamesURLPatterns(namesURLPatternsExpect)

  expect_equal(pipe$getNamesURLPatterns(), namesURLPatternsExpect)

})

test_that("setNamesURLPatterns namesURLPatterns input error",{

  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- FindUrlPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  namesURLPatterns <- NULL

  expect_error(pipe$setNamesURLPatterns(namesURLPatterns),"\\[FindUrlPipe\\]\\[setNamesURLPatterns\\]\\[Error\\]
                Checking the type of the variable: namesURLPatterns NULL")

})
