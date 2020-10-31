testthat::context("FindUrlPipe")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  testthat::expect_silent(FindUrlPipe$new(propertyName,
                                          alwaysBeforeDeps,
                                          notAfterDeps,
                                          removeUrls,
                                          URLPatterns,
                                          namesURLPatterns))
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
  testthat::skip_if_not_installed("stringr")
  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  testthat::expect_error(FindUrlPipe$new(propertyName,
                                         alwaysBeforeDeps,
                                         notAfterDeps,
                                         removeUrls,
                                         URLPatterns,
                                         namesURLPatterns),
                         "[FindUrlPipe][initialize][FATAL] Checking the type of the 'propertyName' variable: NULL",
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
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  testthat::expect_error(FindUrlPipe$new(propertyName,
                                         alwaysBeforeDeps,
                                         notAfterDeps,
                                         removeUrls,
                                         URLPatterns,
                                         namesURLPatterns),
                         "[FindUrlPipe][initialize][FATAL] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
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
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  testthat::expect_error(FindUrlPipe$new(propertyName,
                                         alwaysBeforeDeps,
                                         notAfterDeps,
                                         removeUrls,
                                         URLPatterns,
                                         namesURLPatterns),
                         "[FindUrlPipe][initialize][FATAL] Checking the type of the 'notAfterDeps' variable: NULL",
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

testthat::test_that("initialize removeUrls type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- NULL
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  testthat::expect_error(FindUrlPipe$new(propertyName,
                                         alwaysBeforeDeps,
                                         notAfterDeps,
                                         removeUrls,
                                         URLPatterns,
                                         namesURLPatterns),
                         "[FindUrlPipe][initialize][FATAL] Checking the type of the 'removeUrls' variable: NULL",
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

testthat::test_that("initialize URLPatterns type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPatterns <- NULL
  namesURLPatterns <- list("UrlPattern")

  testthat::expect_error(FindUrlPipe$new(propertyName,
                                         alwaysBeforeDeps,
                                         notAfterDeps,
                                         removeUrls,
                                         URLPatterns,
                                         namesURLPatterns),
                         "[FindUrlPipe][initialize][FATAL] Checking the type of the 'URLPatterns' variable: NULL",
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

testthat::test_that("initialize namesURLPatterns type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- NULL

  testthat::expect_error(FindUrlPipe$new(propertyName,
                                         alwaysBeforeDeps,
                                         notAfterDeps,
                                         removeUrls,
                                         URLPatterns,
                                         namesURLPatterns),
                         "[FindUrlPipe][initialize][FATAL] Checking the type of the 'namesURLPatterns' variable: NULL",
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

testthat::test_that("pipe removeUrl <- TRUE",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps,
                          removeUrls,
                          URLPatterns,
                          namesURLPatterns)

  path <- file.path("testFiles",
                    "testFindUrlPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("example www.google.com")
  instance <- pipe$pipe(instance)
  testthat::expect_equal(instance$getSpecificProperty("URLs"),
                         c(UrlPattern = c("www.google.com")))
  testthat::expect_equal(instance$getData(),
                         "example")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("pipe removeUrl <- FALSE",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- FALSE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps,
                          removeUrls,
                          URLPatterns,
                          namesURLPatterns)

  path <- file.path("testFiles",
                    "testFindUrlPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("www.google.com")
  instance <- pipe$pipe(instance)
  testthat::expect_equal(instance$getSpecificProperty("URLs"),
                         c(UrlPattern = c("www.google.com")))
  testthat::expect_equal(instance$getData(),
                         "www.google.com")
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
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps,
                          removeUrls,
                          URLPatterns,
                          namesURLPatterns)

  path <- file.path("testFiles",
                    "testFindUrlPipe",
                    "testFile.tsms")

  instance <- NULL

  testthat::expect_error(pipe$pipe(instance),
                         "[FindUrlPipe][pipe][FATAL] Checking the type of the 'instance' variable: NULL",
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

testthat::test_that("pipe empty data",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps,
                          removeUrls,
                          URLPatterns,
                          namesURLPatterns)

  path <- file.path("testFiles",
                    "testFindUrlPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$setData("www.google.com")
  testthat::expect_warning(pipe$pipe(instance),
                           "\\[FindUrlPipe\\]\\[pipe\\]\\[WARN\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFiles\\/testFindUrlPipe\\/testFile\\.tsms has data empty on pipe Url")
  testthat::expect_equal(instance$getSpecificProperty("URLs"),
                         c(UrlPattern = c("www.google.com")))
  testthat::expect_equal(instance$getData(),
                         "")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("findUrl",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
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

  testthat::expect_equal(pipe$findUrl(pattern, data),
                         "www.google.com")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("findUrl pattern type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps,
                          removeUrls,
                          URLPatterns,
                          namesURLPatterns)

  pattern <- NULL
  data <- "www.google.com"

  testthat::expect_error(pipe$findUrl(pattern, data),
                         "[FindUrlPipe][findUrl][FATAL] Checking the type of the 'pattern' variable: NULL",
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

testthat::test_that("findUrl data type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps,
                          removeUrls,
                          URLPatterns,
                          namesURLPatterns)

  pattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  data <- NULL

  testthat::expect_error(pipe$findUrl(pattern,
                                      data),
                         "[FindUrlPipe][findUrl][FATAL] Checking the type of the 'data' variable: NULL",
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

testthat::test_that("removeUrl",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps,
                          removeUrls,
                          URLPatterns,
                          namesURLPatterns)

  pattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  data <- "www.google.com"

  testthat::expect_equal(pipe$removeUrl(pattern,
                                        data),
                         " ")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("removeUrl pattern type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps,
                          removeUrls,
                          URLPatterns,
                          namesURLPatterns)

  pattern <- NULL
  data <- "www.google.com"

  testthat::expect_error(pipe$removeUrl(pattern,
                                        data),
                         "[FindUrlPipe][removeUrl][FATAL] Checking the type of the 'pattern' variable: NULL",
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

testthat::test_that("removeUrl data type error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps,
                          removeUrls,
                          URLPatterns,
                          namesURLPatterns)

  pattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  data <- NULL

  testthat::expect_error(pipe$removeUrl(pattern, data),
                         "[FindUrlPipe][removeUrl][FATAL] Checking the type of the 'data' variable: NULL",
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

testthat::test_that("putNamesURLPattern",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps,
                          removeUrls,
                          URLPatterns,
                          namesURLPatterns)

  pipe$setNamesURLPatterns(list("URL","EMAIL"))
  resultOfURLPatterns <- list(c("www.google.com"),c("mm@gmail.com"))
  testthat::expect_equal(pipe$putNamesURLPattern(resultOfURLPatterns),
                         list(URL = c("www.google.com"),"EMAIL" = c("mm@gmail.com")))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("putNamesURLPattern resultOfURLPatterns input error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps,
                          removeUrls,
                          URLPatterns,
                          namesURLPatterns)
  pipe$setNamesURLPatterns(list("URL","EMAIL"))
  resultOfURLPatterns <- NULL

  testthat::expect_error(pipe$putNamesURLPattern(resultOfURLPatterns),
                         "[FindUrlPipe][putNamesURLPattern][FATAL] Checking the type of the 'resultOfURLPatterns' variable: NULL",
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

testthat::test_that("getURLPatterns",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps,
                          removeUrls,
                          URLPatterns,
                          namesURLPatterns)

  testthat::expect_equal(pipe$getURLPatterns(),
                         URLPatterns)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("setURLPatterns",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps,
                          removeUrls,
                          URLPatterns,
                          namesURLPatterns)

  URLPatternsExpect <- list("[:alnum:]*","[-]+")

  pipe$setURLPatterns(URLPatternsExpect)

  testthat::expect_equal(pipe$getURLPatterns(),
                         URLPatternsExpect)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("setURLPatterns namesURLPatterns input error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps,
                          removeUrls,
                          URLPatterns,
                          namesURLPatterns)

  URLPatterns <- NULL

  testthat::expect_error(pipe$setURLPatterns(URLPatterns),
                         "[FindUrlPipe][setURLPatterns][FATAL] Checking the type of the 'URLPatterns' variable: NULL",
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

testthat::test_that("getNamesURLPatterns",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps,
                          removeUrls,
                          URLPatterns,
                          namesURLPatterns)

  testthat::expect_equal(pipe$getNamesURLPatterns(),
                         namesURLPatterns)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("setNamesURLPatterns",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps,
                          removeUrls,
                          URLPatterns,
                          namesURLPatterns)

  namesURLPatternsExpect <- list("URL","EMAIL")

  pipe$setNamesURLPatterns(namesURLPatternsExpect)

  testthat::expect_equal(pipe$getNamesURLPatterns(),
                         namesURLPatternsExpect)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("setNamesURLPatterns namesURLPatterns input error",{
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("textutils")
  testthat::skip_if_not_installed("stringr")
  propertyName <- "URLs"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  removeUrls <- TRUE
  URLPattern <- "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))"
  URLPatterns <- list(URLPattern)
  namesURLPatterns <- list("UrlPattern")

  pipe <- FindUrlPipe$new(propertyName,
                          alwaysBeforeDeps,
                          notAfterDeps,
                          removeUrls,
                          URLPatterns,
                          namesURLPatterns)

  namesURLPatterns <- NULL

  testthat::expect_error(pipe$setNamesURLPatterns(namesURLPatterns),
                         "[FindUrlPipe][setNamesURLPatterns][FATAL] Checking the type of the 'namesURLPatterns' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
