testthat::context("ExtractorFactory")

testthat::test_that("initialize",{

  testthat::expect_silent(ExtractorFactory$new())
})

testthat::test_that("createInstance NULL",{

  factory <- ExtractorFactory$new()
  testthat::expect_null(factory$createInstance("example.exa"))
})

testthat::test_that("createInstance path type error",{

  path <- NULL
  testthat::expect_error(ExtractorFactory$new()$createInstance(path),
                         "[ExtractorFactory][createInstance][Error] Checking the type of the 'path' variable: NULL",
                         fixed = TRUE)

})

testthat::test_that("createInstance tsms",{
  testthat::skip_if_not_installed("readr")
  factory <- ExtractorFactory$new()

  testthat::expect_equal(class(factory$createInstance("example.tsms")),
                         c("ExtractorSms","Instance","R6"))

})

testthat::test_that("createInstance eml",{

  factory <- ExtractorFactory$new()
  testthat::expect_equal(class(factory$createInstance("example.eml")),
                         c("ExtractorEml","Instance","R6"))
})

testthat::test_that("createInstance eml",{

  factory <- ExtractorFactory$new()
  testthat::expect_equal(class(factory$createInstance("example.eml")),
                         c("ExtractorEml","Instance","R6"))
})


testthat::test_that("createInstance eml",{

  factory <- ExtractorFactory$new()
  testthat::expect_equal(class(factory$createInstance("example.eml")),
                         c("ExtractorEml","Instance","R6"))
})

testthat::test_that("getAllExtractors",{
  extractors <- ExtractorFactory$new()
  testthat::expect_type(extractors$getAllExtractors(),"list")
})

testthat::test_that("registerExtractor",{
  extractors <- ExtractorFactory$new()
  testthat::expect_invisible(extractors$registerExtractor("json", ExtractorEml))
  testthat::expect_true(extractors$isSpecificExtractor("json"))
})

testthat::test_that("registerExtractor type error",{
  extractors <- ExtractorFactory$new()
  testthat::expect_error(extractors$registerExtractor(1,1),
                         "[ExtractorFactory][registerExtractor][Error] Checking the type of the 'extension' variable: numeric",
                         fixed = TRUE)

  testthat::expect_error(extractors$registerExtractor("eml", 2),
                         "[ExtractorFactory][registerExtractor][Error] 'eml' extension is already added",
                         fixed = TRUE)

  testthat::expect_error(extractors$registerExtractor("json", 2),
                         "[ExtractorFactory][registerExtractor][Error] Checking the type of the 'extractor' variable: numeric",
                         fixed = TRUE)
})

testthat::test_that("setExtractor",{
  extractors <- ExtractorFactory$new()
  testthat::expect_invisible(extractors$setExtractor("tsms", ExtractorSms))
  testthat::expect_true(extractors$isSpecificExtractor("tsms"))
})

testthat::test_that("setExtractor type error",{
  extractors <- ExtractorFactory$new()
  testthat::expect_error(extractors$setExtractor(1,1),
                         "[ExtractorFactory][setExtractor][Error] Checking the type of the 'extension' variable: numeric",
                         fixed = TRUE)

  testthat::expect_error(extractors$setExtractor("json", 2),
                         "[ExtractorFactory][setExtractor][Error] 'json' extension is not configured",
                         fixed = TRUE)

  testthat::expect_error(extractors$setExtractor("eml", 2),
                         "[ExtractorFactory][setExtractor][Error] Checking the type of the 'extractor' variable: numeric",
                         fixed = TRUE)
})

testthat::test_that("remove",{
  extractors <- ExtractorFactory$new()
  testthat::expect_invisible(extractors$removeExtractor("tsms"))
  testthat::expect_false(extractors$isSpecificExtractor("tsms"))
})

testthat::test_that("remove type error",{
  extractors <- ExtractorFactory$new()
  testthat::expect_error(extractors$removeExtractor(1),
                         "[ExtractorFactory][removeExtractor][Error] Checking the type of the 'extension' variable: numeric",
                         fixed = TRUE)
  testthat::expect_error(extractors$removeExtractor("a"),
                         "[ExtractorFactory][removeExtractor][Error] 'a' extension is not configured",
                         fixed = TRUE)
})

testthat::test_that("isSpecificExtractor",{
  extractors <- ExtractorFactory$new()
  testthat::expect_true(extractors$isSpecificExtractor("tsms"))
  testthat::expect_false(extractors$isSpecificExtractor("wrong"))
})

testthat::test_that("reset",{
  extractors <- ExtractorFactory$new()

  all <- extractors$getAllExtractors()
  extractors$registerExtractor("json", ExtractorEml)
  testthat::expect_invisible(extractors$reset())
  testthat::expect_equal(extractors$getAllExtractors(), all)
})

testthat::test_that("print",{
  extractors <- ExtractorFactory$new()
  testthat::expect_output(print(extractors), "[A-Za-z0-9$/.]+")
})
