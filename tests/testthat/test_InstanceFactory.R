testthat::context("InstanceFactory")

testthat::test_that("initialize",{

  testthat::expect_silent(InstanceFactory$new())
})

testthat::test_that("createInstance NULL",{

  factory <- InstanceFactory$new()
  testthat::expect_null(factory$createInstance("example.exa"))
})

testthat::test_that("createInstance path type error",{

  path <- NULL
  testthat::expect_error(InstanceFactory$new()$createInstance(path),
                         "[InstanceFactory][createInstance][Error] Checking the type of the 'path' variable: NULL",
                         fixed = TRUE)

})

testthat::setup(bdpar.Options$reset())

testthat::test_that("createInstance tsms",{
  testthat::skip_if_not_installed("readr")
  factory <- InstanceFactory$new()

  bdpar.Options$set("eml.PartSelectedOnMPAlternative", "text/plain")

  testthat::expect_equal(class(factory$createInstance("example.tsms")),
                         c("ExtractorSms","Instance","R6"))

})

testthat::test_that("createInstance eml",{

  factory <- InstanceFactory$new()
  testthat::expect_equal(class(factory$createInstance("example.eml")),
                         c("ExtractorEml","Instance","R6"))

})

testthat::teardown(bdpar.Options$reset())

# test_that("createInstance twtid",{
#
#   factory <- InstanceFactory$new()
#
#   path <- system.file("testFiles_InstanceFactory",
#                       "example.twtid",
#                       package = "bdpar")
#
#   expect_equal(class(factory$createInstance(path)), c("ExtractorTwtid","Instance","R6"))
#
# })

# test_that("createInstance ytbid",{
#
#   factory <- InstanceFactory$new()
#
#   path <- system.file("testFiles_InstanceFactory",
#                       "example.ytbid",
#                       package = "bdpar")
#
#   expect_equal(class(factory$createInstance(path)), c("ExtractorYtbid","Instance","R6"))
#
# })
