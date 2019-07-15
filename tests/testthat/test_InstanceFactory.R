context("InstanceFactory")

test_that("initialize",{

  expect_silent(InstanceFactory$new())
})

test_that("createInstance NULL",{

  factory <- InstanceFactory$new()
  expect_null(factory$createInstance("example.exa"))
})

test_that("createInstance path type error",{

  path <- NULL
  expect_error(InstanceFactory$new()$createInstance(path),"\\[InstanceFactory\\]\\[createInstance\\]\\[Error\\]
                Checking the type of the variable: path NULL")

})

test_that("createInstance tsms",{

  factory <- InstanceFactory$new()

  expect_equal(class(factory$createInstance("example.tsms")), c("ExtractorSms","Instance","R6"))

})

test_that("createInstance eml",{

  factory <- InstanceFactory$new()
  expect_equal(class(factory$createInstance("example.eml")), c("ExtractorEml","Instance","R6"))

})

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
