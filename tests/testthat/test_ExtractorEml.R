context("ExtractorEml")

test_that("initialize",{

  path <- "example.eml"
  expect_silent(ExtractorEml$new(path))
})

test_that("initialize path type error",{

  path <- NULL

  expect_error(ExtractorEml$new(path),"\\[ExtractorEml\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: path NULL")

})


# test_that("obtainDate",{
#
# })
#
# test_that("obtainSource",{
#
# })

test_that("getPartSelectedOnMPAlternative",{

  path <- "example.tsms"
  instance <- ExtractorEml$new(path)

  expect_equal(instance$getPartSelectedOnMPAlternative(),"text/plain")

})

test_that("setPartSelectedOnMPAlternative",{

  path <- "example.tsms"
  instance <- ExtractorEml$new(path)

  partSelectedExpect <- "text/html"

  instance$setPartSelectedOnMPAlternative(partSelectedExpect)

  expect_equal(instance$getPartSelectedOnMPAlternative(), partSelectedExpect)

})


test_that("setPartSelectedOnMPAlternative PartSelectedOnMPAlternative input error",{

  path <- "example.eml"
  instance <- ExtractorEml$new(path)

  partSelected <- NULL

  expect_error(instance$setPartSelectedOnMPAlternative(partSelected),"\\[ExtractorEml\\]\\[setPartSelectedOnMPAlternative\\]\\[Error\\]
                Checking the type of the variable: PartSelectedOnMPAlternative NULL")

})
