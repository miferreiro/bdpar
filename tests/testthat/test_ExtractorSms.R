context("ExtractorSms")

test_that("initialize",{

  path <- "example.tsms"
  expect_silent(ExtractorSms$new(path))
})

test_that("initialize path type error",{

  path <- NULL

  expect_error(ExtractorSms$new(path),"\\[ExtractorSms\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: path NULL")

})

test_that("obtainDate",{

  path <- system.file("testFiles_ExtractorSms",
                      "example.tsms",
                      package = "bdpar")
  instance <- ExtractorSms$new(path)
  instance$obtainDate()
  expect_equal(instance$getDate(),"")

})

test_that("obtainSource",{

  path <- system.file("testFiles_ExtractorSms",
                      "example.tsms",
                      package = "bdpar")
  instance <- ExtractorSms$new(path)
  instance$obtainSource()
  expect_equal(instance$getSource(),"example file")
  expect_equal(instance$getData(),"example file")

})
