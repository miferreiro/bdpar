context("ExtractorSms")

test_that("initialize",{
  skip_if_not_installed("readr")
  path <- "testFile.tsms"
  expect_silent(ExtractorSms$new(path))
})

test_that("initialize path type error",{
  skip_if_not_installed("readr")
  path <- NULL

  expect_error(ExtractorSms$new(path),"\\[ExtractorSms\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: path NULL")

})

test_that("obtainDate",{
  skip_if_not_installed("readr")
  path <- file.path("testFiles",
                    "testExtractorSms",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$obtainDate()
  expect_equal(instance$getDate(),"")

})

test_that("obtainSource",{
  skip_if_not_installed("readr")
  path <- file.path("testFiles",
                    "testExtractorSms",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$obtainSource()
  expect_equal(instance$.__enclos_env__$private$source,"Wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us")
  expect_equal(instance$.__enclos_env__$private$data,"Wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us")
})
