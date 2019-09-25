context("ExtractorSms")

test_that("initialize",{

  path <- "testFile.tsms"
  expect_silent(ExtractorSms$new(path))
})

test_that("initialize path type error",{

  path <- NULL

  expect_error(ExtractorSms$new(path),"\\[ExtractorSms\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: path NULL")

})

test_that("obtainDate",{

  path <- file.path("testFiles",
                    "testExtractorSms",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$obtainDate()
  expect_equal(instance$getDate(),"")

})

test_that("obtainSource",{

  path <- file.path("testFiles",
                    "testExtractorSms",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$obtainSource()
  expect_equal(instance$getSource(),"Wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us\r\n")
  expect_equal(instance$getData(),"Wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us\r\n")

})
