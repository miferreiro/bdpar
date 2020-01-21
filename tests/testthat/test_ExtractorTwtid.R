context("ExtractorTwtid")

test_that("initialize path type error",{
  skip_if_not_installed("rtweet")
  skip_if_not_installed("rjson")
  path <- NULL
  expect_error(ExtractorTwtid$new(path),"\\[ExtractorTwtid\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: path NULL")
})
