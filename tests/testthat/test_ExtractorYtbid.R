context("ExtractorYtbid")

test_that("initialize path type error",{
  skip_if_not_installed("tuber")
  skip_if_not_installed("rjson")
  path <- NULL

  expect_error(ExtractorYtbid$new(path),"\\[ExtractorYtbid\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: path NULL")

})
