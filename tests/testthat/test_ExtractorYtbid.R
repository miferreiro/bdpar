context("ExtractorYtbid")

test_that("initialize path type error",{

  path <- NULL

  expect_error(ExtractorYtbid$new(path),"\\[ExtractorYtbid\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: path NULL")

})
