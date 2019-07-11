context("ExtractorTwtid")

test_that("initialize path type error",{

  path <- NULL

  expect_error(ExtractorTwtid$new(path),"\\[ExtractorTwtid\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: path NULL")

})
