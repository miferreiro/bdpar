testthat::context("GenericPipeline")

testthat::test_that("initialize",{
  testthat::expect_silent(GenericPipeline$new())
})

testthat::test_that("execute",{

  generic <- GenericPipeline$new()

  testthat::expect_error(generic$execute(),
                         "[GenericPipeline][execute][Error] I am an abstract interface method",
                         fixed = TRUE)
})

testthat::test_that("get",{

  generic <- GenericPipeline$new()

  testthat::expect_error(generic$get(),
                         "[GenericPipeline][get][Error] I am an abstract interface method",
                         fixed = TRUE)
})
