testthat::context("GenericPipeline")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize",{
  testthat::expect_silent(GenericPipeline$new())
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("execute",{

  generic <- GenericPipeline$new()

  testthat::expect_error(generic$execute(),
                         "[GenericPipeline][execute][FATAL] I am an abstract interface method",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("get",{

  generic <- GenericPipeline$new()

  testthat::expect_error(generic$get(),
                         "[GenericPipeline][get][FATAL] I am an abstract interface method",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
