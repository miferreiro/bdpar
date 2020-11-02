testthat::context(".onLoad")

testthat::test_that(".onLoad works",{
  options <- BdparOptions$new()
  op <- options$getAll()
  bdpar:::.onLoad()
  testthat::expect_equal(bdpar.Options$getAll(), op)
})
