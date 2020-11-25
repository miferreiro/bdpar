testthat::context(".onLoad")

testthat::test_that(".onLoad works",{
  bdpar:::.onLoad()
  options <- BdparOptions$new()
  op <- options$getAll()
  testthat::expect_equal(bdpar.Options$getAll(), op)
  testthat::expect_equal(class(getOption("loggerSettings")$logger[[1]]),
                         "Logger")
})
