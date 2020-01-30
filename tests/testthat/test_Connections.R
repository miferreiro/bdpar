testthat::context("Connections")

test_that("startConnectionWithTwitter connectionWithTwitter=FALSE",{
  testthat::skip_if_not_installed("rtweet")

  connection <- Connections$new()

  testthat::expect_error(connection$startConnectionWithTwitter(),
                         "[Connections][startConnectionWithTwitter][Error] Twitter API keys are not defined on bdpar.Options",
                         fixed = TRUE)
})

testthat::test_that("startConnectionWithTwitter connectionWithTwitter=TRUE",{
  testthat::skip_if_not_installed("rtweet")

  connection <- Connections$new()

  connection$.__enclos_env__$private$connectionWithTwitter <- TRUE

  testthat::expect_null(connection$startConnectionWithTwitter())
})

testthat::test_that("startConnectionWithYoutube connectionWithYoutube=FALSE",{
  testthat::skip_if_not_installed("tuber")

  connection <- Connections$new()

  testthat::expect_error(connection$startConnectionWithYoutube(),
                         "[Connections][startConnectionWithYoutube][Error] Youtube API keys are not defined on bdpar.Options",
                         fixed = TRUE)
})

testthat::test_that("startConnectionWithYoutube connectionWithYoutube=TRUE",{
  testthat::skip_if_not_installed("tuber")

  connection <- Connections$new()

  connection$.__enclos_env__$private$connectionWithYoutube <- TRUE

  testthat::expect_null(connection$startConnectionWithYoutube())
})

testthat::test_that("addNumRequestToYoutube",{
  #Path where the configuration file are located
  connection <- Connections$new()

  connection$addNumRequestToYoutube()

  testthat::expect_equal(connection$.__enclos_env__$private$numRequestToYoutube,
                         1)
})

testthat::test_that("checkRequestToYoutube numRequest < numRequestMax",{
  testthat::skip_if_not_installed("tuber")

  connection <- Connections$new()

  connection$.__enclos_env__$private$numRequestToYoutube <- 0
  connection$.__enclos_env__$private$numRequestMaxToYoutube <- 1

  testthat::expect_null(connection$checkRequestToYoutube())
})

testthat::test_that("getNumRequestMaxToYoutube",{
  testthat::skip_if_not_installed("tuber")

  connection <- Connections$new()

  testthat::expect_type(connection$getNumRequestMaxToYoutube(), "double")
})
