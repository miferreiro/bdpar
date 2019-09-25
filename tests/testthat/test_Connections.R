context("Connections")

test_that("initialize",{

  #Path where the configuration file are located
  configurationFilePath <-  file.path("testFiles",
                                      "testConnections",
                                      "configurations.ini")

  connection <- Connections$new(configurationFilePath)
  expect_type(connection$.__enclos_env__$private$keys, "list")
})

test_that("startConnectionWithYoutube connectionWithYoutube=FALSE",{

  #Path where the configuration file are located
  configurationFilePath <-  file.path("testFiles",
                                      "testConnections",
                                      "configurations.ini")

  connection <- Connections$new(configurationFilePath)


  expect_error(connection$startConnectionWithYoutube(), "Please provide values for app_id and app_secret")

})

test_that("startConnectionWithYoutube connectionWithYoutube=TRUE",{

  #Path where the configuration file are located
  configurationFilePath <-  file.path("testFiles",
                                      "testConnections",
                                      "configurations.ini")

  connection <- Connections$new(configurationFilePath)

  connection$.__enclos_env__$private$connectionWithYoutube = TRUE

  expect_null(connection$startConnectionWithYoutube())

})

test_that("addNumRequestToYoutube",{

  #Path where the configuration file are located
  configurationFilePath <-  file.path("testFiles",
                                      "testConnections",
                                      "configurations.ini")

  connection <- Connections$new(configurationFilePath)

  connection$addNumRequestToYoutube()

  expect_equal(connection$.__enclos_env__$private$numRequestToYoutube, 1)

})

test_that("checkRequestToYoutube numRequest < numRequestMax",{

  #Path where the configuration file are located
  configurationFilePath <-  file.path("testFiles",
                                      "testConnections",
                                      "configurations.ini")

  connection <- Connections$new(configurationFilePath)

  connection$.__enclos_env__$private$numRequestToYoutube <- 0
  connection$.__enclos_env__$private$numRequestMaxToYoutube <- 1

  expect_null(connection$checkRequestToYoutube())

})

# test_that("checkRequestToYoutube numRequest >= numRequestMax",{
#
#   #Path where the configuration file are located
#   configurationFilePath <-  system.file("configurations",
#                                         "test_Connections.ini",
#                                         package = "bdpar")
#
#   connection <- Connections$new(configurationFilePath)
#
#   connection$.__enclos_env__$private$numRequestToYoutube <- 1
#   connection$.__enclos_env__$private$numRequestMaxToYoutube <- 1
#
#   expect_message(connection$checkRequestToYoutube(), "Connections[Connections][checkRequestToYoutube][Info]  Waiting 15 min to be able to make new requests from youtube...")
#
# })

test_that("getNumRequestMaxToYoutube",{

  #Path where the configuration file are located
  configurationFilePath <-  file.path("testFiles",
                                      "testConnections",
                                      "configurations.ini")

  connection <- Connections$new(configurationFilePath)

  expect_type(connection$getNumRequestMaxToYoutube(), "double")

})


