context("TeeCSVPipe")

test_that("initialize",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_silent(TeeCSVPipe$new(propertyName,alwaysBeforeDeps,notAfterDeps))
})

test_that("initialize propertyName type error",{

  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(TeeCSVPipe$new(propertyName,alwaysBeforeDeps,notAfterDeps),"\\[TeeCSVPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- ""
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  expect_error(TeeCSVPipe$new(propertyName,alwaysBeforeDeps,notAfterDeps),"\\[TeeCSVPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  expect_error(TeeCSVPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps),"\\[TeeCSVPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("pipe",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TeeCSVPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)

  withData <- TRUE
  withSource <- TRUE
  expect_equal(pipe$pipe(instance, withData, withSource), instance)
  expect_equal(file.exists("output_tsms.csv"), TRUE)
  file.remove("output_tsms.csv")
})

test_that("pipe instance invalid",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TeeCSVPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)

  withData <- TRUE
  withSource <- TRUE
  instance$invalidate()
  expect_equal(pipe$pipe(instance, withData, withSource), instance)
  expect_equal(file.exists("output_tsms.csv"), FALSE)

})

test_that("pipe Bad compatibility between Pipes.",{

  propertyName <- ""
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()

  pipe <- TeeCSVPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  withData <- TRUE
  withSource <- TRUE
  instance$addBanPipes("pipeExample")
  expect_error(pipe$pipe(instance, withData, withSource),"\\[TeeCSVPipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TeeCSVPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))
  instance <- NULL
  withData <- TRUE
  withSource <- TRUE
  expect_error(pipe$pipe(instance, withData, withSource),"\\[TeeCSVPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("pipe withData type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TeeCSVPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)

  withData <- NULL
  withSource <- TRUE
  expect_error(pipe$pipe(instance, withData, withSource),"\\[TeeCSVPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: withData NULL")

})

test_that("pipe withSource type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TeeCSVPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  Bdpar$new(configurationFilePath = system.file("configurations",
                                                "test_pipeline_execute_tsms_configurations.ini",
                                                package = "bdpar"))

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)

  withData <- TRUE
  withSource <- NULL
  expect_error(pipe$pipe(instance, withData, withSource),"\\[TeeCSVPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: withSource NULL")

})
