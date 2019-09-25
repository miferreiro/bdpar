context("File2Pipe")

test_that("initialize",{

  propertyName <- "source"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_silent(File2Pipe$new(propertyName,alwaysBeforeDeps,notAfterDeps))
})

test_that("initialize propertyName type error",{

  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(File2Pipe$new(propertyName,alwaysBeforeDeps,notAfterDeps),"\\[File2Pipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "source"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  expect_error(File2Pipe$new(propertyName,alwaysBeforeDeps,notAfterDeps),"\\[File2Pipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{

  propertyName <- "source"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  expect_error(File2Pipe$new(propertyName, alwaysBeforeDeps, notAfterDeps),"\\[File2Pipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("pipe",{

  propertyName <- "source"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- File2Pipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testFile2Pipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instanceExpected <- ExtractorSms$new(path)
  instanceExpected$setSource("Wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us\r\n")
  instanceExpected$setData("Wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us\r\n")
  instanceExpected$addFlowPipes("File2Pipe")
  expect_equal(pipe$pipe(instance),instanceExpected)

})

test_that("pipe empty source",{

  propertyName <- "source"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- File2Pipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testFile2Pipe",
                    "example_File2Pipe_empty.tsms")

  instance <- ExtractorSms$new(path)
  expect_warning(pipe$pipe(instance),"\\[File2Pipe\\]\\[pipe\\]\\[Warning\\] The file: [\\\\\\:[:alnum:]\\/_.-]*testFile2Pipe\\/example_File2Pipe_empty\\.tsms has source empty")

})

test_that("pipe Bad compatibility between Pipes.",{

  propertyName <- "source"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  pipe <- File2Pipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testFile2Pipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  expect_error(pipe$pipe(instance),"\\[File2Pipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{

  propertyName <- "source"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- File2Pipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  instance <- NULL

  expect_error(pipe$pipe(instance),"\\[File2Pipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})
