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

  path <- system.file(file.path("testFiles_ExtractorSms",
                                "example.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instanceExpected <- ExtractorSms$new(path)
  instanceExpected$setSource("example file")
  instanceExpected$setData("example file")
  instanceExpected$addFlowPipes("File2Pipe")
  expect_equal(pipe$pipe(instance),instanceExpected)

})

test_that("pipe empty source",{

  propertyName <- "source"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- File2Pipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- system.file(file.path("testFiles_ExtractorSms",
                                "example_File2Pipe_empty.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  expect_output(pipe$pipe(instance),"\\[File2Pipe\\]\\[pipe\\]\\[Warning\\]  The file:  [\\:[:alnum:]\\/_-]*testFiles_ExtractorSms\\/example_File2Pipe_empty\\.tsms  has source empty")

})

test_that("pipe Bad compatibility between Pipes.",{

  propertyName <- "source"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  pipe <- File2Pipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package <- "bdpar")

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
