context("StoreFileExtPipe")

test_that("initialize",{

  propertyName <- "extension"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_silent(StoreFileExtPipe$new(propertyName,alwaysBeforeDeps,notAfterDeps))
})

test_that("initialize propertyName type error",{

  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(StoreFileExtPipe$new(propertyName,alwaysBeforeDeps,notAfterDeps),"\\[StoreFileExtPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "extension"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  expect_error(StoreFileExtPipe$new(propertyName,alwaysBeforeDeps,notAfterDeps),"\\[StoreFileExtPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{

  propertyName <- "extension"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  expect_error(StoreFileExtPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps),"\\[StoreFileExtPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("pipe",{

  propertyName <- "extension"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- StoreFileExtPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)

  expect_equal(pipe$pipe(instance)$getSpecificProperty("extension"),"tsms")


})

test_that("pipe Bad compatibility between Pipes.",{

  propertyName <- "extension"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  pipe <- StoreFileExtPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  expect_error(pipe$pipe(instance),"\\[StoreFileExtPipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- StoreFileExtPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  instance <- NULL
  expect_error(pipe$pipe(instance),"\\[StoreFileExtPipe\\]\\[pipe\\]\\[Error\\]
                Checking the type of the variable: instance NULL")

})

test_that("pipe",{

  propertyName <- "extension"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- StoreFileExtPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- "example/exam"

  instance <- ExtractorSms$new(path)

  expect_output(pipe$pipe(instance),"\\[StoreFileExtPipe\\]\\[pipe\\]\\[Warning\\]  The file:  example/exam  has not an extension")


})

test_that("obtainExtension",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- StoreFileExtPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- "example/exam.exe"
  expect_equal(pipe$obtainExtension(path), "exe")
})


test_that("obtainExtension path type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- StoreFileExtPipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- NULL
  expect_error(pipe$obtainExtension(path),"\\[StoreFileExtPipe\\]\\[obtainExtension\\]\\[Error\\]
                  Checking the type of the variable: path NULL")
})
