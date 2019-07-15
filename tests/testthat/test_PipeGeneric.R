context("PipeGeneric")

test_that("initialize",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName, alwaysBeforeDeps, notAfterDeps)
  expect_equal(pipe$.__enclos_env__$private$propertyName, propertyName)
  expect_equal(pipe$.__enclos_env__$private$alwaysBeforeDeps, alwaysBeforeDeps)
  expect_equal(pipe$.__enclos_env__$private$notAfterDeps, notAfterDeps)
})

test_that("initialize propertyName type error",{

  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(PipeGeneric$new(propertyName, alwaysBeforeDeps, notAfterDeps),"\\[PipeGeneric\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")

})

test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "example"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  expect_error(PipeGeneric$new(propertyName, alwaysBeforeDeps, notAfterDeps),"\\[PipeGeneric\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")

})

test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  expect_error(PipeGeneric$new(propertyName, alwaysBeforeDeps, notAfterDeps),"\\[PipeGeneric\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("pipe",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(PipeGeneric$new(propertyName, alwaysBeforeDeps, notAfterDeps)$pipe(NULL), "I'm an abstract interface method")

})

test_that("getPropertyName",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName, alwaysBeforeDeps, notAfterDeps)
  expect_equal(pipe$getPropertyName(), propertyName)

})

test_that("getAlwaysBeforeDeps",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName, alwaysBeforeDeps, notAfterDeps)
  expect_equal(pipe$getAlwaysBeforeDeps(), alwaysBeforeDeps)

})

test_that("getNotAfterDeps",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName, alwaysBeforeDeps, notAfterDeps)
  expect_equal(pipe$getNotAfterDeps(), notAfterDeps)

})

test_that("setPropertyName",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  propertyNameExpected <- "exampleExpected"

  pipe$setPropertyName(propertyNameExpected)
  expect_equal(pipe$getPropertyName(), propertyNameExpected)

})

test_that("setPropertyName propertyName type error",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  propertyNameExpected <- NULL

  expect_error(pipe$setPropertyName(propertyNameExpected),"\\[PipeGeneric\\]\\[setPropertyName\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("setAlwaysBeforeDeps",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  alwaysBeforeDepsExpected <- list("pipe")

  pipe$setAlwaysBeforeDeps(alwaysBeforeDepsExpected)
  expect_equal(pipe$getAlwaysBeforeDeps(), alwaysBeforeDepsExpected)

})

test_that("setAlwaysBeforeDeps alwaysBeforeDeps type error",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  alwaysBeforeDepsExpected <- NULL

  expect_error(pipe$setAlwaysBeforeDeps(alwaysBeforeDepsExpected),"\\[PipeGeneric\\]\\[setAlwaysBeforeDeps\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("setNotAfterDeps",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  notAfterDeps <- list("pipe")

  pipe$setNotAfterDeps(notAfterDeps)
  expect_equal(pipe$getNotAfterDeps(), notAfterDeps)

})

test_that("setNotAfterDeps notAfterDeps type error",{

  propertyName <- "example"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- PipeGeneric$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  notAfterDepsExpected <- NULL

  expect_error(pipe$setNotAfterDeps(notAfterDepsExpected),"\\[PipeGeneric\\]\\[setNotAfterDeps\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")
})

