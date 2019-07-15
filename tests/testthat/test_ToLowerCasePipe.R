context("ToLowerCasePipe")

test_that("initialize",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_silent(ToLowerCasePipe$new(propertyName,alwaysBeforeDeps,notAfterDeps))
})

test_that("initialize propertyName type error",{

  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(ToLowerCasePipe$new(propertyName,alwaysBeforeDeps,notAfterDeps),"\\[ToLowerCasePipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("initialize alwaysBeforeDeps type error",{

  propertyName <- ""
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  expect_error(ToLowerCasePipe$new(propertyName,alwaysBeforeDeps,notAfterDeps),"\\[ToLowerCasePipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")
})

test_that("initialize notAfterDeps type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  expect_error(ToLowerCasePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps),"\\[ToLowerCasePipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("pipe",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- ToLowerCasePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$setData("eXaMpLe")
  instance <- pipe$pipe(instance)
  expect_equal(instance$getData(),"example")

})

test_that("pipe Bad compatibility between Pipes.",{

  propertyName <- ""
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  pipe <- ToLowerCasePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- system.file(file.path("testFiles","_ham_",
                                "30.tsms"),
                      package = "bdpar")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  expect_error(pipe$pipe(instance),"\\[ToLowerCasePipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe instance type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- ToLowerCasePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  instance <- NULL
  expect_error(pipe$pipe(instance),"\\[ToLowerCasePipe\\]\\[pipe\\]\\[Error\\]
                  Checking the type of the variable: instance NULL")

})


test_that("toLowerCase",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- ToLowerCasePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  data <- "ExAmPlE"
  expect_equal(pipe$toLowerCase(data),"example")

})

test_that("toLowerCase data type error",{

  propertyName <- ""
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- ToLowerCasePipe$new(propertyName, alwaysBeforeDeps, notAfterDeps)

  data <- NULL
  expect_error(pipe$toLowerCase(data),"\\[ToLowerCasePipe\\]\\[toLowerCase\\]\\[Error\\]
                  Checking the type of the variable: data NULL")

})
