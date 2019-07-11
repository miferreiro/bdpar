context("ResourceHandler")

test_that("initialize",{

  expect_silent(ResourceHandler$new())
})

test_that("isLoadResource loaded",{

  resourceHandler <- ResourceHandler$new()
  resourceHandler$setResources(list(exampleResource = list("aa","bb")))

  expect_equal(resourceHandler$isLoadResource("exampleResource"), list("aa","bb"))

})

test_that("isLoadResource file not exists",{

  resourceHandler <- ResourceHandler$new()

  expect_equal(resourceHandler$isLoadResource("example"), NULL)

})

test_that("isLoadResource file exists",{

  resourceHandler <- ResourceHandler$new()

  pathResource <- system.file(file.path("testResources",
                                        "abbreviations-json",
                                        "abbrev.en.json"),
                                       package = "bdpar")

  expect_length(resourceHandler$isLoadResource(pathResource), 1235)

})

test_that("isLoadResource pathResource type error",{

  resourceHandler <- ResourceHandler$new()
  pathResource <- NULL

  expect_error(resourceHandler$isLoadResource(pathResource),"\\[ResourceHandler\\]\\[isLoadResource\\]\\[Error\\]
                Checking the type of the variable: pathResource NULL")
})

test_that("getResources",{

  resourceHandler <- ResourceHandler$new()

  expect_equal(resourceHandler$getResources(), list())

})

test_that("setResources",{

  resourceHandler <- ResourceHandler$new()
  resourceHandler$setResources(list(exampleResource = list("aa","bb")))

  expect_equal(resourceHandler$getResources(), list(exampleResource = list("aa","bb")))

})

test_that("getNamesResources",{

  resourceHandler <- ResourceHandler$new()
  resourceHandler$setResources(list(exampleResource = list("aa","bb")))

  expect_equal(resourceHandler$getNamesResources(), c("exampleResource"))

})
