testthat::context("ResourceHandler")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize",{
  testthat::skip_if_not_installed("rjson")
  testthat::expect_silent(ResourceHandler$new())
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("isLoadResource loaded",{
  testthat::skip_if_not_installed("rjson")
  resourceHandler <- ResourceHandler$new()
  resourceHandler$setResources(list(exampleResource = list("aa","bb")))

  testthat::expect_equal(resourceHandler$isLoadResource("exampleResource"),
                         list("aa","bb"))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("isLoadResource file not exists",{
  testthat::skip_if_not_installed("rjson")
  resourceHandler <- ResourceHandler$new()

  testthat::expect_equal(resourceHandler$isLoadResource("example"),
                         NULL)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("isLoadResource file exists",{
  testthat::skip_if_not_installed("rjson")
  resourceHandler <- ResourceHandler$new()

  pathResource <- file.path("resourcesFiles",
                            "testResources",
                            "abbreviations-json",
                            "abbrev.en.json")

  testthat::expect_length(resourceHandler$isLoadResource(pathResource),
                          6)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("isLoadResource pathResource type error",{
  testthat::skip_if_not_installed("rjson")
  resourceHandler <- ResourceHandler$new()
  pathResource <- NULL

  testthat::expect_error(resourceHandler$isLoadResource(pathResource),
                         "[ResourceHandler][isLoadResource][FATAL] Checking the type of the 'pathResource' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("getResources",{
  testthat::skip_if_not_installed("rjson")
  resourceHandler <- ResourceHandler$new()

  testthat::expect_equal(resourceHandler$getResources(),
                         list())
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("setResources",{
  testthat::skip_if_not_installed("rjson")
  resourceHandler <- ResourceHandler$new()
  resourceHandler$setResources(list(exampleResource = list("aa","bb")))

  testthat::expect_equal(resourceHandler$getResources(),
                         list(exampleResource = list("aa","bb")))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("getNamesResources",{
  testthat::skip_if_not_installed("rjson")
  resourceHandler <- ResourceHandler$new()
  resourceHandler$setResources(list(exampleResource = list("aa","bb")))

  testthat::expect_equal(resourceHandler$getNamesResources(),
                         c("exampleResource"))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
