testthat::context("BdparOptions")

testthat::test_that("initialize",{
  testthat::expect_silent(BdparOptions$new())
})

testthat::test_that("get",{
  options <- BdparOptions$new()
  testthat::expect_null(options$get("twitter.consumer.key"))
})

testthat::test_that("get key type error",{
  options <- BdparOptions$new()
  testthat::expect_error(options$get(1),
                         "[BdparOptions][get][Error] Checking the type of the 'key' variable: numeric",
                         fixed = TRUE)
  testthat::expect_error(options$get("a"),
                         "[BdparOptions][get][Error] 'a' option is not configured",
                         fixed = TRUE)

})

testthat::setup(bdpar.Options$reset())

testthat::test_that("add",{
  options <- BdparOptions$new()
  testthat::expect_invisible(options$add("new", 2))
  testthat::expect_equal(options$get("new"), 2)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("add key type error",{
  options <- BdparOptions$new()
  testthat::expect_error(options$add(1,1),
                         "[BdparOptions][add][Error] Checking the type of the 'key' variable: numeric",
                         fixed = TRUE)
  testthat::expect_error(options$add("resources.abbreviations.path", 2),
                         "[BdparOptions][add][Error] 'resources.abbreviations.path' option is already configured with the value: ",
                         fixed = TRUE)

})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("set",{
  options <- BdparOptions$new()
  testthat::expect_invisible(options$set("resources.abbreviations.path", 2))
  testthat::expect_equal(options$get("resources.abbreviations.path"), 2)

  testthat::expect_invisible(options$set("resources.abbreviations.path", NULL))
  testthat::expect_null(options$get("resources.abbreviations.path"))
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("set key type error",{
  options <- BdparOptions$new()
  testthat::expect_error(options$set(1,1),
                         "[BdparOptions][set][Error] Checking the type of the 'key' variable: numeric",
                         fixed = TRUE)
  testthat::expect_error(options$set("a", 2),
                         "[BdparOptions][set][Error] 'a' option is not configured",
                         fixed = TRUE)

})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("remove",{
  options <- BdparOptions$new()
  testthat::expect_invisible(options$remove("resources.abbreviations.path"))
  testthat::expect_error(options$get("resources.abbreviations.path"))
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("remove key type error",{
  options <- BdparOptions$new()
  testthat::expect_error(options$remove(1),
                         "[BdparOptions][remove][Error] Checking the type of the 'key' variable: numeric",
                         fixed = TRUE)
  testthat::expect_error(options$remove("a"),
                         "[BdparOptions][remove][Error] 'a' option is not configured",
                         fixed = TRUE)

})

testthat::teardown(bdpar.Options$reset())

testthat::test_that("getAll",{
  options <- BdparOptions$new()
  testthat::expect_type(options$getAll(),"list")
})

testthat::test_that("reset",{
  options <- BdparOptions$new()

  op <- options$getAll()
  options$add("new", 2)
  testthat::expect_invisible(options$reset())
  testthat::expect_equal(options$getAll(), op)
})

testthat::test_that("isSpecificOption",{
  options <- BdparOptions$new()
  testthat::expect_true(options$isSpecificOption("resources.abbreviations.path"))
  testthat::expect_false(options$isSpecificOption("a"))
})

testthat::test_that("print",{
  options <- BdparOptions$new()
  testthat::expect_output(print(options), "[A-Za-z0-9$/.]+")
})

testthat::setup(bdpar.Options$reset())

testthat::test_that("cleanCache works",{

  bdpar.Options$set("cache", TRUE)
  cache.path <- file.path("testFiles",
                          "testBdparOptions",
                          ".cacheTestCleanCache")

  bdpar.Options$set("cache.folder", cache.path)

  pipeline <- DynamicPipeline$new()
  pipeline$add(list(TargetAssigningPipe$new(),  StoreFileExtPipe$new()),
               pos = NULL)

  path <- file.path("testFiles",
                    "testBdparOptions",
                    "tsms")

  runPipeline(path = path,
              extractors = ExtractorFactory$new(),
              pipeline = pipeline)

  testthat::expect_true(dir.exists(cache.path))

  testthat::expect_message(bdpar.Options$cleanCache(),
                           paste0("[cleanCache][Info] The cache folder \"",
                                  cache.path,
                                  "\" has been deleted successfully!"),
                           fixed = TRUE)

  testthat::expect_true(!dir.exists(cache.path))
})

testthat::teardown({
  cache.path <- file.path("testFiles",
                          "testBdparOptions",
                          ".cacheTestCleanCache")
  unlink(cache.path, recursive = T)
  bdpar.Options$reset()
})

testthat::setup(bdpar.Options$reset())

testthat::test_that("cleanCache not defined cache.folder field in bdpar.Options variable",{

  cache.path <- NULL
  bdpar.Options$set("cache.folder", cache.path)

  testthat::expect_error(bdpar.Options$cleanCache(),
                         "[cleanCache][Error] Cache folder is not defined in bdpar.Options",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
