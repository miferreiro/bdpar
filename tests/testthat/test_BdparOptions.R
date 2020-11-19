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
                         "[BdparOptions][get][FATAL] Checking the type of the 'key' variable: numeric",
                         fixed = TRUE)
  testthat::expect_error(options$get("a"),
                         "[BdparOptions][get][FATAL] 'a' option is not configured",
                         fixed = TRUE)

})

testthat::test_that("add",{
  options <- BdparOptions$new()
  testthat::expect_invisible(options$add("new", 2))
  testthat::expect_equal(options$get("new"), 2)
})

testthat::test_that("add key type error",{
  options <- BdparOptions$new()
  testthat::expect_error(options$add(1,1),
                         "[BdparOptions][add][FATAL] Checking the type of the 'key' variable: numeric",
                         fixed = TRUE)
  testthat::expect_error(options$add("resources.abbreviations.path", 2),
                         "[BdparOptions][add][FATAL] 'resources.abbreviations.path' option is already configured with the value: ",
                         fixed = TRUE)

})

testthat::test_that("set",{
  options <- BdparOptions$new()
  testthat::expect_invisible(options$set("resources.abbreviations.path", 2))
  testthat::expect_equal(options$get("resources.abbreviations.path"), 2)

  testthat::expect_invisible(options$set("resources.abbreviations.path", NULL))
  testthat::expect_null(options$get("resources.abbreviations.path"))
})

testthat::test_that("set key type error",{
  options <- BdparOptions$new()
  testthat::expect_error(options$set(1,1),
                         "[BdparOptions][set][FATAL] Checking the type of the 'key' variable: numeric",
                         fixed = TRUE)
  testthat::expect_error(options$set("a", 2),
                         "[BdparOptions][set][FATAL] 'a' option is not configured",
                         fixed = TRUE)
})

testthat::test_that("remove",{
  options <- BdparOptions$new()
  testthat::expect_invisible(options$remove("resources.abbreviations.path"))
  testthat::expect_error(options$get("resources.abbreviations.path"))
})

testthat::test_that("remove key type error",{
  options <- BdparOptions$new()
  testthat::expect_error(options$remove(1),
                         "[BdparOptions][remove][FATAL] Checking the type of the 'key' variable: numeric",
                         fixed = TRUE)
  testthat::expect_error(options$remove("a"),
                         "[BdparOptions][remove][FATAL] 'a' option is not configured",
                         fixed = TRUE)

})

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

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("cleanCache works",{

  options <- BdparOptions$new()

  options$set("cache", TRUE)
  bdpar.Options$set("cache", TRUE)
  cache.path <- file.path("testFiles",
                          "testBdparOptions",
                          ".cacheTestCleanCache")

  options$set("cache.folder", cache.path)
  bdpar.Options$set("cache.folder", cache.path)

  pipeline <- DynamicPipeline$new()
  pipeline$add(list(TargetAssigningPipe$new(), StoreFileExtPipe$new()),
               pos = NULL)

  path <- file.path("testFiles",
                    "testBdparOptions",
                    "tsms")

  runPipeline(path = path,
              extractors = ExtractorFactory$new(),
              pipeline = pipeline)

  testthat::expect_true(dir.exists(cache.path))

  testthat::expect_message(options$cleanCache(),
                           paste0("[BdparOptions][cleanCache][Info] The cache folder \"",
                                  cache.path,
                                  "\" has been deleted successfully!"),
                           fixed = TRUE)

  testthat::expect_true(!dir.exists(cache.path))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
  cache.path <- file.path("testFiles",
                          "testBdparOptions",
                          ".cacheTestCleanCache")
  unlink(cache.path, recursive = T)
})

testthat::test_that("cleanCache not defined cache.folder field",{

  options <- BdparOptions$new()

  cache.path <- NULL
  options$set("cache.folder", cache.path)

  testthat::expect_error(options$cleanCache(),
                         "[BdparOptions][cleanCache][Error] Cache folder is not defined in bdpar.Options",
                         fixed = TRUE)
})

testthat::test_that("configureLog works",{

  options <- BdparOptions$new()

  console <- TRUE
  threshold <- "INFO"
  log.file <- file.path("testFiles",
                        "testBdparOptions",
                        "log.txt")

  options$configureLog(console = console,
                       threshold = threshold,
                       file = log.file)

  testthat::expect_message(options$getLogConfiguration(),
                           "[BdparOptions][getLogConfiguration][INFO] Log configuration:
	- Threshold: INFO
	- Console log status: Actived
	- File log status: Actived",
                           fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$configureLog()
  log.file <- file.path("testFiles",
                        "testBdparOptions",
                        "log.txt")
  unlink(log.file, recursive = T)
})

testthat::test_that("configureLog mode file works",{

  options <- BdparOptions$new()

  console <- TRUE
  threshold <- "INFO"
  log.file <- file.path("testFiles",
                        "testBdparOptions",
                        "log.txt")

  options$configureLog(console = console,
                       threshold = threshold,
                       file = log.file)

  bdpar.Options$configureLog(console = console,
                             threshold = threshold,
                             file = log.file)

  message <- "message"
  level <- "INFO"
  className <- "className"
  methodName <- "methodName"

  bdpar.log(message, level, className, methodName)

  testthat::expect_true(file.exists(log.file))

  testthat::expect_match(readLines(log.file),
                         "[-\\[\\]:0-9 ]+\\[className\\]\\[methodName\\]\\[INFO\\] message",
                         perl = TRUE)
})

testthat::teardown({
  bdpar.Options$configureLog()
  log.file <- file.path("testFiles",
                        "testBdparOptions",
                        "log.txt")
  unlink(log.file, recursive = T)
})

testthat::test_that("configureLog error threshold argument",{

  options <- BdparOptions$new()

  console <- TRUE
  file <- NULL

  threshold <- NULL

  testthat::expect_error(options$configureLog(console = console,
                                                threshold = threshold,
                                                file = file),
                           "[BdparOptions][configureLog][FATAL] The 'threshold' parameter must be between these values: FATAL, ERROR, WARN, INFO or DEBUG",
                           fixed = TRUE)

  threshold <- 1

  testthat::expect_error(options$configureLog(console = console,
                                              threshold = threshold,
                                              file = file),
                         "[BdparOptions][configureLog][FATAL] The 'threshold' parameter must be between these values: FATAL, ERROR, WARN, INFO or DEBUG",
                         fixed = TRUE)

  threshold <- "wrongThreshold"

  testthat::expect_error(options$configureLog(console = console,
                                              threshold = threshold,
                                              file = file),
                         "[BdparOptions][configureLog][FATAL] The 'threshold' parameter must be between these values: FATAL, ERROR, WARN, INFO or DEBUG",
                         fixed = TRUE)
})

testthat::test_that("configureLog error console argument",{

  options <- BdparOptions$new()

  console <- NULL
  threshold <- "INFO"
  file <- NULL

  testthat::expect_error(options$configureLog(console = console,
                                              threshold = threshold,
                                              file = file),
                         "[BdparOptions][configureLog][FATAL] Checking the type of the 'console' variable: NULL",
                         fixed = TRUE)
})

testthat::setup({
  bdpar.Options$configureLog()
})

testthat::test_that("disableLog works",{

  options <- BdparOptions$new()

  options$disableLog()

  testthat::expect_message(options$getLogConfiguration(),
                           "[BdparOptions][getLogConfiguration][INFO] Log configuration:
	- Threshold: INFO
	- Console log status: Disabled
	- File log status: Disabled",
                           fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$configureLog()
})

testthat::test_that("getLogConfiguration works",{

  options <- BdparOptions$new()

  testthat::expect_message(options$getLogConfiguration(),
                           "[BdparOptions][getLogConfiguration][INFO] Log configuration:
	- Threshold: INFO
	- Console log status: Actived
	- File log status: Disabled",
                           fixed = TRUE)

  console <- FALSE
  threshold <- "INFO"
  file <- NULL

  options$configureLog(console = console,
                       threshold = threshold,
                       file = file)

  testthat::expect_message(options$getLogConfiguration(),
                           "[BdparOptions][getLogConfiguration][INFO] Log configuration:
	- Threshold: INFO
	- Console log status: Disabled
	- File log status: Disabled",
                           fixed = TRUE)

  bdpar.Options$configureLog(console = console,
                             threshold = threshold,
                             file = file)

  testthat::expect_silent(bdpar.log(message = "empty", level = "INFO"))
})

testthat::teardown({
  bdpar.Options$configureLog()
})
