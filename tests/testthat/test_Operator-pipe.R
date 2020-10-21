testthat::context("Operator-pipe")

testthat::setup(bdpar.Options$reset())

testthat::test_that("'cache.folder' field error",{

  testthat::skip_if_not_installed("stringi")

  path <- file.path("testFiles",
                    "testOperator-pipe",
                    "files",
                    "_ham_",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  bdpar.Options$set("cache", FALSE)
  pipeline <- DynamicPipeline$new()
  pipeline$add(list(File2Pipe$new()))
  testthat::expect_message(pipeline$execute(instance),
                           "[pipeOperator][freduce][Error] Bad compatibility between Pipes on File2Pipe",
                           fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("'cache' field error",{

  testthat::skip_if_not_installed("stringi")

  path <- file.path("testFiles",
                    "testOperator-pipe",
                    "files",
                    "_ham_",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)

  bdpar.Options$set("cache", NULL)
  pipeline <- DynamicPipeline$new()
  pipeline$add(list(TargetAssigningPipe$new(), StoreFileExtPipe$new()))
  testthat::expect_message(pipeline$execute(instance),
                         "[DynamicPipeline][execute][Error] testFiles/testOperator-pipe/files/_ham_/testFile.tsms :Error in freduce(value, `_function_list`): [pipeOperator][freduce][Error] Cache status is not defined in bdpar.Options",
                         fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup(bdpar.Options$reset())

testthat::test_that("'cache.folder' field error",{

  testthat::skip_if_not_installed("stringi")

  path <- file.path("testFiles",
                    "testOperator-pipe",
                    "files",
                    "_ham_",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  bdpar.Options$set("cache", TRUE)
  bdpar.Options$set("cache.folder", NULL)
  pipeline <- DynamicPipeline$new()
  pipeline$add(list(TargetAssigningPipe$new(), StoreFileExtPipe$new()))
  testthat::expect_message(pipeline$execute(instance),
                           "[DynamicPipeline][execute][Error] testFiles/testOperator-pipe/files/_ham_/testFile.tsms :Error in freduce(value, `_function_list`): [pipeOperator][freduce][Error] Cache folder is not defined in bdpar.Options",
                           fixed = TRUE)
})

testthat::teardown(bdpar.Options$reset())
testthat::setup({
  bdpar.Options$reset()
  if (dir.exists(file.path("testFiles",
                           "testOperator-pipe",
                           ".cache"))) {
    unlink(file.path("testFiles",
                     "testOperator-pipe",
                     ".cache"),
           recursive = T)
  }
})

testthat::test_that("'cache' field error",{

  testthat::skip_if_not_installed("stringi")

  path <- file.path("testFiles",
                    "testOperator-pipe",
                    "files",
                    "_ham_",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  bdpar.Options$set("cache",
                    TRUE)
  bdpar.Options$set("cache.folder",
                    file.path("testFiles",
                              "testOperator-pipe",
                              ".cache"))
  pipeline <- DynamicPipeline$new()
  pipeline$add(list(TargetAssigningPipe$new(), StoreFileExtPipe$new()))
  pipeline$execute(instance)
  testthat::expect_true(dir.exists(file.path("testFiles",
                                             "testOperator-pipe",
                                             ".cache",
                                             "7ca200ced299")))

  testthat::expect_true(file.exists(file.path("testFiles",
                                              "testOperator-pipe",
                                              ".cache",
                                              "7ca200ced299",
                                              "1-e0cd002c1193-101e954308b7.z")))

  testthat::expect_true(file.exists(file.path("testFiles",
                                              "testOperator-pipe",
                                              ".cache",
                                              "7ca200ced299",
                                              "2-70a218fe10a5-039d3e4f119b.z")))

})

testthat::teardown({
  bdpar.Options$reset()
  if (dir.exists(file.path("testFiles",
                           "testOperator-pipe",
                           ".cache"))) {
    unlink(file.path("testFiles",
                     "testOperator-pipe",
                     ".cache"),
           recursive = T)
  }
})
