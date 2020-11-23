testthat::context("DynamicPipeline")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize",{
  testthat::expect_silent(DynamicPipeline$new())

  testthat::expect_silent(DynamicPipeline$new(list(TargetAssigningPipe$new())))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize error",{
  testthat::expect_error(DynamicPipeline$new(1),
                         "[DynamicPipeline][initialize][FATAL] Checking the type of the 'pipeline' variable: numeric",
                         fixed = TRUE)

  testthat::expect_error(DynamicPipeline$new(list(1)),
                         "[DynamicPipeline][initialize][FATAL] Define pipes are not correct. Must be inherit from 'GenericPipe' class. Aborting...",
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

testthat::test_that("add",{

  pipeline <- DynamicPipeline$new()
  testthat::expect_length(pipeline$get(), 0)
  pipeline$add(pipe = TargetAssigningPipe$new(), pos = NULL)
  testthat::expect_length(pipeline$get(), 1)

  pipeline <- DynamicPipeline$new()
  testthat::expect_warning(pipeline$add(pipe = File2Pipe$new(), pos = 2),
                           "[DynamicPipeline][add][WARN] Pipeline empty, adding in the first position",
                           fixed = TRUE)

  pipeline <- DynamicPipeline$new()
  testthat::expect_length(pipeline$get(), 0)
  pipeline$add(pipe = list(TargetAssigningPipe$new(), StoreFileExtPipe$new()), pos = NULL)
  testthat::expect_length(pipeline$get(), 2)

  testthat::expect_warning(pipeline$add(pipe = File2Pipe$new(), pos = 3),
                           "[DynamicPipeline][add][WARN] The position exceeds the length of the pipeline, adding at the end of it",
                           fixed = TRUE)
  testthat::expect_length(pipeline$get(), 3)
  pipeline$add(pipe = GuessDatePipe$new(), pos = 3)
  testthat::expect_length(pipeline$get(), 4)

  testthat::expect_error(pipeline$add(pipe = GuessDatePipe$new(), pos = -2),
                         "[DynamicPipeline][add][FATAL] It can only be added between positions '0' and '4'",
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

testthat::test_that("add error",{

  pipeline <- DynamicPipeline$new()
  testthat::expect_error(pipeline$add(pipe = 1, pos = NULL),
                         "[DynamicPipeline][add][FATAL] Checking the type of the 'pipe' variable: list",
                         fixed = TRUE)

  testthat::expect_error(pipeline$add(pipe = GuessDatePipe$new(), pos = "wrong"),
                         "[DynamicPipeline][add][FATAL] Checking the type of the 'pos' variable: character",
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

testthat::test_that("removeByPos",{

  pipeline <- DynamicPipeline$new()
  pipeline$add(pipe = TargetAssigningPipe$new(), pos = NULL)
  testthat::expect_length(pipeline$get(), 1)
  pipeline$removeByPos(pos = 1)
  testthat::expect_length(pipeline$get(), 0)

})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("removeByPos error",{

  pipeline <- DynamicPipeline$new()
  testthat::expect_warning(pipeline$removeByPos(pos = 1),
                           "[DynamicPipeline][removeByPos][WARN] Pipeline empty. Imposible remove",
                           fixed = TRUE)

  testthat::expect_error(pipeline$removeByPos(pos = "A"),
                         "[DynamicPipeline][removeByPos][FATAL] Checking the type of the 'pos' variable: character",
                         fixed = TRUE)

  pipeline$add(pipe = GuessDatePipe$new())
  testthat::expect_error(pipeline$removeByPos(pos = 3),
                         "[DynamicPipeline][removeByPos][FATAL] It can only be deleted between positions '0' and '1'",
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

testthat::test_that("removeByPipe",{

  pipeline <- DynamicPipeline$new()
  pipeline$add(pipe = TargetAssigningPipe$new(), pos = NULL)
  testthat::expect_length(pipeline$get(), 1)
  pipeline$removeByPipe(pipe = "TargetAssigningPipe")
  testthat::expect_length(pipeline$get(), 0)

})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("removeByPipe error",{

  pipeline <- DynamicPipeline$new()
  testthat::expect_warning(pipeline$removeByPipe(pipe.name = "TargetAssigningPipe"),
                           "[DynamicPipeline][removeByPipe][WARN] Pipeline empty. Imposible remove",
                           fixed = TRUE)

  testthat::expect_error(pipeline$removeByPipe(pipe = 1),
                         "[DynamicPipeline][removeByPipe][FATAL] Checking the type of the 'pipe.name' variable (must be a character list)",
                         fixed = TRUE)

  pipeline$add(pipe = GuessDatePipe$new())
  testthat::expect_warning(pipeline$removeByPipe(pipe.name = "a"),
                           "[DynamicPipeline][removeByPipe][WARN] Not found elements to remove",
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

testthat::test_that("execute instance type error",{

  instance <- NULL
  testthat::expect_error(DynamicPipeline$new()$execute(instance),
                         "[DynamicPipeline][execute][FATAL] Checking the type of the 'instance' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

if (Sys.info()[['sysname']] %in% "Windows") {

  testthat::setup({
    bdpar.Options$reset()
    bdpar.Options$configureLog()
    bdpar.Options$set("cache", FALSE)
  })

  testthat::test_that("execute",{
    testthat::skip_if_not_installed("cld2")
    testthat::skip_if_not_installed("rex")
    testthat::skip_if_not_installed("rjson")
    testthat::skip_if_not_installed("rtweet")
    testthat::skip_if_not_installed("stringi")
    testthat::skip_if_not_installed("stringr")
    path <- file.path("testFiles",
                      "testDynamicPipeline",
                      "files",
                      "_ham_",
                      "testFile.tsms")

    bdpar.Options$set("extractorEML.mpaPartSelected", "text/plain")
    bdpar.Options$set("resources.abbreviations.path", "")
    bdpar.Options$set("resources.contractions.path", "")
    bdpar.Options$set("resources.interjections.path", "")
    bdpar.Options$set("resources.slangs.path", "")
    bdpar.Options$set("resources.stopwords.path", "")
    bdpar.Options$set("teeCSVPipe.output.path", "output_tsms.csv")

    instance <- ExtractorSms$new(path)
    instanceInitial <- ExtractorSms$new(path)

    instance$setDate("")

    instance$setSource("Wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us")

    instance$setData("Wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us")
    instance$setSpecificProperty("target", "ham")
    instance$setSpecificProperty("extension", "tsms")

    instance$addFlowPipes("TargetAssigningPipe")
    instance$addFlowPipes("StoreFileExtPipe")
    instance$addFlowPipes("GuessDatePipe")
    instance$addFlowPipes("File2Pipe")

    pipeline <- DynamicPipeline$new()
    pipeline$add(list(TargetAssigningPipe$new(), StoreFileExtPipe$new(), GuessDatePipe$new(), File2Pipe$new()))
    suppressWarnings(pipeline$execute(instanceInitial))
    testthat::expect_equal(instanceInitial,
                           instance)
  })

  testthat::teardown({
    bdpar.Options$reset()
    bdpar.Options$configureLog()
  })
}

if (Sys.info()[['sysname']] %in% "Windows") {

  testthat::setup({
    bdpar.Options$reset()
    bdpar.Options$configureLog()
    bdpar.Options$set("cache", FALSE)
    bdpar.Options$set("verbose", TRUE)
  })

  testthat::test_that("execute error",{

    path <- file.path("testFiles",
                      "testDynamicPipeline",
                      "files",
                      "_ham_",
                      "testFileEmpty.tsms")

    instanceInitial <- ExtractorSms$new(path)
    instance <- ExtractorSms$new(path)

    instance$setDate("")

    instance$setSource("")

    instance$setData("")
    instance$setSpecificProperty("target", "ham")
    instance$setSpecificProperty("extension", "tsms")
    instance$addFlowPipes("TargetAssigningPipe")
    instance$addFlowPipes("StoreFileExtPipe")
    instance$addFlowPipes("GuessDatePipe")
    instance$addFlowPipes("File2Pipe")

    instance$addBanPipes(c("FindUrlPipe", "FindHashtagPipe", "AbbreviationPipe"))

    pipeline <- DynamicPipeline$new()
    pipeline$add(list(TargetAssigningPipe$new(), StoreFileExtPipe$new(), GuessDatePipe$new(), File2Pipe$new()))
    testthat::expect_message(suppressWarnings(pipeline$execute(instanceInitial)),
                             "[pipeOperator][INFO] The instance testFiles/testDynamicPipeline/files/_ham_/testFileEmpty.tsms is invalid and will not continue through the flow of pipes",
                             fixed = TRUE)

    pipeline <- DynamicPipeline$new()
    testthat::expect_error(suppressWarnings(pipeline$execute(instanceInitial)),
                           "[DynamicPipeline][execute][FATAL] Pipeline is empty",
                           fixed = TRUE)

    pipeline <- DynamicPipeline$new()
    bdpar.Options$remove("cache")
    pipeline$add(list(TargetAssigningPipe$new(), StoreFileExtPipe$new(), GuessDatePipe$new(), File2Pipe$new()))
    testthat::expect_message(suppressWarnings(pipeline$execute(instanceInitial)),
                             "[-\\[\\]:0-9 ]+\\[DynamicPipeline\\]\\[execute\\]\\[ERROR\\] testFiles\\/testDynamicPipeline\\/files\\/_ham_\\/testFileEmpty\\.tsms :Error: [-\\[\\]:0-9 ]+\\[FATAL\\] Cache status is not defined in bdpar.Options",
                             perl = TRUE)

  })

  testthat::teardown({
    bdpar.Options$reset()
    bdpar.Options$configureLog()
  })
}

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("removeAll",{

  pipeline <- DynamicPipeline$new(list(TargetAssigningPipe$new()))

  testthat::expect_length(pipeline$get(), 1)

  pipeline$removeAll()

  testthat::expect_length(pipeline$get(), 0)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("get",{
  testthat::expect_equal(DynamicPipeline$new()$get(), list())

  testthat::expect_equal(DynamicPipeline$new(list(TargetAssigningPipe$new()))$get(),
                         list(TargetAssigningPipe$new()))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("print",{
  pipeline <- DynamicPipeline$new(list(TargetAssigningPipe$new()))
  testthat::expect_output(print(pipeline), "[A-Za-z0-9$/.\\(\\)%>]+")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("print",{
  pipeline <- DynamicPipeline$new(list(TargetAssigningPipe$new()))
  testthat::expect_equal(pipeline$toString(),
                         "instance %>|%\n\t TargetAssigningPipe")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
