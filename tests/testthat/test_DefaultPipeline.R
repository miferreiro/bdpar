testthat::context("DefaultPipeline")

testthat::test_that("initialize",{

  testthat::expect_silent(DefaultPipeline$new())
})

testthat::test_that("execute instance type error",{

  instance <- NULL
  testthat::expect_error(DefaultPipeline$new()$execute(instance),
                         "[DefaultPipeline][execute][Error] Checking the type of the 'instance' variable: NULL",
                         fixed = TRUE)
})

if (Sys.info()[['sysname']] %in% "Windows") {

testthat::setup(bdpar.Options$reset())

testthat::test_that("execute",{
  testthat::skip_if_not_installed("cld2")
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rtweet")
  testthat::skip_if_not_installed("stringi")
  testthat::skip_if_not_installed("stringr")
  testthat::skip_if_not_installed("textutils")
  path <- file.path("testFiles",
                    "testDefaultPipeline",
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

  Bdpar$new()
  instance <- ExtractorSms$new(path)
  instanceInitial <- ExtractorSms$new(path)

  instance$setDate("")

  instance$setSource("Wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us\r\n")

  instance$setData("wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us")
  instance$setSpecificProperty("target", "ham")
  instance$setSpecificProperty("extension", "tsms")

  instance$setSpecificProperty("length_before_cleaning_text", 132)

  instance$setSpecificProperty("userName", as.character(c()))
  instance$setSpecificProperty("hashtag", as.character(c()))
  instance$setSpecificProperty("URLs", as.character(c()))
  instance$setSpecificProperty("emoticon", as.character(c()))
  instance$setSpecificProperty("Emojis", as.character(c()))
  instance$setSpecificProperty("language", "en")
  instance$setSpecificProperty("contractions", list())
  instance$setSpecificProperty("abbreviation", list())
  instance$setSpecificProperty("langpropname", list())
  instance$setSpecificProperty("interjection", list())
  instance$setSpecificProperty("stopWord", list())
  instance$setSpecificProperty("length_after_cleaning_text", 130)
  instance$addFlowPipes("TargetAssigningPipe")
  instance$addFlowPipes("StoreFileExtPipe")
  instance$addFlowPipes("GuessDatePipe")
  instance$addFlowPipes("File2Pipe")
  instance$addFlowPipes("MeasureLengthPipe")
  instance$addFlowPipes("FindUserNamePipe")
  instance$addFlowPipes("FindHashtagPipe")
  instance$addFlowPipes("FindUrlPipe")
  instance$addFlowPipes("FindEmoticonPipe")
  instance$addFlowPipes("FindEmojiPipe")
  instance$addFlowPipes("GuessLanguagePipe")
  instance$addFlowPipes("ContractionPipe")
  instance$addFlowPipes("AbbreviationPipe")
  instance$addFlowPipes("SlangPipe")
  instance$addFlowPipes("ToLowerCasePipe")
  instance$addFlowPipes("InterjectionPipe")
  instance$addFlowPipes("StopWordPipe")
  instance$addFlowPipes("MeasureLengthPipe")
  instance$addFlowPipes("TeeCSVPipe")

  instance$addBanPipes(c("FindUrlPipe", "FindHashtagPipe", "AbbreviationPipe"))
  suppressWarnings(DefaultPipeline$new()$execute(instanceInitial))
  testthat::expect_equal(instanceInitial,
                         instance)

  file.remove("output_tsms.csv")
})

testthat::teardown(bdpar.Options$reset())

}

if (Sys.info()[['sysname']] %in% "Windows") {

  testthat::setup(bdpar.Options$reset())

  testthat::test_that("execute error",{
    testthat::skip_if_not_installed("readr")
    path <- file.path("testFiles",
                      "testDefaultPipeline",
                      "files",
                      "_ham_",
                      "testFileEmpty.tsms")

    Bdpar$new()

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

    testthat::expect_message(suppressWarnings(DefaultPipeline$new()$execute(instanceInitial)),
                             "[DefaultPipeline][execute][Error]",
                             fixed = TRUE)
  })

  testthat::teardown(bdpar.Options$reset())

}

testthat::setup(bdpar.Options$reset())

testthat::test_that("get",{

  bdpar.Options$set("teeCSVPipe.output.path", "output_tsms.csv")

  testthat::expect_equal(DefaultPipeline$new()$get(),
                         list(TargetAssigningPipe$new(),
                              StoreFileExtPipe$new(),
                              GuessDatePipe$new(),
                              File2Pipe$new(),
                              MeasureLengthPipe$new(propertyName = "length_before_cleaning_text"),
                              FindUserNamePipe$new(),
                              FindHashtagPipe$new(),
                              FindUrlPipe$new(),
                              FindEmoticonPipe$new(),
                              FindEmojiPipe$new(),
                              GuessLanguagePipe$new(),
                              ContractionPipe$new(),
                              AbbreviationPipe$new(),
                              SlangPipe$new(),
                              ToLowerCasePipe$new(),
                              InterjectionPipe$new(),
                              StopWordPipe$new(),
                              MeasureLengthPipe$new(propertyName = "length_after_cleaning_text"),
                              TeeCSVPipe$new()))
})

testthat::teardown(bdpar.Options$reset())

testthat::test_that("print",{
  pipeline <- DefaultPipeline$new()
  testthat::expect_output(print(pipeline), "[A-Za-z0-9$/.\\(\\)%>]+")
})
