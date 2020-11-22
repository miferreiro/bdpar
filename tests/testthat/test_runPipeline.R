testthat::context("runPipeline")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("runPipeline path type error",{

  path <- NULL

  pipeline <- DefaultPipeline$new()

  extractorFactory <- ExtractorFactory$new()

  summary <- FALSE

  testthat::expect_error(runPipeline(path = path,
                                     pipeline = pipeline,
                                     extractors = extractorFactory,
                                     summary = summary),
                         "[runPipeline][FATAL] Checking the type of the 'path' variable: NULL",
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

testthat::test_that("runPipeline pipeline type error",{

  path <- file.path("testFiles",
                    "testPipelineExecute",
                    "tsms")

  pipeline <- NULL

  extractorFactory <- ExtractorFactory$new()

  summary <- FALSE

  testthat::expect_error(runPipeline(path = path,
                                     pipeline = pipeline,
                                     extractors = extractorFactory,
                                     summary = summary),
                         "[runPipeline][FATAL] Checking the type of the 'pipeline' variable: NULL",
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

testthat::test_that("runPipeline extractorFactory type error",{

  path <- file.path("testFiles",
                    "testRunPipeline",
                    "tsms")

  pipeline <- DefaultPipeline$new()

  extractorFactory <- NULL

  summary <- FALSE

  testthat::expect_error(runPipeline(path = path,
                                     pipeline = pipeline,
                                     extractors = extractorFactory,
                                     summary = summary),
                         "[runPipeline][FATAL] Checking the type of the 'extractors' variable: NULL",
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

testthat::test_that("runPipeline summary type error",{

  path <- file.path("testFiles",
                    "testRunPipeline",
                    "tsms")

  pipeline <- DefaultPipeline$new()

  extractorFactory <- ExtractorFactory$new()

  summary <- NULL

  testthat::expect_error(runPipeline(path = path,
                                     pipeline = pipeline,
                                     extractors = extractorFactory,
                                     summary = summary),
                         "[runPipeline][FATAL] Checking the type of the 'summary' variable: NULL",
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
})

testthat::test_that("runPipeline default flow of pipes with the examples files tsms",{
  testthat::skip_if_not_installed("cld2")
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rtweet")
  testthat::skip_if_not_installed("stringi")
  testthat::skip_if_not_installed("stringr")

  path <- file.path("testFiles",
                    "testRunPipeline",
                    "tsms")

  pipeline <- DefaultPipeline$new()

  extractorFactory <- ExtractorFactory$new()

  summary <- FALSE

  bdpar.Options$set("extractorEML.mpaPartSelected", "text/plain")
  bdpar.Options$set("resources.abbreviations.path", "")
  bdpar.Options$set("resources.contractions.path", "")
  bdpar.Options$set("resources.interjections.path", "")
  bdpar.Options$set("resources.slangs.path", "")
  bdpar.Options$set("resources.stopwords.path", "")
  bdpar.Options$set("teeCSVPipe.output.path", file.path("testFiles",
                                                        "testRunPipeline",
                                                        "output_tsms.csv"))
  bdpar.Options$set("cache", FALSE)

  output <- suppressWarnings(runPipeline(path = path,
                                         pipeline = pipeline,
                                         extractors = extractorFactory,
                                         summary = summary))
  file1 <- output[[1]]

  testthat::expect_equal(file1$getDate(),"")

  testthat::expect_equal(file1$getSource(),"Wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us")

  testthat::expect_equal(file1$getData(),"wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us")
  testthat::expect_equal(file1$getSpecificProperty("target"),"ham")
  testthat::expect_equal(file1$getSpecificProperty("extension"),"tsms")

  testthat::expect_equal(file1$getSpecificProperty("length_before_cleaning_text"),130)

  testthat::expect_equal(file1$getSpecificProperty("userName"),as.character(c()))
  testthat::expect_equal(file1$getSpecificProperty("hashtag"),as.character(c()))
  testthat::expect_equal(file1$getSpecificProperty("URLs"),as.character(c()))
  testthat::expect_equal(file1$getSpecificProperty("emoticon"),as.character(c()))
  testthat::expect_equal(file1$getSpecificProperty("Emojis"),as.character(c()))
  testthat::expect_equal(file1$getSpecificProperty("language"),"en")
  testthat::expect_equal(file1$getSpecificProperty("contractions"),list())
  testthat::expect_equal(file1$getSpecificProperty("abbreviation"),list())
  testthat::expect_equal(file1$getSpecificProperty("langpropname"),list())
  testthat::expect_equal(file1$getSpecificProperty("interjection"),list())
  testthat::expect_equal(file1$getSpecificProperty("stopWord"),list())
  testthat::expect_equal(file1$getSpecificProperty("length_after_cleaning_text"),130)
  testthat::expect_equal(file1$isInstanceValid(),TRUE)
  testthat::expect_equal(file1$getFlowPipes(),list("TargetAssigningPipe",
                                                   "StoreFileExtPipe",
                                                   "GuessDatePipe",
                                                   "File2Pipe",
                                                   "MeasureLengthPipe",
                                                   "FindUserNamePipe",
                                                   "FindHashtagPipe",
                                                   "FindUrlPipe",
                                                   "FindEmoticonPipe",
                                                   "FindEmojiPipe",
                                                   "GuessLanguagePipe",
                                                   "ContractionPipe",
                                                   "AbbreviationPipe",
                                                   "SlangPipe",
                                                   "ToLowerCasePipe",
                                                   "InterjectionPipe",
                                                   "StopWordPipe",
                                                   "MeasureLengthPipe",
                                                   "TeeCSVPipe"))

  testthat::expect_equal(file1$getBanPipes(),
                         c("FindUrlPipe","FindHashtagPipe","AbbreviationPipe"))

  file2 <- output[[2]]

  testthat::expect_equal(file2$getDate(),"")

  testthat::expect_equal(file2$getSource(),"Guess what! Somebody you know secretly fancies you! Wanna find out who it is? Give us a call on 09065394514 From Landline DATEBox1282EssexCM61XN 150p/min 18")

  testthat::expect_equal(file2$getData(),"guess what! somebody you know secretly fancies you! wanna find out who it is? give us a call on 09065394514 from landline datebox1282essexcm61xn 150p/min 18")
  testthat::expect_equal(file2$getSpecificProperty("target"),"spam")
  testthat::expect_equal(file2$getSpecificProperty("extension"),"tsms")

  testthat::expect_equal(file2$getSpecificProperty("length_before_cleaning_text"),156)

  testthat::expect_equal(file2$getSpecificProperty("userName"),as.character(c()))
  testthat::expect_equal(file2$getSpecificProperty("hashtag"),as.character(c()))
  testthat::expect_equal(file2$getSpecificProperty("URLs"),as.character(c()))
  testthat::expect_equal(file2$getSpecificProperty("emoticon"),as.character(c()))
  testthat::expect_equal(file2$getSpecificProperty("Emojis"),as.character(c()))
  testthat::expect_equal(file2$getSpecificProperty("language"),"en")
  testthat::expect_equal(file2$getSpecificProperty("contractions"),list())
  testthat::expect_equal(file2$getSpecificProperty("abbreviation"),list())
  testthat::expect_equal(file2$getSpecificProperty("langpropname"),list())
  testthat::expect_equal(file2$getSpecificProperty("interjection"),list())
  testthat::expect_equal(file2$getSpecificProperty("stopWord"),list())
  testthat::expect_equal(file2$getSpecificProperty("length_after_cleaning_text"),156)
  testthat::expect_equal(file2$isInstanceValid(),TRUE)
  testthat::expect_equal(file2$getFlowPipes(),list("TargetAssigningPipe",
                                                   "StoreFileExtPipe",
                                                   "GuessDatePipe",
                                                   "File2Pipe",
                                                   "MeasureLengthPipe",
                                                   "FindUserNamePipe",
                                                   "FindHashtagPipe",
                                                   "FindUrlPipe",
                                                   "FindEmoticonPipe",
                                                   "FindEmojiPipe",
                                                   "GuessLanguagePipe",
                                                   "ContractionPipe",
                                                   "AbbreviationPipe",
                                                   "SlangPipe",
                                                   "ToLowerCasePipe",
                                                   "InterjectionPipe",
                                                   "StopWordPipe",
                                                   "MeasureLengthPipe",
                                                   "TeeCSVPipe"))
  testthat::expect_equal(file2$getBanPipes(),
                         c("FindUrlPipe","FindHashtagPipe","AbbreviationPipe"))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()

  if (file.exists(file.path("testFiles",
                            "testRunPipeline",
                            "output_tsms.csv"))) {
    file.remove(file.path("testFiles",
                          "testRunPipeline",
                          "output_tsms.csv"))
  }
})
}

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("runPipeline DEBUG mode works",{

  path <- file.path("testFiles",
                    "testRunPipeline",
                    "tsms",
                    "_ham_",
                    "30.tsms")

  pipeline <- DefaultPipeline$new()

  extractorFactory <- ExtractorFactory$new()

  summary <- FALSE

  pipeline <- DynamicPipeline$new()
  pipeline$add(list(TargetAssigningPipe$new()), pos = NULL)

  bdpar.Options$configureLog(console = TRUE, threshold = "DEBUG")

  testthat::expect_message(runPipeline(path = path,
                                       pipeline = pipeline,
                                       extractors = extractorFactory,
                                       summary = summary),
                           '[-\\[\\]:0-9 ]+\\[DEBUG\\] Instance_ID:1 \\(Last pipe: TargetAssigningPipe\\)\n\tPath: testFiles\\/testRunPipeline\\/tsms\\/_ham_\\/30\\.tsms\n\tDate: \n\tIsValid: TRUE\n\tSource: ""\n\tData: ""\n\tFlowPipes: TargetAssigningPipe\n\tBanPipes: \n\tProperties: \n\t\t- target: ham\n',
                           perl = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
