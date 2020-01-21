context("TypePipe")

test_that("initialize",{

  expect_silent(TypePipe$new())
})

test_that("pipeAll instance type error",{

  instance <- NULL
  expect_error(TypePipe$new()$pipeAll(instance),"\\[TypePipe\\]\\[pipeAll\\]\\[Error\\]
                Checking the type of the variable: instance NULL")
})

if (Sys.info()[['sysname']] %in% "Windows") {
test_that("pipeAll",{
  skip_if_not_installed("cld2")
  skip_if_not_installed("readr")
  skip_if_not_installed("rex")
  skip_if_not_installed("rjson")
  skip_if_not_installed("rtweet")
  skip_if_not_installed("stringi")
  skip_if_not_installed("stringr")
  skip_if_not_installed("textutils")
  path <- file.path("testFiles",
                    "testTypePipe",
                    "files",
                    "_ham_",
                    "testFile.tsms")

  Bdpar$new(configurationFilePath = file.path("testFiles",
                                              "testTypePipe",
                                              "configurations.ini"))
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
  instanceInitial <- suppressWarnings(TypePipe$new()$pipeAll(instanceInitial))
  expect_equal(instanceInitial, instance)

  file.remove("output_tsms.csv")
})
}
