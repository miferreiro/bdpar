context("Bdpar")

test_that("Bdpar editConfigurationFile <- FALSE",{

  configurationFilePath <-  system.file("configurations",
                                        "test_pipeline_execute_tsms_configurations.ini",
                                        package = "bdpar")

  editConfigurationFile <- FALSE

  expect_silent(Bdpar$new(configurationFilePath, editConfigurationFile))
})

test_that("Bdpar configurationFilePath type error",{

  configurationFilePath <-  1

  editConfigurationFile <- FALSE

  expect_error(Bdpar$new(configurationFilePath, editConfigurationFile),"\\[Bdpar\\]\\[initialize\\]\\[Error\\]
                  Checking the type of the variable: configurationFilePath numeric")
})

test_that("Bdpar configurationFilePath extension error",{

  configurationFilePath <-  "example.ext"

  editConfigurationFile <- FALSE


  expect_error(Bdpar$new(configurationFilePath, editConfigurationFile),"\\[Bdpar\\]\\[initialize\\]\\[Error\\]
                  Checking the extension of the file: configurationFilePath ext")

})

test_that("Bdpar editConfigurationFile type error",{

  configurationFilePath <-  system.file("configurations",
                                        "test_pipeline_execute_tsms_configurations.ini",
                                        package = "bdpar")

  editConfigurationFile <- NULL

  expect_error(Bdpar$new(configurationFilePath, editConfigurationFile),"\\[Bdpar\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: editConfigurationFile NULL")

})

test_that("Bdpar filesPath type error",{

  #Path where the configuration file are located
  configurationFilePath <-  system.file("configurations",
                                        "test_pipeline_execute_tsms_configurations.ini",
                                        package = "bdpar")

  editConfigurationFile <- FALSE

  object <- Bdpar$new(configurationFilePath, editConfigurationFile)

  filesPath <- NULL

  pipe <- SerialPipe$new()

  instanceFactory <- InstanceFactory$new()

  expect_error(object$proccess_files(filesPath = filesPath,
                                     pipe = pipe,
                                     instanceFactory = instanceFactory),"\\[Bdpar\\]\\[proccess_files\\]\\[Error\\]
                Checking the type of the variable: filesPath NULL")
})

test_that("Bdpar pipe type error",{

  #Path where the configuration file are located
  configurationFilePath <-  system.file("configurations",
                                        "test_pipeline_execute_tsms_configurations.ini",
                                        package = "bdpar")

  editConfigurationFile <- FALSE

  object <- Bdpar$new(configurationFilePath, editConfigurationFile)

  filesPath <- system.file("testFiles_pipeline_execute_tsms",
                           package = "bdpar")

  pipe <- NULL

  instanceFactory <- InstanceFactory$new()

  expect_error(object$proccess_files(filesPath = filesPath,
                                     pipe = pipe,
                                     instanceFactory = instanceFactory),"\\[Bdpar\\]\\[proccess_files\\]\\[Error\\]
                Checking the type of the variable: pipe NULL")
})

test_that("Bdpar instanceFactory type error",{

  #Path where the configuration file are located
  configurationFilePath <-  system.file("configurations",
                                        "test_pipeline_execute_tsms_configurations.ini",
                                        package = "bdpar")

  editConfigurationFile <- FALSE

  object <- Bdpar$new(configurationFilePath, editConfigurationFile)

  filesPath <- system.file("testFiles_pipeline_execute_tsms",
                           package = "bdpar")

  pipe <- SerialPipe$new()

  instanceFactory <- NULL

  expect_error(object$proccess_files(filesPath = filesPath,
                                     pipe = pipe,
                                     instanceFactory = instanceFactory),"\\[Bdpar\\]\\[proccess_files\\]\\[Error\\]
                Checking the type of the variable: instanceFactory NULL")
})

test_that("Bdpar default flow of pipes with the examples files tsms",{


  #Path where the configuration file are located
  configurationFilePath <-  system.file("configurations",
                                        "test_pipeline_execute_tsms_configurations.ini",
                                        package = "bdpar")



  object <- Bdpar$new(configurationFilePath)

  #Folder with the files to preprocess
  filesPath <- system.file("testFiles_pipeline_execute_tsms",
                           package = "bdpar")

  #Object which indicates the pipes' flow
  pipe <- SerialPipe$new()

  #Object which decides how creates the instances
  instanceFactory <- InstanceFactory$new()

  #Starting file preprocessing...
  output <- object$proccess_files(filesPath,
                                  pipe = pipe,
                                  instanceFactory = instanceFactory)
  file1 <- output[[1]]

  expect_equal(file1$getDate(),"")
  expect_equal(file1$getSource(),"Wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us\r\n")
  expect_equal(file1$getData(),"wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us")
  expect_equal(file1$getSpecificProperty("target"),"ham")
  expect_equal(file1$getSpecificProperty("extension"),"tsms")
  expect_equal(file1$getSpecificProperty("length_before_cleaning_text"),132)
  expect_equal(file1$getSpecificProperty("userName"),as.character(c()))
  expect_equal(file1$getSpecificProperty("hashtag"),as.character(c()))
  expect_equal(file1$getSpecificProperty("URLs"),as.character(c()))
  expect_equal(file1$getSpecificProperty("emoticon"),as.character(c()))
  expect_equal(file1$getSpecificProperty("Emojis"),as.character(c()))
  expect_equal(file1$getSpecificProperty("language"),"en")
  expect_equal(file1$getSpecificProperty("contractions"),list())
  expect_equal(file1$getSpecificProperty("abbreviation"),list())
  expect_equal(file1$getSpecificProperty("langpropname"),list())
  expect_equal(file1$getSpecificProperty("interjection"),list())
  expect_equal(file1$getSpecificProperty("stopWord"),list())
  expect_equal(file1$getSpecificProperty("length_after_cleaning_text"),130)
  expect_equal(file1$isInstanceValid(),TRUE)
  expect_equal(file1$getFlowPipes(),list("TargetAssigningPipe",
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

  expect_equal(file1$getBanPipes(),c("FindUrlPipe","FindHashtagPipe","AbbreviationPipe"))


  file2 <- output[[2]]

  expect_equal(file2$getDate(),"")
  expect_equal(file2$getSource(),"Guess what! Somebody you know secretly fancies you! Wanna find out who it is? Give us a call on 09065394514 From Landline DATEBox1282EssexCM61XN 150p/min 18\r\n")
  expect_equal(file2$getData(),"guess what! somebody you know secretly fancies you! wanna find out who it is? give us a call on 09065394514 from landline datebox1282essexcm61xn 150p/min 18")
  expect_equal(file2$getSpecificProperty("target"),"spam")
  expect_equal(file2$getSpecificProperty("extension"),"tsms")
  expect_equal(file2$getSpecificProperty("length_before_cleaning_text"),158)
  expect_equal(file2$getSpecificProperty("userName"),as.character(c()))
  expect_equal(file2$getSpecificProperty("hashtag"),as.character(c()))
  expect_equal(file2$getSpecificProperty("URLs"),as.character(c()))
  expect_equal(file2$getSpecificProperty("emoticon"),as.character(c()))
  expect_equal(file2$getSpecificProperty("Emojis"),as.character(c()))
  expect_equal(file2$getSpecificProperty("language"),"en")
  expect_equal(file2$getSpecificProperty("contractions"),list())
  expect_equal(file2$getSpecificProperty("abbreviation"),list())
  expect_equal(file2$getSpecificProperty("langpropname"),list())
  expect_equal(file2$getSpecificProperty("interjection"),list())
  expect_equal(file2$getSpecificProperty("stopWord"),list())
  expect_equal(file2$getSpecificProperty("length_after_cleaning_text"),156)
  expect_equal(file2$isInstanceValid(),TRUE)
  expect_equal(file2$getFlowPipes(),list("TargetAssigningPipe",
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
  expect_equal(file2$getBanPipes(),c("FindUrlPipe","FindHashtagPipe","AbbreviationPipe"))

  file.remove("output_tsms.csv")

})
