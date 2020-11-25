testthat::context("Bdpar")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("Bdpar initialize static variables",{

  testthat::expect_equal(class(Bdpar[["private_methods"]][["connections"]]()),
                         c("Connections", "R6"))
  testthat::expect_equal(class(Bdpar[["private_methods"]][["resourceHandler"]]()),
                         c("ResourceHandler", "R6"))

  object <- Bdpar$new()

  testthat::expect_equal(class(object$.__enclos_env__$private$connections()),
                         c("Connections", "R6"))
  testthat::expect_equal(class(object$.__enclos_env__$private$resourceHandler()),
                         c("ResourceHandler", "R6"))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("Bdpar path type error",{

  path <- NULL

  pipeline <- DefaultPipeline$new()

  extractorFactory <- ExtractorFactory$new()

  cache <- FALSE

  verbose <- TRUE

  summary <- FALSE

  object <- Bdpar$new()

  testthat::expect_error(object$execute(path = path,
                                        pipeline = pipeline,
                                        extractors = extractorFactory,
                                        cache = cache,
                                        verbose = verbose,
                                        summary = summary),
                         "[Bdpar][execute][FATAL] Checking the type of the 'path' variable: NULL",
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

testthat::test_that("Bdpar path does not exists",{

  path <- "wrong.tsms"

  pipeline <- DefaultPipeline$new()

  extractorFactory <- ExtractorFactory$new()

  cache <- FALSE

  verbose <- TRUE

  summary <- FALSE

  object <- Bdpar$new()

  testthat::expect_error(object$execute(path = path,
                                        pipeline = pipeline,
                                        extractors = extractorFactory,
                                        cache = cache,
                                        verbose = verbose,
                                        summary = summary),
                         "[Bdpar][execute][FATAL] Path parameter must be an existing file or directory",
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

testthat::test_that("Bdpar pipeline type error",{

  path <- file.path("testFiles",
                    "testBdpar",
                    "tsms")

  pipeline <- NULL

  extractorFactory <- ExtractorFactory$new()

  cache <- FALSE

  verbose <- TRUE

  summary <- FALSE

  object <- Bdpar$new()

  testthat::expect_error(object$execute(path = path,
                                        pipeline = pipeline,
                                        extractors = extractorFactory,
                                        cache = cache,
                                        verbose = verbose,
                                        summary = summary),
                         "[Bdpar][execute][FATAL] Checking the type of the 'pipeline' variable: NULL",
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

testthat::test_that("Bdpar extractorFactory type error",{

  path <- file.path("testFiles",
                    "testBdpar",
                    "tsms")

  pipeline <- DefaultPipeline$new()

  extractorFactory <- NULL

  cache <- FALSE

  verbose <- TRUE

  summary <- FALSE

  object <- Bdpar$new()

  testthat::expect_error(object$execute(path = path,
                                        pipeline = pipeline,
                                        extractors = extractorFactory,
                                        cache = cache,
                                        verbose = verbose,
                                        summary = summary),
                         "[Bdpar][execute][FATAL] Checking the type of the 'extractors' variable: NULL",
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

testthat::test_that("Bdpar summary type error",{

  path <- file.path("testFiles",
                    "testBdpar",
                    "tsms")

  pipeline <- DefaultPipeline$new()

  extractorFactory <- ExtractorFactory$new()

  cache <- FALSE

  verbose <- TRUE

  summary <- NULL

  object <- Bdpar$new()

  testthat::expect_error(object$execute(path = path,
                                        pipeline = pipeline,
                                        extractors = extractorFactory,
                                        cache = cache,
                                        verbose = verbose,
                                        summary = summary),
                         "[Bdpar][execute][FATAL] Checking the type of the 'summary' variable: NULL",
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

testthat::test_that("Bdpar cache type error",{

  path <- file.path("testFiles",
                    "testBdpar",
                    "tsms")

  pipeline <- DefaultPipeline$new()

  extractorFactory <- ExtractorFactory$new()

  cache <- NULL

  verbose <- FALSE

  summary <- FALSE

  object <- Bdpar$new()

  testthat::expect_error(object$execute(path = path,
                                        pipeline = pipeline,
                                        extractors = extractorFactory,
                                        cache = cache,
                                        verbose = verbose,
                                        summary = summary),
                         "[Bdpar][execute][FATAL] Checking the type of the 'cache' variable: NULL",
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

testthat::test_that("Bdpar verbose type error",{

  path <- file.path("testFiles",
                    "testBdpar",
                    "tsms")

  pipeline <- DefaultPipeline$new()

  extractorFactory <- ExtractorFactory$new()

  cache <- FALSE

  verbose <- NULL

  summary <- FALSE

  object <- Bdpar$new()

  testthat::expect_error(object$execute(path = path,
                                        pipeline = pipeline,
                                        extractors = extractorFactory,
                                        cache = cache,
                                        verbose = verbose,
                                        summary = summary),
                         "[Bdpar][execute][FATAL] Checking the type of the 'verbose' variable: NULL",
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

testthat::test_that("Bdpar cache option not set",{

  path <- file.path("testFiles",
                    "testBdpar",
                    "tsms")

  pipeline <- DefaultPipeline$new()

  extractorFactory <- ExtractorFactory$new()

  cache <- FALSE

  verbose <- FALSE

  summary <- FALSE

  object <- Bdpar$new()

  bdpar.Options$remove("cache")

  testthat::expect_error(object$execute(path = path,
                                        pipeline = pipeline,
                                        extractors = extractorFactory,
                                        cache = cache,
                                        verbose = verbose,
                                        summary = summary),
                         "[Bdpar][execute][FATAL] Cache status is not defined in bdpar.Options",
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

testthat::test_that("Bdpar verbose option not set",{

  path <- file.path("testFiles",
                    "testBdpar",
                    "tsms")

  pipeline <- DefaultPipeline$new()

  extractorFactory <- ExtractorFactory$new()

  cache <- FALSE

  verbose <- FALSE

  summary <- FALSE

  object <- Bdpar$new()

  bdpar.Options$remove("verbose")

  testthat::expect_error(object$execute(path = path,
                                        pipeline = pipeline,
                                        extractors = extractorFactory,
                                        cache = cache,
                                        verbose = verbose,
                                        summary = summary),
                         "[bdpar.log][FATAL] Verbose is not defined in bdpar.Options",
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

testthat::test_that("Bdpar default flow of pipes with the examples files tsms",{
  testthat::skip_if_not_installed("cld2")
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rtweet")
  testthat::skip_if_not_installed("stringi")
  testthat::skip_if_not_installed("stringr")

  bdpar.Options$set("extractorEML.mpaPartSelected", "text/plain")
  bdpar.Options$set("resources.abbreviations.path", "")
  bdpar.Options$set("resources.contractions.path", "")
  bdpar.Options$set("resources.interjections.path", "")
  bdpar.Options$set("resources.slangs.path", "")
  bdpar.Options$set("resources.stopwords.path", "")
  bdpar.Options$set("teeCSVPipe.output.path", file.path("testFiles",
                                                        "testBdpar",
                                                        "output_tsms.csv"))
  bdpar.Options$set("cache", FALSE)

  path <- file.path("testFiles",
                    "testBdpar",
                    "tsms")

  pipeline <- DefaultPipeline$new()

  extractorFactory <- ExtractorFactory$new()

  cache <- FALSE

  verbose <- FALSE

  summary <- FALSE

  object <- Bdpar$new()

  output <- suppressWarnings(object$execute(path = path,
                                            pipeline = pipeline,
                                            extractors = extractorFactory,
                                            cache = cache,
                                            verbose = verbose,
                                            summary = summary))
  file1 <- output[[1]]

  testthat::expect_equal(file1$getDate(), "")

  testthat::expect_equal(file1$getSource(), "Wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us")

  testthat::expect_equal(file1$getData(), "wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us")
  testthat::expect_equal(file1$getSpecificProperty("target"), "ham")
  testthat::expect_equal(file1$getSpecificProperty("extension"), "tsms")

  testthat::expect_equal(file1$getSpecificProperty("length_before_cleaning_text"), 130)

  testthat::expect_equal(file1$getSpecificProperty("userName"), as.character(c()))
  testthat::expect_equal(file1$getSpecificProperty("hashtag"), as.character(c()))
  testthat::expect_equal(file1$getSpecificProperty("URLs"), as.character(c()))
  testthat::expect_equal(file1$getSpecificProperty("emoticon"), as.character(c()))
  testthat::expect_equal(file1$getSpecificProperty("Emojis"), as.character(c()))
  testthat::expect_equal(file1$getSpecificProperty("language"), "en")
  testthat::expect_equal(file1$getSpecificProperty("contractions"), list())
  testthat::expect_equal(file1$getSpecificProperty("abbreviation"), list())
  testthat::expect_equal(file1$getSpecificProperty("langpropname"), list())
  testthat::expect_equal(file1$getSpecificProperty("interjection"), list())
  testthat::expect_equal(file1$getSpecificProperty("stopWord"), list())
  testthat::expect_equal(file1$getSpecificProperty("length_after_cleaning_text"), 130)
  testthat::expect_equal(file1$isInstanceValid(), TRUE)
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
                         c("FindUrlPipe", "FindHashtagPipe", "AbbreviationPipe"))

  file2 <- output[[2]]

  testthat::expect_equal(file2$getDate(),"")

  testthat::expect_equal(file2$getSource(),"Guess what! Somebody you know secretly fancies you! Wanna find out who it is? Give us a call on 09065394514 From Landline DATEBox1282EssexCM61XN 150p/min 18")


  testthat::expect_equal(file2$getData(),"guess what! somebody you know secretly fancies you! wanna find out who it is? give us a call on 09065394514 from landline datebox1282essexcm61xn 150p/min 18")
  testthat::expect_equal(file2$getSpecificProperty("target"), "spam")
  testthat::expect_equal(file2$getSpecificProperty("extension"), "tsms")

  testthat::expect_equal(file2$getSpecificProperty("length_before_cleaning_text"), 156)

  testthat::expect_equal(file2$getSpecificProperty("userName"), as.character(c()))
  testthat::expect_equal(file2$getSpecificProperty("hashtag"), as.character(c()))
  testthat::expect_equal(file2$getSpecificProperty("URLs"), as.character(c()))
  testthat::expect_equal(file2$getSpecificProperty("emoticon"), as.character(c()))
  testthat::expect_equal(file2$getSpecificProperty("Emojis"), as.character(c()))
  testthat::expect_equal(file2$getSpecificProperty("language"), "en")
  testthat::expect_equal(file2$getSpecificProperty("contractions"), list())
  testthat::expect_equal(file2$getSpecificProperty("abbreviation"), list())
  testthat::expect_equal(file2$getSpecificProperty("langpropname"), list())
  testthat::expect_equal(file2$getSpecificProperty("interjection"), list())
  testthat::expect_equal(file2$getSpecificProperty("stopWord"), list())
  testthat::expect_equal(file2$getSpecificProperty("length_after_cleaning_text"), 156)
  testthat::expect_equal(file2$isInstanceValid(), TRUE)
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
                         c("FindUrlPipe", "FindHashtagPipe", "AbbreviationPipe"))

  file.remove(file.path("testFiles",
                        "testBdpar",
                        "output_tsms.csv"))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
  if (file.exists(file.path("testFiles",
                            "testBdpar",
                            "output_tsms.csv"))) {
    file.remove(file.path("testFiles",
                          "testBdpar",
                          "output_tsms.csv"))
  }
})
}

if (Sys.info()[['sysname']] %in% "Windows") {

  testthat::setup({
    bdpar.Options$reset()
    bdpar.Options$configureLog()
  })

  testthat::test_that("Bdpar summary works",{
    testthat::skip_if_not_installed("cld2")
    testthat::skip_if_not_installed("rex")
    testthat::skip_if_not_installed("rjson")
    testthat::skip_if_not_installed("rtweet")
    testthat::skip_if_not_installed("stringi")
    testthat::skip_if_not_installed("stringr")


    bdpar.Options$set("extractorEML.mpaPartSelected", "text/plain")
    bdpar.Options$set("resources.abbreviations.path", "resourcesFiles/testResources/abbreviations-json")
    bdpar.Options$set("resources.contractions.path", "resourcesFiles/testResources/contractions-json")
    bdpar.Options$set("resources.interjections.path", "resourcesFiles/testResources/interjections-json")
    bdpar.Options$set("resources.slangs.path", "resourcesFiles/testResources/slangs-json")
    bdpar.Options$set("resources.stopwords.path", "resourcesFiles/testResources/stopwords-json")
    bdpar.Options$set("teeCSVPipe.output.path", file.path("testFiles",
                                                          "testBdpar",
                                                          "output_tsms.csv"))
    bdpar.Options$set("cache", FALSE)

    path <- file.path("testFiles",
                      "testBdpar",
                      "tsms")

    pipeline <- DefaultPipeline$new()

    extractorFactory <- ExtractorFactory$new()

    cache <- FALSE

    verbose <- TRUE

    summary <- TRUE

    object <- Bdpar$new()

    messageOutputExpected <-
      paste0("\\[Bdpar]\\[summary\\]\\[INFO\\] ",
             "Summary after bdpar execution\n\tPipeline executed: ",
             "\n\t\tinstance %>\\|%",
             "\n\t\t\tTargetAssigningPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tStoreFileExtPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tGuessDatePipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tFile2Pipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tMeasureLengthPipe\\$new\\(propertyName = \"length_before_cleaning_text\"\\) %>\\|%",
             "\n\t\t\tFindUserNamePipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tFindHashtagPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tFindUrlPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tFindEmoticonPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tFindEmojiPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tGuessLanguagePipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tContractionPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tAbbreviationPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tSlangPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tToLowerCasePipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tInterjectionPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tStopWordPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tMeasureLengthPipe\\$new\\(propertyName = \"length_after_cleaning_text\"\\) %>\\|%",
             "\n\t\t\tTeeCSVPipe\\$new\\(\\)",
             "\n\tValid instances: 2",
             "\n\tInvalid instances: 0",
             "\n\tAll the possible properties obtained in the different instances: 15",
             "\n\t\t- target",
             "\n\t\t- extension",
             "\n\t\t- length_before_cleaning_text",
             "\n\t\t- userName",
             "\n\t\t- hashtag",
             "\n\t\t- URLs",
             "\n\t\t- emoticon",
             "\n\t\t- Emojis",
             "\n\t\t- language",
             "\n\t\t- contractions",
             "\n\t\t- abbreviation",
             "\n\t\t- langpropname",
             "\n\t\t- interjection",
             "\n\t\t- stopWord",
             "\n\t\t- length_after_cleaning_text")


    output <- testthat::expect_message(object$execute(path = path,
                                                      pipeline = pipeline,
                                                      extractors = extractorFactory,
                                                      cache = cache,
                                                      verbose = verbose,
                                                      summary = summary),
                                       messageOutputExpected,
                                       perl = TRUE)

    file.remove(file.path("testFiles",
                          "testBdpar",
                          "output_tsms.csv"))
  })

  testthat::teardown({
    bdpar.Options$reset()
    bdpar.Options$configureLog()
    if (file.exists(file.path("testFiles",
                              "testBdpar",
                              "output_tsms.csv"))) {
      file.remove(file.path("testFiles",
                            "testBdpar",
                            "output_tsms.csv"))
    }
  })
}

if (Sys.info()[['sysname']] %in% "Windows") {

  testthat::setup({
    bdpar.Options$reset()
    bdpar.Options$configureLog()
  })

  testthat::test_that("Bdpar summary works",{
    testthat::skip_if_not_installed("cld2")
    testthat::skip_if_not_installed("rex")
    testthat::skip_if_not_installed("rjson")
    testthat::skip_if_not_installed("rtweet")
    testthat::skip_if_not_installed("stringi")
    testthat::skip_if_not_installed("stringr")

    bdpar.Options$set("extractorEML.mpaPartSelected", "text/plain")
    bdpar.Options$set("resources.abbreviations.path", "resourcesFiles/testResources/abbreviations-json")
    bdpar.Options$set("resources.contractions.path", "resourcesFiles/testResources/contractions-json")
    bdpar.Options$set("resources.interjections.path", "resourcesFiles/testResources/interjections-json")
    bdpar.Options$set("resources.slangs.path", "resourcesFiles/testResources/slangs-json")
    bdpar.Options$set("resources.stopwords.path", "resourcesFiles/testResources/stopwords-json")
    bdpar.Options$set("teeCSVPipe.output.path", file.path("testFiles",
                                                          "testBdpar",
                                                          "output_tsms.csv"))
    bdpar.Options$set("cache", FALSE)

    path <- file.path("testFiles",
                      "testBdpar",
                      "tsms")

    pipeline <- DefaultPipeline$new()

    extractorFactory <- ExtractorFactory$new()

    cache <- FALSE

    verbose <- FALSE

    summary <- TRUE

    object <- Bdpar$new()

    messageOutputExpected <-
      paste0("\\[Bdpar]\\[summary\\]\\[INFO\\] ",
             "Summary after bdpar execution\n\tPipeline executed: ",
             "\n\t\tinstance %>\\|%",
             "\n\t\t\tTargetAssigningPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tStoreFileExtPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tGuessDatePipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tFile2Pipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tMeasureLengthPipe\\$new\\(propertyName = \"length_before_cleaning_text\"\\) %>\\|%",
             "\n\t\t\tFindUserNamePipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tFindHashtagPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tFindUrlPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tFindEmoticonPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tFindEmojiPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tGuessLanguagePipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tContractionPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tAbbreviationPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tSlangPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tToLowerCasePipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tInterjectionPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tStopWordPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tMeasureLengthPipe\\$new\\(propertyName = \"length_after_cleaning_text\"\\) %>\\|%",
             "\n\t\t\tTeeCSVPipe\\$new\\(\\)",
             "\n\tValid instances: 2",
             "\n\tInvalid instances: 0",
             "\n\tAll the possible properties obtained in the different instances: 15",
             "\n\t\t- target",
             "\n\t\t- extension",
             "\n\t\t- length_before_cleaning_text",
             "\n\t\t- userName",
             "\n\t\t- hashtag",
             "\n\t\t- URLs",
             "\n\t\t- emoticon",
             "\n\t\t- Emojis",
             "\n\t\t- language",
             "\n\t\t- contractions",
             "\n\t\t- abbreviation",
             "\n\t\t- langpropname",
             "\n\t\t- interjection",
             "\n\t\t- stopWord",
             "\n\t\t- length_after_cleaning_text")

    bdpar.Options$disableLog()

    output <- testthat::expect_message(object$execute(path = path,
                                                      pipeline = pipeline,
                                                      extractors = extractorFactory,
                                                      cache = cache,
                                                      verbose = verbose,
                                                      summary = summary),
                                       messageOutputExpected,
                                       perl = TRUE)

    file.remove(file.path("testFiles",
                          "testBdpar",
                          "output_tsms.csv"))
  })

  testthat::teardown({
    bdpar.Options$reset()
    bdpar.Options$configureLog()
    if (file.exists(file.path("testFiles",
                              "testBdpar",
                              "output_tsms.csv"))) {
      file.remove(file.path("testFiles",
                            "testBdpar",
                            "output_tsms.csv"))
    }
  })
}

if (Sys.info()[['sysname']] %in% "Windows") {

  testthat::setup({
    bdpar.Options$reset()
    bdpar.Options$configureLog()
  })

  testthat::test_that("Bdpar summary works with invalid instance",{
    testthat::skip_if_not_installed("cld2")
    testthat::skip_if_not_installed("rex")
    testthat::skip_if_not_installed("rjson")
    testthat::skip_if_not_installed("rtweet")
    testthat::skip_if_not_installed("stringi")
    testthat::skip_if_not_installed("stringr")

    bdpar.Options$set("extractorEML.mpaPartSelected", "text/plain")
    bdpar.Options$set("resources.abbreviations.path", "resourcesFiles/testResources/abbreviations-json")
    bdpar.Options$set("resources.contractions.path", "resourcesFiles/testResources/contractions-json")
    bdpar.Options$set("resources.interjections.path", "resourcesFiles/testResources/interjections-json")
    bdpar.Options$set("resources.slangs.path", "resourcesFiles/testResources/slangs-json")
    bdpar.Options$set("resources.stopwords.path", "resourcesFiles/testResources/stopwords-json")
    bdpar.Options$set("teeCSVPipe.output.path", file.path("testFiles",
                                                          "testBdpar",
                                                          "output_tsms.csv"))
    bdpar.Options$set("cache", FALSE)

    path <- file.path("testFiles",
                      "testBdpar",
                      "tsms-1fileInvalid")

    pipeline <- DefaultPipeline$new()

    extractorFactory <- ExtractorFactory$new()

    cache <- FALSE

    verbose <- TRUE

    summary <- TRUE

    object <- Bdpar$new()

    messageOutputExpected <-
      paste0("[-\\[\\]:0-9 ]+\\[Bdpar]\\[summary\\]\\[INFO\\] ",
             "Summary after bdpar execution\n\tPipeline executed: ",
             "\n\t\tinstance %>\\|%",
             "\n\t\t\tTargetAssigningPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tStoreFileExtPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tGuessDatePipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tFile2Pipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tMeasureLengthPipe\\$new\\(propertyName = \"length_before_cleaning_text\"\\) %>\\|%",
             "\n\t\t\tFindUserNamePipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tFindHashtagPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tFindUrlPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tFindEmoticonPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tFindEmojiPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tGuessLanguagePipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tContractionPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tAbbreviationPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tSlangPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tToLowerCasePipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tInterjectionPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tStopWordPipe\\$new\\(\\) %>\\|%",
             "\n\t\t\tMeasureLengthPipe\\$new\\(propertyName = \"length_after_cleaning_text\"\\) %>\\|%",
             "\n\t\t\tTeeCSVPipe\\$new\\(\\)",
             "\n\tValid instances: 1",
             "\n\tInvalid instances: 1",
             "\n\t\t- testFiles\\/testBdpar\\/tsms-1fileInvalid\\/_ham_\\/30\\.tsms : The file: testFiles\\/testBdpar\\/tsms-1fileInvalid\\/_ham_\\/30\\.tsms has source empty",
             "\n\tAll the possible properties obtained in the different instances: 16",
             "\n\t\t- target",
             "\n\t\t- extension",
             "\n\t\t- reasonToInvalidate",
             "\n\t\t- length_before_cleaning_text",
             "\n\t\t- userName",
             "\n\t\t- hashtag",
             "\n\t\t- URLs",
             "\n\t\t- emoticon",
             "\n\t\t- Emojis",
             "\n\t\t- language",
             "\n\t\t- contractions",
             "\n\t\t- abbreviation",
             "\n\t\t- langpropname",
             "\n\t\t- interjection",
             "\n\t\t- stopWord",
             "\n\t\t- length_after_cleaning_text")


    output <- testthat::expect_message(suppressWarnings(object$execute(path = path,
                                                                       pipeline = pipeline,
                                                                       extractors = extractorFactory,
                                                                       cache = cache,
                                                                       verbose = verbose,
                                                                       summary = summary)),
                                       messageOutputExpected,
                                       perl = TRUE)

    file.remove(file.path("testFiles",
                          "testBdpar",
                          "output_tsms.csv"))
  })

  testthat::teardown({
    bdpar.Options$reset()
    bdpar.Options$configureLog()
    if (file.exists(file.path("testFiles",
                              "testBdpar",
                              "output_tsms.csv"))) {
      file.remove(file.path("testFiles",
                            "testBdpar",
                            "output_tsms.csv"))
    }
  })
}

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("Bdpar parallel works",{
  testthat::skip_on_cran()
  testthat::skip_if(parallel::detectCores() - 2 > 2,  message = "Test if the number of cores is less or equal than two cores to avoid fails in R CMD check")
  testthat::skip_if(parallel::detectCores() < 4,  message = "Test if the number of cores is less than four cores to avoid fails in R CMD check")
  testthat::skip_if_not_installed("cld2")
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rtweet")
  testthat::skip_if_not_installed("stringi")
  testthat::skip_if_not_installed("stringr")


  bdpar.Options$set("numCores", 2)
  bdpar.Options$set("extractorEML.mpaPartSelected", "text/plain")
  bdpar.Options$set("resources.abbreviations.path", "")
  bdpar.Options$set("resources.contractions.path", "")
  bdpar.Options$set("resources.interjections.path", "")
  bdpar.Options$set("resources.slangs.path", "")
  bdpar.Options$set("resources.stopwords.path", "")
  bdpar.Options$set("teeCSVPipe.output.path", file.path("testFiles",
                                                        "testBdpar",
                                                        "output_tsms.csv"))
  bdpar.Options$set("cache", FALSE)

  bdpar.Options$configureLog(console = TRUE,
                             threshold = "DEBUG",
                             file = file.path("testFiles",
                                              "testBdpar",
                                              "log_parallel.txt"))

  path <- file.path("testFiles",
                    "testBdpar",
                    "tsms")

  pipeline <- DefaultPipeline$new()

  extractorFactory <- ExtractorFactory$new()

  cache <- FALSE

  verbose <- TRUE

  summary <- FALSE

  object <- Bdpar$new()

  output <- testthat::expect_message(suppressWarnings(object$execute(path = path,
                                                                     pipeline = pipeline,
                                                                     extractors = extractorFactory,
                                                                     cache = cache,
                                                                     verbose = verbose,
                                                                     summary = summary)),
                                     "[-\\[\\]:0-9 ]+\\[Bdpar\\]\\[makeCluster\\]\\[DEBUG\\] Initiating cluster with 2 threads",
                                     perl = TRUE)
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
                            "testBdpar",
                            "log_parallel.txt"))) {
    file.remove(file.path("testFiles",
                          "testBdpar",
                          "log_parallel.txt"))
  }
  if (file.exists(file.path("testFiles",
                            "testBdpar",
                            "output_tsms.csv"))) {
    file.remove(file.path("testFiles",
                          "testBdpar",
                          "output_tsms.csv"))
  }
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("Bdpar parallel works with more files than cores",{
  testthat::skip_on_cran()
  testthat::skip_if(parallel::detectCores() - 2 > 2,  message = "Test if the number of cores is less or equal than two cores to avoid fails in R CMD check")
  testthat::skip_if_not_installed("cld2")
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rtweet")
  testthat::skip_if_not_installed("stringi")
  testthat::skip_if_not_installed("stringr")


  bdpar.Options$set("numCores", 2)
  bdpar.Options$set("extractorEML.mpaPartSelected", "text/plain")
  bdpar.Options$set("resources.abbreviations.path", "resourcesFiles/testResources/abbreviations-json")
  bdpar.Options$set("resources.contractions.path", "resourcesFiles/testResources/contractions-json")
  bdpar.Options$set("resources.interjections.path", "resourcesFiles/testResources/interjections-json")
  bdpar.Options$set("resources.slangs.path", "resourcesFiles/testResources/slangs-json")
  bdpar.Options$set("resources.stopwords.path", "resourcesFiles/testResources/stopwords-json")
  bdpar.Options$set("teeCSVPipe.output.path", file.path("testFiles",
                                                        "testBdpar",
                                                        "output_tsms.csv"))
  bdpar.Options$set("cache", FALSE)

  bdpar.Options$configureLog(console = TRUE,
                             threshold = "DEBUG",
                             file = file.path("testFiles",
                                              "testBdpar",
                                              "log_parallel.txt"))

  path <- file.path("testFiles",
                    "testBdpar",
                    "tsms5Files")

  pipeline <- DefaultPipeline$new()

  extractorFactory <- ExtractorFactory$new()

  cache <- FALSE

  verbose <- TRUE

  summary <- TRUE

  object <- Bdpar$new()

  messageOutputExpected <-
    paste0("[-\\[\\]:0-9 ]+\\[Bdpar]\\[summary\\]\\[INFO\\] ",
           "Summary after bdpar execution\n\tPipeline executed: ",
           "\n\t\tinstance %>\\|%",
           "\n\t\t\tTargetAssigningPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tStoreFileExtPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tGuessDatePipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tFile2Pipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tMeasureLengthPipe\\$new\\(propertyName = \"length_before_cleaning_text\"\\) %>\\|%",
           "\n\t\t\tFindUserNamePipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tFindHashtagPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tFindUrlPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tFindEmoticonPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tFindEmojiPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tGuessLanguagePipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tContractionPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tAbbreviationPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tSlangPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tToLowerCasePipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tInterjectionPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tStopWordPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tMeasureLengthPipe\\$new\\(propertyName = \"length_after_cleaning_text\"\\) %>\\|%",
           "\n\t\t\tTeeCSVPipe\\$new\\(\\)",
           "\n\tValid instances: 5",
           "\n\tInvalid instances: 0",
           "\n\tAll the possible properties obtained in the different instances: 15",
           "\n\t\t- target",
           "\n\t\t- extension",
           "\n\t\t- length_before_cleaning_text",
           "\n\t\t- userName",
           "\n\t\t- hashtag",
           "\n\t\t- URLs",
           "\n\t\t- emoticon",
           "\n\t\t- Emojis",
           "\n\t\t- language",
           "\n\t\t- contractions",
           "\n\t\t- abbreviation",
           "\n\t\t- langpropname",
           "\n\t\t- interjection",
           "\n\t\t- stopWord",
           "\n\t\t- length_after_cleaning_text")

  output <- testthat::expect_message(object$execute(path = path,
                                                    pipeline = pipeline,
                                                    extractors = extractorFactory,
                                                    cache = cache,
                                                    verbose = verbose,
                                                    summary = summary),
                                     messageOutputExpected,
                                     perl = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
  if (file.exists(file.path("testFiles",
                            "testBdpar",
                            "log_parallel.txt"))) {
    file.remove(file.path("testFiles",
                          "testBdpar",
                          "log_parallel.txt"))
  }
  if (file.exists(file.path("testFiles",
                            "testBdpar",
                            "output_tsms.csv"))) {
    file.remove(file.path("testFiles",
                          "testBdpar",
                          "output_tsms.csv"))
  }
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("Bdpar parallel works with invalid instances",{
  testthat::skip_on_cran()
  testthat::skip_if(parallel::detectCores() - 2 > 2,  message = "Test if the number of cores is less or equal than two cores to avoid fails in R CMD check")
  testthat::skip_if_not_installed("cld2")
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rtweet")
  testthat::skip_if_not_installed("stringi")
  testthat::skip_if_not_installed("stringr")

  bdpar.Options$set("numCores", 2)
  bdpar.Options$set("extractorEML.mpaPartSelected", "text/plain")
  bdpar.Options$set("resources.abbreviations.path", "resourcesFiles/testResources/abbreviations-json")
  bdpar.Options$set("resources.contractions.path", "resourcesFiles/testResources/contractions-json")
  bdpar.Options$set("resources.interjections.path", "resourcesFiles/testResources/interjections-json")
  bdpar.Options$set("resources.slangs.path", "resourcesFiles/testResources/slangs-json")
  bdpar.Options$set("resources.stopwords.path", "resourcesFiles/testResources/stopwords-json")
  bdpar.Options$set("teeCSVPipe.output.path", file.path("testFiles",
                                                        "testBdpar",
                                                        "output_tsms.csv"))
  bdpar.Options$set("cache", FALSE)

  bdpar.Options$configureLog(console = TRUE,
                             threshold = "DEBUG",
                             file = file.path("testFiles",
                                              "testBdpar",
                                              "log_parallel.txt"))

  path <- file.path("testFiles",
                    "testBdpar",
                    "tsms-1fileInvalid")

  pipeline <- DefaultPipeline$new()

  extractorFactory <- ExtractorFactory$new()

  cache <- FALSE

  verbose <- TRUE

  summary <- TRUE

  object <- Bdpar$new()

  messageOutputExpected <-
    paste0("[-\\[\\]:0-9 ]+\\[Bdpar]\\[summary\\]\\[INFO\\] ",
           "Summary after bdpar execution\n\tPipeline executed: ",
           "\n\t\tinstance %>\\|%",
           "\n\t\t\tTargetAssigningPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tStoreFileExtPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tGuessDatePipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tFile2Pipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tMeasureLengthPipe\\$new\\(propertyName = \"length_before_cleaning_text\"\\) %>\\|%",
           "\n\t\t\tFindUserNamePipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tFindHashtagPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tFindUrlPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tFindEmoticonPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tFindEmojiPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tGuessLanguagePipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tContractionPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tAbbreviationPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tSlangPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tToLowerCasePipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tInterjectionPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tStopWordPipe\\$new\\(\\) %>\\|%",
           "\n\t\t\tMeasureLengthPipe\\$new\\(propertyName = \"length_after_cleaning_text\"\\) %>\\|%",
           "\n\t\t\tTeeCSVPipe\\$new\\(\\)",
           "\n\tValid instances: 1",
           "\n\tInvalid instances: 1",
           "\n\t\t- testFiles\\/testBdpar\\/tsms-1fileInvalid\\/_ham_\\/30\\.tsms : The file: testFiles\\/testBdpar\\/tsms-1fileInvalid\\/_ham_\\/30\\.tsms has source empty",
           "\n\tAll the possible properties obtained in the different instances: 16",
           "\n\t\t- target",
           "\n\t\t- extension",
           "\n\t\t- reasonToInvalidate",
           "\n\t\t- length_before_cleaning_text",
           "\n\t\t- userName",
           "\n\t\t- hashtag",
           "\n\t\t- URLs",
           "\n\t\t- emoticon",
           "\n\t\t- Emojis",
           "\n\t\t- language",
           "\n\t\t- contractions",
           "\n\t\t- abbreviation",
           "\n\t\t- langpropname",
           "\n\t\t- interjection",
           "\n\t\t- stopWord",
           "\n\t\t- length_after_cleaning_text")

  output <- testthat::expect_message(suppressWarnings(object$execute(path = path,
                                                                     pipeline = pipeline,
                                                                     extractors = extractorFactory,
                                                                     cache = cache,
                                                                     verbose = verbose,
                                                                     summary = summary)),
                                     messageOutputExpected,
                                     perl = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
  if (file.exists(file.path("testFiles",
                            "testBdpar",
                            "log_parallel.txt"))) {
    file.remove(file.path("testFiles",
                          "testBdpar",
                          "log_parallel.txt"))
  }
  if (file.exists(file.path("testFiles",
                            "testBdpar",
                            "output_tsms.csv"))) {
    file.remove(file.path("testFiles",
                          "testBdpar",
                          "output_tsms.csv"))
  }
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("Bdpar parallel invalid number of cores",{
  testthat::skip_on_cran()
  testthat::skip_if(parallel::detectCores() - 2 > 2,  message = "Test if the number of cores is less or equal than two cores to avoid fails in R CMD check")
  testthat::skip_if_not_installed("parallel")
  testthat::skip_if_not_installed("cld2")
  testthat::skip_if_not_installed("rex")
  testthat::skip_if_not_installed("rjson")
  testthat::skip_if_not_installed("rtweet")
  testthat::skip_if_not_installed("stringi")
  testthat::skip_if_not_installed("stringr")

  bdpar.Options$set("extractorEML.mpaPartSelected", "text/plain")
  bdpar.Options$set("resources.abbreviations.path", "resourcesFiles/testResources/abbreviations-json")
  bdpar.Options$set("resources.contractions.path", "resourcesFiles/testResources/contractions-json")
  bdpar.Options$set("resources.interjections.path", "resourcesFiles/testResources/interjections-json")
  bdpar.Options$set("resources.slangs.path", "resourcesFiles/testResources/slangs-json")
  bdpar.Options$set("resources.stopwords.path", "resourcesFiles/testResources/stopwords-json")
  bdpar.Options$set("teeCSVPipe.output.path", file.path("testFiles",
                                                        "testBdpar",
                                                        "output_tsms.csv"))

  bdpar.Options$configureLog(console = TRUE,
                             threshold = "DEBUG",
                             file = file.path("testFiles",
                                              "testBdpar",
                                              "log_parallel_error.txt"))

  path <- file.path("testFiles",
                    "testBdpar",
                    "tsms")

  pipeline <- DefaultPipeline$new()

  extractorFactory <- ExtractorFactory$new()

  cache <- FALSE

  verbose <- TRUE

  summary <- FALSE

  object <- Bdpar$new()

  bdpar.Options$set("numCores", -2)

  object$execute(path = path,
                 pipeline = pipeline,
                 extractors = extractorFactory,
                 cache = cache,
                 verbose = verbose,
                 summary = summary)

  if (parallel::detectCores() - 2 < 1) {
    numCoresExpected <- 1
  } else {
    numCoresExpected <- parallel::detectCores() - 2
  }

  testthat::expect_equal(bdpar.Options$get("numCores"),
                         numCoresExpected)

  bdpar.Options$remove("numCores")

  object$execute(path = path,
                 pipeline = pipeline,
                 extractors = extractorFactory,
                 cache = cache,
                 verbose = verbose,
                 summary = summary)

  testthat::expect_equal(bdpar.Options$get("numCores"),
                         numCoresExpected)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
  if (file.exists(file.path("testFiles",
                            "testBdpar",
                            "log_parallel_error.txt"))) {
    file.remove(file.path("testFiles",
                          "testBdpar",
                          "log_parallel_error.txt"))
  }
  if (file.exists(file.path("testFiles",
                            "testBdpar",
                            "output_tsms.csv"))) {
    file.remove(file.path("testFiles",
                          "testBdpar",
                          "output_tsms.csv"))
  }
})
