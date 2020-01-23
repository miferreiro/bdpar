#
# Bdpar provide a tool to easily build customized data flows to pre-process
# large volumes of information from different sources. To this end, bdpar allows
# to (i) easily use and create new functionalities and (ii) develop new data
# source extractors according to the user needs. Additionally, the package
# provides by default a predefined data flow to extract and preprocess the most
# relevant information (tokens, dates, ... ) from some textual sources (SMS,
# email, tweets, YouTube comments).
#
# Copyright (C) 2018 Sing Group (University of Vigo)
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <https://www.gnu.org/licenses/gpl-3.0.html>

#' @title Class to find and/or remove the stop words on the data field of an Instance
#'
#' @description \code{\link{StopWordPipe}} class is responsible for detecting
#' the existing stop words in the \strong{data} field of each \code{\link{Instance}}.
#' Identified stop words are stored inside the \strong{contraction} field of
#' \code{\link{Instance}} class. Moreover if needed, is able to perform inline
#' stop words removement.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \preformatted{
#' StopWordPipe$new(propertyName = "stopWord",
#'                  propertyLanguageName = "language",
#'                  alwaysBeforeDeps = list("GuessLanguagePipe"),
#'                  notAfterDeps = list("AbbreviationPipe"),
#'                  removeStopWords = TRUE)
#' }
#' \itemize{
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{propertyName:}}{
#' (\emph{character}) name of the property associated with the Pipe.
#' }
#' \item{\strong{propertyLanguageName:}}{
#' (\emph{character}) name of the language property.
#' }
#' \item{\strong{alwaysBeforeDeps:}}{
#' (\emph{list}) the dependences alwaysBefore (Pipes that must be executed before this
#' one).
#' }
#' \item{\strong{notAfterDeps:}}{
#' (\emph{list}) the dependences notAfter (Pipes that cannot be executed after this one).
#' }
#' \item{\strong{removeStopWords:}}{
#' (\emph{logical}) indicates if the stop words are removed or not.
#' }
#' }
#' }
#' }
#'
#' @section Details:
#' \code{\link{StopWordPipe}} class requires the resource files (in json format)
#' containing the list of stop words. To this end, the language of the text
#' indicated in the \emph{propertyLanguageName} should be contained in the
#' resource file name (ie. xxx.json where xxx is the value defined in the
#' \emph{propertyLanguageName} ). The location of the resources should defined
#' in the \emph{resourcesPath} section of the configuration file.
#'
#' \strong{[resourcesPath]}
#'
#' resourcesStopWordsPath = \emph{<<resources_stopWords_path>>}
#'
#' @section Note:
#' \code{\link{StopWordPipe}} will automatically invalidate the
#' \code{\link{Instance}} whenever the obtained data is empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe:}}{
#' preprocesses the \code{\link{Instance}} to obtain/remove the stop words.
#' The stop words found in the pipe are added to the list of properties of
#' the \code{\link{Instance}}.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{pipe(instance)}
#' }
#' \item{\emph{Value:}}{
#' the \code{\link{Instance}} with the modifications that have occurred in the Pipe.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{instance:}}{
#' (\emph{Instance}) \code{\link{Instance}} to preproccess.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{findStopWord:}}{
#' checks if the stop word is in the data.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{findStopWord(data, stopWord)}
#' }
#' \item{\emph{Value:}}{
#' boolean, depending on whether the stop word is on the data.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{data:}}{
#' (\emph{character}) text where stop words will be searched.
#' }
#' \item{\strong{stopWord:}}{
#' (\emph{character}) Indicates the stop word to find.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{removeStopWord:}}{
#' removes the stop word in the data.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{removeStopWord(stopWord, data)}
#' }
#' \item{\emph{Value:}}{
#' the data with stop word removed.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{stopWord:}}{
#' (\emph{character}) indicates the stop word to remove.
#' }
#' \item{\strong{data:}}{
#' (\emph{character}) text where stop words will be removed.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getPropertyLanguageName:}}{
#' gets of name of property language.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getPropertyLanguageName()}
#' }
#' \item{\emph{Value:}}{
#' value of name of property language.
#' }
#' }
#' }
#'
#' \item{\bold{getPathResourcesStopWords:}}{
#' gets of path of stop words resources.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getPathResourcesStopWords()}
#' }
#' \item{\emph{Value:}}{
#' value of path of stop words resources.
#' }
#' }
#' }
#'
#' \item{\bold{setPathResourcesStopWords:}}{
#' sets the path of stop words resources.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{setPathResourcesStopWords(path)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{path:}}{
#' (\emph{character}) the new value of the path of stop words resources.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{propertyLanguageName:}}{
#'  (\emph{character}) the name of property about language.
#' }
#' \item{\bold{pathResourcesStopWords:}}{
#'  (\emph{character}) the path where are the resources.
#' }
#' \item{\bold{removeStopWords:}}{
#' (\emph{logical}) indicates if the stop words are removed or not.
#' }
#' }
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{ContractionPipe}},
#'          \code{\link{File2Pipe}}, \code{\link{FindEmojiPipe}},
#'          \code{\link{FindEmoticonPipe}}, \code{\link{FindHashtagPipe}},
#'          \code{\link{FindUrlPipe}}, \code{\link{FindUserNamePipe}},
#'          \code{\link{GuessDatePipe}}, \code{\link{GuessLanguagePipe}},
#'          \code{\link{Instance}}, \code{\link{InterjectionPipe}},
#'          \code{\link{MeasureLengthPipe}}, \code{\link{PipeGeneric}},
#'          \code{\link{ResourceHandler}}, \code{\link{SlangPipe}},
#'          \code{\link{StoreFileExtPipe}}, \code{\link{TargetAssigningPipe}},
#'          \code{\link{TeeCSVPipe}}, \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import ini pipeR R6 rlist
#' @export StopWordPipe

StopWordPipe <- R6Class(

  "StopWordPipe",

  inherit = PipeGeneric,

  public = list(

    initialize = function(propertyName = "stopWord",
                          propertyLanguageName = "language",
                          alwaysBeforeDeps = list("GuessLanguagePipe"),
                          notAfterDeps = list("AbbreviationPipe"),
                          removeStopWords = TRUE) {

      if (!requireNamespace("rex", quietly = TRUE)) {
        stop("[StopWordPipe][initialize][Error]
                Package \"rex\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!requireNamespace("textutils", quietly = TRUE)) {
        stop("[StopWordPipe][initialize][Error]
                Package \"textutils\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!"character" %in% class(propertyName)) {
        stop("[StopWordPipe][initialize][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }

      if (!"character" %in% class(propertyLanguageName)) {
        stop("[StopWordPipe][initialize][Error]
                Checking the type of the variable: propertyLanguageName ",
                  class(propertyLanguageName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[StopWordPipe][initialize][Error]
                Checking the type of the variable: alwaysBeforeDeps ",
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[StopWordPipe][initialize][Error]
                 Checking the type of the variable: notAfterDeps ",
                  class(notAfterDeps))
      }

      if (!"logical" %in% class(removeStopWords)) {
        stop("[StopWordPipe][initialize][Error]
                Checking the type of the variable: removeStopWords ",
                  class(removeStopWords))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)

      private$propertyLanguageName <- propertyLanguageName

      private$resourcesStopWordsPath <- read.ini(Bdpar[["private_fields"]][["configurationFilePath"]])$resourcesPath$resourcesStopWordsPath

      private$removeStopWords <- removeStopWords
    },

    pipe = function(instance) {

      if (!"Instance" %in% class(instance)) {
        stop("[StopWordPipe][pipe][Error]
                Checking the type of the variable: instance ",
                  class(instance))
      }

      instance$addFlowPipes("StopWordPipe")

      if (!instance$checkCompatibility("StopWordPipe", self$getAlwaysBeforeDeps())) {
        stop("[StopWordPipe][pipe][Error] Bad compatibility between Pipes.")
      }

      instance$addBanPipes(unlist(super$getNotAfterDeps()))

      languageInstance <- "Unknown"

      languageInstance <- instance$getSpecificProperty(self$getPropertyLanguageName())

      # If the language property is not found, the instance can not be preprocessed
      if (is.null(languageInstance) ||
            is.na(languageInstance) ||
              "Unknown" %in% languageInstance) {

        instance$addProperties(list(), super$getPropertyName())

        message <-
          paste("The file: " ,
                instance$getPath() ,
                " has not language property",
                sep = "")

        warning("[StopWordPipe][pipe][Warning] ", message, " \n")


        return(instance)
      }

      JsonFile <- paste(self$getResourcesStopWordsPath(),
                        "/",
                        languageInstance,
                        ".json",
                        sep = "")

      jsonData <- Bdpar[["private_fields"]][["resourceHandler"]]$isLoadResource(JsonFile)

      if (!is.null(jsonData)) {

        #Variable which stores the stopwords located in the data
        stopWordLocated <- list()

        for (stopWord in jsonData) {

          if (self$findStopWord(instance$getData(), stopWord)) {
            stopWordLocated <- list.append(stopWordLocated, stopWord)
          }

          if (private$removeStopWords && stopWord %in% stopWordLocated) {

            instance$getData() %>>%
              {self$removeStopWord(stopWord, .)} %>>%
                textutils::trim() %>>%
                  instance$setData()
          }
        }

        instance$addProperties(paste(stopWordLocated),
                                super$getPropertyName())

      } else {

        instance$addProperties(list(), super$getPropertyName())

        message <-
          paste(
            "The file: " ,
            instance$getPath() ,
            " has not an StopWordsJsonFile to apply to the language-> ",
            languageInstance,
            sep = ""
          )

        warning("[StopWordPipe][pipe][Warning] ", message, " \n")

        return(instance)
      }

      if (is.na(instance$getData()) ||
          all(instance$getData() == "") ||
          is.null(instance$getData())) {

        message <- c( "The file: " , instance$getPath() , " has data empty on pipe StopWord")

        instance$addProperties(message, "reasonToInvalidate")

        warning("[StopWordPipe][pipe][Warning] ", message, " \n")

        instance$invalidate()

        return(instance)
      }

      return(instance)
    },

    findStopWord = function(data, stopWord) {

      if (!"character" %in% class(data)) {
        stop("[StopWordPipe][findStopWord][Error]
                Checking the type of the variable: data ",
                  class(data))
      }

      if (!"character" %in% class(stopWord)) {
        stop("[StopWordPipe][findStopWord][Error]
                Checking the type of the variable: stopWord ",
                  class(stopWord))
      }

      stopWordEscaped <- rex::escape(stopWord)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><\u00A1?\u00BF!;:,.'-]|^)(" ,
                                 stopWordEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      return(grepl(pattern = rex::regex(regularExpresion), x = data , perl = TRUE))

    },

    removeStopWord = function(stopWord, data) {

      if (!"character" %in% class(stopWord)) {
        stop("[StopWordPipe][removeStopWord][Error]
                Checking the type of the variable: stopWord ",
                  class(stopWord))
      }

      if (!"character" %in% class(data)) {
        stop("[StopWordPipe][removeStopWord][Error]
                Checking the type of the variable: data ",
                  class(data))
      }

      stopWordEscaped <- rex::escape(stopWord)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><\u00A1?\u00BF!;:,.'-]|^)(" ,
                                 stopWordEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      return(gsub(rex::regex(regularExpresion),"", data, perl = TRUE))
    },

    getPropertyLanguageName = function() {

      return(private$propertyLanguageName)
    },

    getResourcesStopWordsPath = function() {

      return(private$resourcesStopWordsPath)
    },

    setResourcesStopWordsPath = function(path) {

      if (!"character" %in% class(path)) {
        stop("[StopWordPipe][setResourcesStopWordsPath][Error]
                Checking the type of the variable: path ",
                  class(path))
      }

      private$resourcesStopWordsPath <- path

      return()
    }
  ),

  private = list(
    propertyLanguageName = "",
    resourcesStopWordsPath = "",
    removeStopWords = TRUE
  )
)
