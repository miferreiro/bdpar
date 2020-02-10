#
# Bdpar provide a tool to easily build customized data flows to pre-process
# large volumes of information from different sources. To this end, bdpar allows
# to (i) easily use and create new functionalities and (ii) develop new data
# source extractors according to the user needs. Additionally, the package
# provides by default a predefined data flow to extract and preprocess the most
# relevant information (tokens, dates, ... ) from some textual sources (SMS,
# email, tweets, YouTube comments).
#
# Copyright (C) 2020 Sing Group (University of Vigo)
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
#' \emph{propertyLanguageName} ). The location of the resources should be
#' defined in the \strong{"resources.stopwords.path"} field of
#' \emph{\link{bdpar.Options}} variable.
#'
#' @section Note:
#' \code{\link{StopWordPipe}} will automatically invalidate the
#' \code{\link{Instance}} whenever the obtained data is empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{GenericPipe}} and implements the
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
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{bdpar.Options}},
#'          \code{\link{ContractionPipe}}, \code{\link{File2Pipe}},
#'          \code{\link{FindEmojiPipe}}, \code{\link{FindEmoticonPipe}},
#'          \code{\link{FindHashtagPipe}}, \code{\link{FindUrlPipe}},
#'          \code{\link{FindUserNamePipe}}, \code{\link{GuessDatePipe}},
#'          \code{\link{GuessLanguagePipe}}, \code{\link{Instance}},
#'          \code{\link{InterjectionPipe}}, \code{\link{MeasureLengthPipe}},
#'          \code{\link{GenericPipe}}, \code{\link{ResourceHandler}},
#'          \code{\link{SlangPipe}}, \code{\link{StoreFileExtPipe}},
#'          \code{\link{TargetAssigningPipe}}, \code{\link{TeeCSVPipe}},
#'          \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import pipeR R6 rlist
#' @export StopWordPipe

StopWordPipe <- R6Class(

  "StopWordPipe",

  inherit = GenericPipe,

  public = list(

    initialize = function(propertyName = "stopWord",
                          propertyLanguageName = "language",
                          alwaysBeforeDeps = list("GuessLanguagePipe"),
                          notAfterDeps = list("AbbreviationPipe"),
                          removeStopWords = TRUE,
                          resourcesStopWordsPath = NULL) {

      if (!"character" %in% class(propertyName)) {
        stop("[StopWordPipe][initialize][Error] ",
             "Checking the type of the 'propertyName' variable: ",
             class(propertyName))
      }

      if (!"character" %in% class(propertyLanguageName)) {
        stop("[StopWordPipe][initialize][Error] ",
             "Checking the type of the 'propertyLanguageName' variable: ",
             class(propertyLanguageName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[StopWordPipe][initialize][Error] ",
             "Checking the type of the 'alwaysBeforeDeps' variable: ",
             class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[StopWordPipe][initialize][Error] ",
             "Checking the type of the 'notAfterDeps' variable: ",
             class(notAfterDeps))
      }

      if (!"logical" %in% class(removeStopWords)) {
        stop("[StopWordPipe][initialize][Error] ",
             "Checking the type of the 'removeStopWords' variable: ",
             class(removeStopWords))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)

      private$propertyLanguageName <- propertyLanguageName

      if (is.null(resourcesStopWordsPath)) {
        if (!all(bdpar.Options$isSpecificOption("resources.stopwords.path"),
                 !is.null(bdpar.Options$get("resources.stopwords.path")))) {
          stop("[StopWordPipe][initialize][Error] Path of stop words ",
               "resources is neither defined in initialize or in bdpar.Options")
        } else {
          resourcesStopWordsPath <- bdpar.Options$get("resources.stopwords.path")
        }
      }

      if (!"character" %in% class(resourcesStopWordsPath)) {
        stop("[StopWordPipe][initialize][Error] ",
             "Checking the type of the 'resourcesStopWordsPath' variable: ",
             class(resourcesStopWordsPath))
      }

      private$resourcesStopWordsPath <- resourcesStopWordsPath

      private$removeStopWords <- removeStopWords
    },

    pipe = function(instance) {

      if (!"Instance" %in% class(instance)) {
        stop("[StopWordPipe][pipe][Error] ",
             "Checking the type of the 'instance' variable: ",
             class(instance))
      }

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

        warning("[StopWordPipe][pipe][Warning] ", message)


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

        warning("[StopWordPipe][pipe][Warning] ", message)

        return(instance)
      }

      if (is.na(instance$getData()) ||
          all(instance$getData() == "") ||
          is.null(instance$getData())) {

        message <- c( "The file: " , instance$getPath() , " has data empty on pipe StopWord")

        instance$addProperties(message, "reasonToInvalidate")

        warning("[StopWordPipe][pipe][Warning] ", message)

        instance$invalidate()

        return(instance)
      }

      return(instance)
    },

    findStopWord = function(data, stopWord) {

      if (!"character" %in% class(data)) {
        stop("[StopWordPipe][findStopWord][Error] ",
             "Checking the type of the 'data' variable: ",
             class(data))
      }

      if (!"character" %in% class(stopWord)) {
        stop("[StopWordPipe][findStopWord][Error] ",
             "Checking the type of the 'stopWord' variable: ",
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
        stop("[StopWordPipe][removeStopWord][Error] ",
             "Checking the type of the 'stopWord' variable: ",
             class(stopWord))
      }

      if (!"character" %in% class(data)) {
        stop("[StopWordPipe][removeStopWord][Error] ",
             "Checking the type of the 'data' variable: ",
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
        stop("[StopWordPipe][setResourcesStopWordsPath][Error] ",
             "Checking the type of the 'path' variable: ",
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
