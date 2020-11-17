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
#' @import R6
#' @export StopWordPipe

StopWordPipe <- R6Class(

  "StopWordPipe",

  inherit = GenericPipe,

  public = list(
    #'
    #' @description Creates a \code{\link{StopWordPipe}} object.
    #'
    #' @param propertyName A \code{\link{character}} value. Name of the property
    #' associated with the \code{\link{GenericPipe}}.
    #' @param propertyLanguageName A \code{\link{character}} value. Name of the
    #' language property.
    #' @param alwaysBeforeDeps A \code{\link{list}} value. The dependencies
    #' alwaysBefore (\code{\link{GenericPipe}s} that must be executed before
    #' this one).
    #' @param notAfterDeps A \code{\link{list}} value. The dependencies
    #' notAfter (\code{\link{GenericPipe}s} that cannot be executed after
    #' this one).
    #' @param removeStopWords A \code{\link{logical}} value. Indicates if
    #' the stop words are removed or not.
    #' @param resourcesStopWordsPath A \code{\link{character}} value. Path
    #' of resource files (in json format) containing the stop words.
    #'
    initialize = function(propertyName = "stopWord",
                          propertyLanguageName = "language",
                          alwaysBeforeDeps = list("GuessLanguagePipe"),
                          notAfterDeps = list("AbbreviationPipe"),
                          removeStopWords = TRUE,
                          resourcesStopWordsPath = NULL) {

      if (!"character" %in% class(propertyName)) {
        bdpar.log(message = paste0("Checking the type of the 'propertyName' variable: ",
                                   class(propertyName)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      if (!"character" %in% class(propertyLanguageName)) {
        bdpar.log(message = paste0("Checking the type of the 'propertyLanguageName' variable: ",
                                   class(propertyLanguageName)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        bdpar.log(message = paste0("Checking the type of the 'alwaysBeforeDeps' variable: ",
                                   class(alwaysBeforeDeps)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }
      if (!"list" %in% class(notAfterDeps)) {
        bdpar.log(message = paste0("Checking the type of the 'notAfterDeps' variable: ",
                                   class(notAfterDeps)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      if (!"logical" %in% class(removeStopWords)) {
        bdpar.log(message = paste0("Checking the type of the 'removeStopWords' variable: ",
                                   class(removeStopWords)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)

      private$propertyLanguageName <- propertyLanguageName

      if (is.null(resourcesStopWordsPath)) {
        if (!all(bdpar.Options$isSpecificOption("resources.stopwords.path"),
                 !is.null(bdpar.Options$get("resources.stopwords.path")))) {
          bdpar.log(message = paste0("Path of stop words resources is neither ",
                                     "defined in initialize or in bdpar.Options"),
                    level = "FATAL",
                    className = class(self)[1],
                    methodName = "initialize")

        } else {
          resourcesStopWordsPath <- bdpar.Options$get("resources.stopwords.path")
        }
      }

      if (!"character" %in% class(resourcesStopWordsPath)) {
        bdpar.log(message = paste0("Checking the type of the 'resourcesStopWordsPath' variable: ",
                                   class(resourcesStopWordsPath)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      private$resourcesStopWordsPath <- resourcesStopWordsPath

      private$removeStopWords <- removeStopWords
    },
    #'
    #' @description Preprocesses the \code{\link{Instance}} to obtain/remove
    #' the stop words. The stop words found in the data are added to the
    #' list of properties of the \code{\link{Instance}}.
    #'
    #' @param instance A \code{\link{Instance}} value. The \code{\link{Instance}}
    #' to preprocess.
    #'
    #' @return The \code{\link{Instance}} with the modifications that have
    #' occurred in the pipe.
    #'
    #' @import rlist
    #'
    pipe = function(instance) {

      if (!"Instance" %in% class(instance)) {
        bdpar.log(message = paste0("Checking the type of the 'instance' variable: ",
                                   class(instance)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "pipe")
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

        bdpar.log(message = message,
                  level = "WARN",
                  className = class(self)[1],
                  methodName = "pipe")

        instance
      }

      JsonFile <- paste(self$getResourcesStopWordsPath(),
                        "/",
                        languageInstance,
                        ".json",
                        sep = "")

      jsonData <- Bdpar[["private_methods"]][["resourceHandler"]]()$isLoadResource(JsonFile)

      if (!is.null(jsonData)) {

        #Variable which stores the stopwords located in the data
        stopWordLocated <- list()

        for (stopWord in jsonData) {

          if (self$findStopWord(instance$getData(), stopWord)) {
            stopWordLocated <- list.append(stopWordLocated, stopWord)
          }

          if (private$removeStopWords && stopWord %in% stopWordLocated) {
            instance$setData(trimws(x = self$removeStopWord(stopWord,
                                                            instance$getData())))
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

        bdpar.log(message = message,
                  level = "WARN",
                  className = class(self)[1],
                  methodName = "pipe")

        return(instance)
      }

      if (is.na(instance$getData()) ||
          all(instance$getData() == "") ||
          is.null(instance$getData())) {

        message <- c("The file: ", instance$getPath(), " has data empty on pipe StopWord")

        instance$addProperties(message, "reasonToInvalidate")

        bdpar.log(message = message,
                  level = "WARN",
                  className = class(self)[1],
                  methodName = "pipe")

        instance$invalidate()

        return(instance)
      }

      instance
    },
    #'
    #' @description Checks if the stop word is in the data.
    #'
    #' @param data A \code{\link{character}} value. The text where stop word
    #' will be searched.
    #' @param stopWord A \code{\link{character}} value. Indicates the
    #' stop word to find.
    #'
    #' @return A \code{\link{logical}} value depending on whether the
    #' stop word is in the data.
    #'
    findStopWord = function(data, stopWord) {

      if (!"character" %in% class(data)) {
        bdpar.log(message = paste0("Checking the type of the 'data' variable: ",
                                   class(data)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "findStopWord")
      }

      if (!"character" %in% class(stopWord)) {
        bdpar.log(message = paste0("Checking the type of the 'stopWord' variable: ",
                                   class(stopWord)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "findStopWord")
      }

      stopWordEscaped <- rex::escape(stopWord)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><\u00A1?\u00BF!;:,.'-]|^)(" ,
                                 stopWordEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      grepl(pattern = rex::regex(regularExpresion),
            x = data ,
            perl = TRUE)
    },
    #'
    #' @description Removes the \emph{stop word} in the data.
    #'
    #' @param stopWord A \code{\link{character}} value. Indicates the
    #' stop word to remove.
    #' @param data A \code{\link{character}} value. The text where stop word
    #' will be removed.
    #'
    #' @return The data with the stop words removed.
    #'
    removeStopWord = function(stopWord, data) {

      if (!"character" %in% class(stopWord)) {
        bdpar.log(message = paste0("Checking the type of the 'stopWord' variable: ",
                                   class(stopWord)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "removeStopWord")
      }

      if (!"character" %in% class(data)) {
        bdpar.log(message = paste0("Checking the type of the 'data' variable: ",
                                   class(data)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "removeStopWord")
      }

      stopWordEscaped <- rex::escape(stopWord)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><\u00A1?\u00BF!;:,.'-]|^)(" ,
                                 stopWordEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      gsub(rex::regex(regularExpresion),
           "",
           data,
           perl = TRUE)
    },
    #'
    #' @description Gets the name of property language.
    #'
    #' @return Value of name of property language.
    #'
    getPropertyLanguageName = function() {
      private$propertyLanguageName
    },
    #'
    #' @description Gets the path of stop words resources.
    #'
    #' @return Value of path of stop words resources.
    #'
    getResourcesStopWordsPath = function() {
      private$resourcesStopWordsPath
    },
    #'
    #' @description Sets the path of stop words resources.
    #'
    #' @param path A \code{\link{character}} value. The new value of the path of
    #' stop words resources.
    #'
    setResourcesStopWordsPath = function(path) {

      if (!"character" %in% class(path)) {
        bdpar.log(message = paste0("Checking the type of the 'path' variable: ",
                                   class(path)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "setResourcesStopWordsPath")
      }

      private$resourcesStopWordsPath <- path
    }
  ),

  private = list(
    # A (\emph{character}) value. The name of property about language.
    propertyLanguageName = "",
    # A (\emph{character}) value. Path of resource files (in json format)
    # containing the stop words.
    resourcesStopWordsPath = "",
    # A (\emph{logical}) value. Indicates if the stop words are removed or
    # not.
    removeStopWords = TRUE
  )
)
