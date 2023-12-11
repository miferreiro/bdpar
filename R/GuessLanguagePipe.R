#
# Bdpar provide a tool to easily build customized data flows to pre-process
# large volumes of information from different sources. To this end, bdpar allows
# to (i) easily use and create new functionalities and (ii) develop new data
# source extractors according to the user needs. Additionally, the package
# provides by default a predefined data flow to extract and preprocess the most
# relevant information (tokens, dates, ... ) from some textual sources (SMS,
# email, tweets, YouTube comments).
#
# Copyright (C) 2020-2022 Sing Group (University of Vigo)
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

#' @title Class to guess the language of an Instance
#'
#' @description This class allows guess the language by using language detector of library
#' cld2. Creates the \strong{language} property which indicates the idiom text.
#' Optionally, it is possible to choose the language provided by Twitter.
#'
#' @section Details:
#' To obtain the language of the tweets, it will be verified that there is a
#' json file with the information stored in memory. On the other hand, it is
#' necessary define the \strong{"cache.twitter.path"} field of
#' \emph{\link{bdpar.Options}} variable to know where the
#' information of tweets are saved.
#'
#' @section Note:
#' The Pipe will invalidate the \code{\link{Instance}} if the language of the data
#' can not be detect.
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
#'          \code{\link{Instance}}, \code{\link{InterjectionPipe}},
#'          \code{\link{MeasureLengthPipe}}, \code{\link{GenericPipe}},
#'          \code{\link{SlangPipe}}, \code{\link{StopWordPipe}},
#'          \code{\link{StoreFileExtPipe}}, \code{\link{TargetAssigningPipe}},
#'          \code{\link{TeeCSVPipe}}, \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export GuessLanguagePipe

GuessLanguagePipe <- R6Class(

  "GuessLanguagePipe",

  inherit = GenericPipe,

  public = list(
    #'
    #' @description Creates a \code{\link{GuessLanguagePipe}} object.
    #'
    #' @param propertyName A \code{\link{character}} value. Name of the property
    #' associated with the \code{\link{GenericPipe}}.
    #' @param alwaysBeforeDeps A \code{\link{list}} value. The dependencies
    #' alwaysBefore (\code{\link{GenericPipe}s} that must be executed before
    #' this one).
    #' @param notAfterDeps A \code{\link{list}} value. The dependencies
    #' notAfter (\code{\link{GenericPipe}s} that cannot be executed after
    #' this one).
    #' @param languageTwitter A \code{\link{logical}} value. Indicates whether
    #' for the \code{\link{Instance}s} of type twtid the language that
    #' returns the API is obtained or the detector is applied.
    #'
    initialize = function(propertyName = "language",
                          alwaysBeforeDeps = list("StoreFileExtPipe",
                                                  "TargetAssigningPipe"),
                          notAfterDeps = list(),
                          languageTwitter = TRUE) {

      if (!"character" %in% class(propertyName)) {
        bdpar.log(message = paste0("Checking the type of the 'propertyName' variable: ",
                                   class(propertyName)),
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

      if (!"logical" %in% class(languageTwitter)) {
        bdpar.log(message = paste0("Checking the type of the 'languageTwitter' variable: ",
                                   class(languageTwitter)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      private$languageTwitter <- languageTwitter
    },
    #'
    #' @description Preprocesses the \code{\link{Instance}} to obtain the
    #' language of the data.
    #'
    #' @param instance A \code{\link{Instance}} value. The \code{\link{Instance}}
    #' to preprocess.
    #'
    #' @return The \code{\link{Instance}} with the modifications that have
    #' occurred in the pipe.
    #'
    pipe = function(instance) {

      if (!"Instance" %in% class(instance)) {
        bdpar.log(message = paste0("Checking the type of the 'instance' variable: ",
                                   class(instance)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "pipe")
      }

      if (private$languageTwitter &&
          instance$getSpecificProperty("extension") %in% "twtid") {

        cachePath <- bdpar.Options$get("cache.twitter.path")
        if (!is.null(cachePath) && file.exists(paste0(cachePath, "/_",
                                                      instance$getSpecificProperty("target"),
                                                      "_/",
                                                      instance$getId(),
                                                      ".json"))) {

          path <- paste0(cachePath,"/_",
                         instance$getSpecificProperty("target"),
                         "_/", instance$getId(), ".json")

          dataFromJsonFile <- rjson::fromJSON(file = path)

          if (!is.na(dataFromJsonFile[["lang"]]) &&
                !is.null(dataFromJsonFile[["lang"]])
                  && dataFromJsonFile[["lang"]] != "") {

            langTwitter <- dataFromJsonFile[["lang"]]

            instance$addProperties(langTwitter,super$getPropertyName())

            if (is.null(instance$getSpecificProperty("language"))) {

              message <- paste0("The file: ", instance$getPath(), " has a NULL twitter language")

              instance$addProperties(message, "reasonToInvalidate")

              bdpar.log(message = message,
                        level = "WARN",
                        className = class(self)[1],
                        methodName = "pipe")

              instance$invalidate()

              return(instance)
            }

            return(instance)
          }
        }
      }

      instance$addProperties(self$getLanguage(instance$getData()),
                             super$getPropertyName())

      if (is.na(instance$getSpecificProperty("language"))) {
        message <- paste0("The file: ", instance$getPath(), " has a null language")

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
    #' @description Guesses the language of data.
    #'
    #' @param data A \code{\link{character}} value. The text to guess the
    #' language.
    #'
    #' @return The language guesser. Format: see ISO 639-3:2007.
    #'
    getLanguage = function(data) {

      if (!"character" %in% class(data)) {
        bdpar.log(message = paste0("Checking the type of the 'data' variable: ",
                                   class(data)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "getLanguage")
      }

      cld2::detect_language(data, plain_text = TRUE)
    }
  ),

  private = list(
    # A (\emph{logical}) value. Indicates whether for the Instances of type
    # twtid the language that returns the api is obtained or the detector is
    # applied.
    languageTwitter = FALSE
  )
)
