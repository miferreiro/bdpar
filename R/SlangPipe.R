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


#' @title Class to find and/or replace the slangs on the data field of an Instance
#'
#' @description \code{\link{SlangPipe}} class is responsible for detecting
#' the existing slangs in the \strong{data} field of each \code{\link{Instance}}.
#' Identified slangs are stored inside the \strong{slang} field of
#' \code{\link{Instance}} class. Moreover if needed, is able to perform inline
#' slangs replacement.
#'
#' @section Details:
#' \code{\link{SlangPipe}} class requires the resource files (in json format)
#' containing the correspondence between slangs and meaning. To this end,
#' the language of the text indicated in the \emph{propertyLanguageName} should
#' be contained in the resource file name (ie. slang.xxx.json where xxx is the
#' value defined in the \emph{propertyLanguageName} ). The location of the
#' resources should be defined in the \strong{"resources.slangs.path"} field of
#' \emph{\link{bdpar.Options}} variable.
#'
#' @section Note:
#' \code{\link{SlangPipe}} will automatically invalidate the
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
#'          \code{\link{StopWordPipe}}, \code{\link{StoreFileExtPipe}},
#'          \code{\link{TargetAssigningPipe}}, \code{\link{TeeCSVPipe}},
#'          \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export SlangPipe

SlangPipe <- R6Class(

  "SlangPipe",

  inherit = GenericPipe,

  public = list(
    #'
    #' @description Creates a \code{\link{SlangPipe}} object.
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
    #' @param replaceSlangs A \code{\link{logical}} value. Indicates if
    #' the slangs are replaced or not.
    #' @param resourcesSlangsPath A \code{\link{character}} value. Path
    #' of resource files (in json format) containing the correspondence between
    #' slangs and meaning.
    #'
    initialize = function(propertyName = "langpropname",
                          propertyLanguageName = "language",
                          alwaysBeforeDeps = list("GuessLanguagePipe"),
                          notAfterDeps = list(),
                          replaceSlangs = TRUE,
                          resourcesSlangsPath = NULL) {

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

      if (!"logical" %in% class(replaceSlangs)) {
        bdpar.log(message = paste0("Checking the type of the 'replaceSlangs' variable: ",
                                   class(replaceSlangs)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)

      private$propertyLanguageName <- propertyLanguageName

      if (is.null(resourcesSlangsPath)) {
        if (!bdpar.Options$isSpecificOption("resources.slangs.path") ||
            is.null(bdpar.Options$get("resources.slangs.path"))) {
          bdpar.log(message = paste0("Path of slangs resources is neither ",
                                     "defined in initialize or in bdpar.Options"),
                    level = "FATAL",
                    className = class(self)[1],
                    methodName = "initialize")
        } else {
          resourcesSlangsPath <- bdpar.Options$get("resources.slangs.path")
        }
      }

      if (!"character" %in% class(resourcesSlangsPath)) {
        bdpar.log(message = paste0("Checking the type of the 'resourcesSlangsPath' variable: ",
                                   class(resourcesSlangsPath)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      private$resourcesSlangsPath <- resourcesSlangsPath
      private$replaceSlangs <- replaceSlangs
    },
    #'
    #' @description Preprocesses the \code{\link{Instance}} to obtain/replace
    #' the slangs. The slangs found in the data are added to the
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

      #It is verified that there is a resource associated to the language of the instance
      if (is.null(languageInstance) ||
            is.na(languageInstance) ||
              "Unknown" %in% languageInstance) {

        instance$addProperties(list(), super$getPropertyName())

        message <- paste0("The file: ", instance$getPath(), " has not language property")

        bdpar.log(message = message,
                  level = "WARN",
                  className = class(self)[1],
                  methodName = "pipe")

        return(instance)
      }

      JsonFile <- paste0(self$getResourcesSlangsPath(),
                         "/slang.",
                         languageInstance,
                         ".json")

      jsonData <- Bdpar[["private_methods"]][["resourceHandler"]]()$isLoadResource(JsonFile)

      if (!is.null(jsonData)) {

        #Variable which stores the Slangs located in the data
        slangsLocated <- list()

        for (slang in names(jsonData)) {

          if (self$findSlang(instance$getData(), slang)) {
            slangsLocated <- list.append(slangsLocated, slang)
          }

          if (private$replaceSlangs && slang %in% slangsLocated) {
            instance$setData(trimws(x = self$replaceSlang(slang,
                                                          as.character(jsonData[slang]),
                                                          instance$getData())))
          }
        }

        instance$addProperties(paste(slangsLocated), super$getPropertyName())

      } else {

        instance$addProperties(list(), super$getPropertyName())

        message <- paste0("The file: ", instance$getPath(),
                          " has not an SlangsJsonFile to apply to the language-> ", languageInstance )

        bdpar.log(message = message,
                  level = "WARN",
                  className = class(self)[1],
                  methodName = "pipe")

        return(instance)
      }

      if (is.na(instance$getData()) ||
          all(instance$getData() == "") ||
          is.null(instance$getData())) {
        message <- paste0("The file: ", instance$getPath(), " has data empty on pipe Slang")

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
    #' @description Checks if the slang is in the data.
    #'
    #' @param data A \code{\link{character}} value. The text where slang
    #' will be searched.
    #' @param slang A \code{\link{character}} value. Indicates the
    #' slang to find.
    #'
    #' @return A \code{\link{logical}} value depending on whether the
    #' slang is in the data.
    #'
    findSlang = function(data, slang) {

      if (!"character" %in% class(data)) {
        bdpar.log(message = paste0("Checking the type of the 'data' variable: ",
                                   class(data)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "findSlang")
      }

      if (!"character" %in% class(slang)) {
        bdpar.log(message = paste0("Checking the type of the 'slang' variable: ",
                                   class(slang)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "findSlang")
      }

      slangEscaped <- rex::escape(slang)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><\u00A1?\u00BF!;:,.']|^)(",
                                 slangEscaped,
                                 ")[;:?\"!,.'>]*(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      grepl(pattern = rex::regex(regularExpresion),
            x = data,
            perl = T,
            ignore.case = TRUE)
    },
    #'
    #' @description Replaces the \emph{slang} in the data for the
    #' \emph{extendedSlang}.
    #'
    #' @param slang A \code{\link{character}} value. Indicates the
    #' slang to replace.
    #' @param extendedSlang A \code{\link{character}} value. Indicates the
    #' string to replace for the slangs found.
    #' @param data A \code{\link{character}} value. The text where slang
    #' will be replaced.
    #'
    #' @return The data with the slangs replaced.
    #'
    replaceSlang = function(slang, extendedSlang, data) {

      if (!"character" %in% class(slang)) {
        bdpar.log(message = paste0("Checking the type of the 'slang' variable: ",
                                   class(slang)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "replaceSlang")
      }

      if (!"character" %in% class(extendedSlang)) {
        bdpar.log(message = paste0("Checking the type of the 'extendedSlang' variable: ",
                                   class(extendedSlang)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "replaceSlang")
      }

      if (!"character" %in% class(data)) {
        bdpar.log(message = paste0("Checking the type of the 'data' variable: ",
                                   class(data)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "replaceSlang")
      }

      slangEscaped <- rex::escape(slang)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><\u00A1?\u00BF!;:,.']|^)(",
                                 slangEscaped,
                                 ")[;:?\"!,.'>]*(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      gsub(rex::regex(regularExpresion),
           paste(" ", extendedSlang, " ", sep = ""),
           data,
           perl = T,
           ignore.case = TRUE)
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
    #' @description Gets the path of slangs resources.
    #'
    #' @return Value of path of slangs resources.
    #'
    getResourcesSlangsPath = function() {
      private$resourcesSlangsPath
    },
    #'
    #' @description Sets the path of slangs resources.
    #'
    #' @param path A \code{\link{character}} value. The new value of the path of
    #' slangs resources.
    #'
    setResourcesSlangsPath = function(path) {

      if (!"character" %in% class(path)) {
        bdpar.log(message = paste0("Checking the type of the 'path' variable: ",
                                   class(path)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "setResourcesSlangsPath")
      }

      private$resourcesSlangsPath <- path
    }
  ),

  private = list(
    # A (\emph{character}) value. The name of property about language.
    propertyLanguageName = "",
    # A (\emph{character}) value. Path of resource files (in json format)
    # containing the correspondence between slangs and meaning.
    resourcesSlangsPath = "",
    # A (\emph{logical}) value. Indicates if the slangs are replaced or
    # not.
    replaceSlangs = TRUE
  )
)
