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

#' @title Class to find and/or replace the abbreviations on the data field of an Instance
#'
#' @description \code{\link{AbbreviationPipe}} class is responsible for detecting
#' the existing abbreviations in the \strong{data} field of each \code{\link{Instance}}.
#' Identified abbreviations are stored inside the \strong{abbreviation} field of
#' \code{\link{Instance}} class. Moreover if needed, is able to perform inline
#' abbreviations replacement.
#'
#' @section Details:
#' \code{\link{AbbreviationPipe}} class requires the resource files (in json format)
#' containing the correspondence between abbreviations and meaning. To this end,
#' the language of the text indicated in the \emph{propertyLanguageName} should
#' be contained in the resource file name (ie. abbrev.xxx.json where xxx is the
#' value defined in the \emph{propertyLanguageName} ). The location of the
#' resources should be defined in the \strong{"resources.abbreviations.path"}
#' field of \emph{\link{bdpar.Options}} variable.
#'
#' @section Note:
#' \code{\link{AbbreviationPipe}} will automatically invalidate the
#' \code{\link{Instance}} whenever the obtained data is empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{GenericPipe}} and implements the
#' \code{pipe} abstract function.
#'
#' @seealso \code{\link{bdpar.Options}}, \code{\link{ContractionPipe}},
#'          \code{\link{File2Pipe}}, \code{\link{FindEmojiPipe}},
#'          \code{\link{FindEmoticonPipe}}, \code{\link{FindHashtagPipe}},
#'          \code{\link{FindUrlPipe}}, \code{\link{FindUserNamePipe}},
#'          \code{\link{GuessDatePipe}}, \code{\link{GuessLanguagePipe}},
#'          \code{\link{Instance}}, \code{\link{InterjectionPipe}},
#'          \code{\link{MeasureLengthPipe}}, \code{\link{GenericPipe}},
#'          \code{\link{ResourceHandler}}, \code{\link{SlangPipe}},
#'          \code{\link{StopWordPipe}}, \code{\link{StoreFileExtPipe}},
#'          \code{\link{TargetAssigningPipe}}, \code{\link{TeeCSVPipe}},
#'          \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export AbbreviationPipe

AbbreviationPipe <- R6Class(

  "AbbreviationPipe",

  inherit = GenericPipe,

  public = list(
    #'
    #' @description Creates a \code{\link{AbbreviationPipe}} object.
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
    #' @param replaceAbbreviations A \code{\link{logical}} value. Indicates if
    #' the abbreviations are replaced or not.
    #' @param resourcesAbbreviationsPath A \code{\link{character}} value. Path
    #' of resource files (in json format) containing the correspondence between
    #' abbreviations and meaning.
    #'
    initialize = function(propertyName = "abbreviation",
                          propertyLanguageName = "language",
                          alwaysBeforeDeps = list("GuessLanguagePipe"),
                          notAfterDeps = list(),
                          replaceAbbreviations = TRUE,
                          resourcesAbbreviationsPath = NULL) {

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

      if (!"logical" %in% class(replaceAbbreviations)) {
        bdpar.log(message = paste0("Checking the type of the 'replaceAbbreviations' variable: ",
                                   class(replaceAbbreviations)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)

      private$propertyLanguageName <- propertyLanguageName

      if (is.null(resourcesAbbreviationsPath)) {
        if (any(!bdpar.Options$isSpecificOption("resources.abbreviations.path"),
                is.null(bdpar.Options$get("resources.abbreviations.path")))) {
          bdpar.log(message = paste0("Path of abbreviations resources is ",
                                     "neither defined in initialize or in ",
                                     "bdpar.Options"),
                    level = "FATAL",
                    className = class(self)[1],
                    methodName = "initialize")
        } else {
          resourcesAbbreviationsPath <- bdpar.Options$get("resources.abbreviations.path")
        }
      }

      if (!"character" %in% class(resourcesAbbreviationsPath)) {
        bdpar.log(message = paste0("Checking the type of the 'resourcesAbbreviationsPath' variable: ",
                                   class(resourcesAbbreviationsPath)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      private$resourcesAbbreviationsPath <- resourcesAbbreviationsPath
      private$replaceAbbreviations <- replaceAbbreviations
    },
    #'
    #' @description Preprocesses the \code{\link{Instance}} to obtain/replace
    #' the abbreviations. The abbreviations found in the data are added to the
    #' list of properties of the \code{\link{Instance}}.
    #'
    #' @param instance A \code{\link{Instance}} value. The \code{\link{Instance}}
    #' to preprocess.
    #'
    #' @return The \code{\link{Instance}} with the modifications that have
    #' occurred in the pipe.
    #'
    #' @import pipeR rlist
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

        instance$addProperties(list(),super$getPropertyName())

        bdpar.log(message = paste0("The file: ", instance$getPath(),
                                   " has not language property"),
                  level = "WARN",
                  className = class(self)[1],
                  methodName = "pipe")

        return(instance)
      }

      JsonFile <- paste(self$getResourcesAbbreviationsPath(),
                        "/abbrev.",
                        languageInstance,
                        ".json",
                        sep = "")

      jsonData <- Bdpar[["private_fields"]][["resourceHandler"]]$isLoadResource(JsonFile)

      #It is verified that there is a resource associated to the language of the instance
      if (!is.null(jsonData)) {

        #Variable which stores the abbreviations located in the data
        abbreviationsLocated <- list()

        for (abbreviation in names(jsonData)) {

          if (self$findAbbreviation(instance$getData(), abbreviation)) {
            abbreviationsLocated <- list.append(abbreviationsLocated,
                                                  abbreviation)
          }

          if (private$replaceAbbreviations &&
              abbreviation %in% abbreviationsLocated) {

              instance$getData() %>>%
                {self$replaceAbbreviation(abbreviation,
                                            as.character(jsonData[abbreviation]),
                                              .)} %>>%
                textutils::trim() %>>%
                    instance$setData()
          }
        }

        instance$addProperties(paste(abbreviationsLocated), super$getPropertyName())

      } else {

        instance$addProperties(list(), super$getPropertyName())

        bdpar.log(message = paste0("The file: ", instance$getPath(),
                                   " has not an abbreviationsJsonFile ",
                                   "to apply to the language ->",
                                   languageInstance),
                  level = "WARN",
                  className = class(self)[1],
                  methodName = "pipe")

        return(instance)
      }


      if (is.na(instance$getData()) ||
          all(instance$getData() == "") ||
          is.null(instance$getData())) {
        message <- c("The file: ", instance$getPath(),
                     " has data empty on pipe Abbreviation")
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
    #' @description Checks if the abbreviation is in the data.
    #'
    #' @param data A \code{\link{character}} value. The text where abbreviation
    #' will be searched.
    #' @param abbreviation A \code{\link{character}} value. Indicates the
    #' abbreviation to find.
    #'
    #' @return A \code{\link{logical}} value depending on whether the
    #' abbreviation is in the data.
    #'
    findAbbreviation = function(data, abbreviation) {

      if (!"character" %in% class(data)) {
        bdpar.log(message = paste0("Checking the type of the 'data' variable: ",
                                   class(data)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "findAbbreviation")
      }

      if (!"character" %in% class(abbreviation)) {
        bdpar.log(message = paste0("Checking the type of the 'abbreviation' variable: ",
                                   class(abbreviation)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "findAbbreviation")
      }

      abbreviationEscaped <- rex::escape(abbreviation)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><\u00A1?\u00BF!;:,.'-]|^)(",
                                 abbreviationEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      grepl(pattern = rex::regex(regularExpresion),
            x = data,
            perl = TRUE)
    },
    #'
    #' @description Replaces the \emph{abbreviation} in the data for the
    #' \emph{extendedAbbreviation}.
    #'
    #' @param abbreviation A \code{\link{character}} value. Indicates the
    #' abbreviation to replace.
    #' @param extendedAbbreviation A \code{\link{character}} value. Indicates the
    #' string to replace for the abbreviations found.
    #' @param data A \code{\link{character}} value. The text where abbreviation
    #' will be replaced.
    #'
    #' @return The data with the abbreviations replaced.
    #'
    replaceAbbreviation = function(abbreviation, extendedAbbreviation, data) {

      if (!"character" %in% class(abbreviation)) {
        bdpar.log(message = paste0("Checking the type of the 'abbreviation' variable: ",
                                   class(abbreviation)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "replaceAbbreviation")
      }

      if (!"character" %in% class(extendedAbbreviation)) {
        bdpar.log(message = paste0("Checking the type of the 'extendedAbbreviation' variable: ",
                                   class(extendedAbbreviation)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "replaceAbbreviation")
      }

      if (!"character" %in% class(data)) {
        bdpar.log(message = paste0("Checking the type of the 'data' variable: ",
                                   class(data)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "replaceAbbreviation")
      }

      abbreviationEscaped <- rex::escape(abbreviation)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><\u00A1?\u00BF!;:,.'-]|^)(",
                                 abbreviationEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      gsub(rex::regex(regularExpresion),
           paste(" ", extendedAbbreviation, " ", sep = ""),
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
    #' @description Gets the path of abbreviations resources.
    #'
    #' @return Value of path of abbreviations resources.
    #'
    getResourcesAbbreviationsPath = function() {
      private$resourcesAbbreviationsPath
    },
    #'
    #' @description Sets the path of abbreviations resources.
    #'
    #' @param path A \code{\link{character}} value. The new value of the path of
    #' abbreviations resources.
    #'
    setResourcesAbbreviationsPath = function(path) {

      if (!"character" %in% class(path)) {
        bdpar.log(message = paste0("Checking the type of the 'path' variable: ",
                                   class(path)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "setResourcesAbbreviationsPath")
      }

      private$resourcesAbbreviationsPath <- path
    }
  ),

  private = list(
    # A (\emph{character}) value. The name of property about language.
    propertyLanguageName = "",
    # A (\emph{character}) value. Path of resource files (in json format)
    # containing the correspondence between abbreviations and meaning.
    resourcesAbbreviationsPath = "",
    # A (\emph{logical}) value. Indicates if the abbreviations are replaced or
    # not.
    replaceAbbreviations = TRUE
  )
)
