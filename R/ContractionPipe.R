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

#' @title Class to find and/or replace the contractions on the data field of a Instance
#'
#' @description \code{\link{ContractionPipe}} class is responsible for detecting
#' the existing contractions in the \strong{data} field of each \code{\link{Instance}}.
#' Identified contractions are stored inside the \strong{contraction} field of
#' \code{\link{Instance}} class. Moreover if needed, is able to perform inline
#' contractions replacement.
#'
#' @section Details:
#' \code{\link{ContractionPipe}} class requires the resource files (in json format)
#' containing the correspondence between contractions and meaning. To this end,
#' the language of the text indicated in the \emph{propertyLanguageName} should
#' be contained in the resource file name (ie. contr.xxx.json where xxx is the
#' value defined in the \emph{propertyLanguageName} ). The location of the
#' resources should be defined in the \strong{"resources.contractions.path"}
#' field of \emph{\link{bdpar.Options}} variable.
#'
#' @section Note:
#' \code{\link{ContractionPipe}} will automatically invalidate the
#' \code{\link{Instance}} whenever the obtained data is empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{GenericPipe}} and implements the
#' \code{pipe} abstract function.
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{bdpar.Options}},
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
#' @export ContractionPipe

ContractionPipe <- R6Class(

  "ContractionPipe",

  inherit = GenericPipe,

  public = list(
    #'
    #' @description Creates a \code{\link{ContractionPipe}} object.
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
    #' @param replaceContractions A \code{\link{logical}} value. Indicates if
    #' the contractions are replaced or not.
    #' @param resourcesContractionsPath A \code{\link{character}} value. Path
    #' of resource files (in json format) containing the correspondence between
    #' contractions and meaning.
    #'
    initialize = function(propertyName = "contractions",
                          propertyLanguageName = "language",
                          alwaysBeforeDeps = list("GuessLanguagePipe"),
                          notAfterDeps = list(),
                          replaceContractions = TRUE,
                          resourcesContractionsPath = NULL) {

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

      if (!"logical" %in% class(replaceContractions)) {
        bdpar.log(message = paste0("Checking the type of the 'replaceContractions' variable: ",
                                   class(replaceContractions)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)

      private$propertyLanguageName <- propertyLanguageName

      if (is.null(resourcesContractionsPath)) {
        if (any(!bdpar.Options$isSpecificOption("resources.contractions.path"),
                is.null(bdpar.Options$get("resources.contractions.path")))) {
          bdpar.log(message = paste0("Path of contractions resources is neither ",
                                     "defined in initialize or in bdpar.Options"),
                    level = "FATAL",
                    className = class(self)[1],
                    methodName = "initialize")
        } else {
          resourcesContractionsPath <- bdpar.Options$get("resources.contractions.path")
        }
      }

      if (!"character" %in% class(resourcesContractionsPath)) {
        bdpar.log(message = paste0("Checking the type of the 'resourcesContractionsPath' variable: ",
                                   class(resourcesContractionsPath)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      private$resourcesContractionsPath <- resourcesContractionsPath
      private$replaceContractions <- replaceContractions
    },
    #'
    #' @description Preprocesses the \code{\link{Instance}} to obtain/replace
    #' the contractions. The contractions found in the data are added to the
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

      languageInstance <- instance$getSpecificProperty( self$getPropertyLanguageName())

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

      JsonFile <- paste(self$getResourcesContractionsPath(),
                        "/contr.",
                        languageInstance,
                        ".json",
                        sep = "")

      jsonData <- Bdpar[["private_fields"]][["resourceHandler"]]$isLoadResource(JsonFile)

      #It is verified that there is a resource associated to the language of the instance
      if (!is.null(jsonData)) {

        #Variable which stores the contractions located in the data
        contractionsLocated <- list()

        for (contraction in names(jsonData)) {

          if (self$findContraction(instance$getData(), contraction)) {

            contractionsLocated <- list.append(contractionsLocated,
                                               contraction)
          }

          if (private$replaceContractions &&
              contraction %in% contractionsLocated) {
            instance$setData(
              textutils::trim(
                self$replaceContraction(contraction,
                                        as.character(jsonData[contraction]),
                                        instance$getData())))
          }
        }

        instance$addProperties(paste(contractionsLocated),
                               super$getPropertyName())

      } else {

        instance$addProperties(list(),super$getPropertyName())

        bdpar.log(message = paste0("The file: ", instance$getPath(),
                                   " has not an contractionsJsonFile ",
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
                     " has data empty on pipe Contractions")

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
    #' @description Checks if the contraction is in the data.
    #'
    #' @param data A \code{\link{character}} value. The text where contraction
    #' will be searched.
    #' @param contraction A \code{\link{character}} value. Indicates the
    #' contraction to find.
    #'
    #' @return A \code{\link{logical}} value depending on whether the
    #' contraction is in the data.
    #'
    findContraction = function(data, contraction) {

      if (!"character" %in% class(data)) {
        bdpar.log(message = paste0("Checking the type of the 'data' variable: ",
                                   class(data)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "findContraction")
      }

      if (!"character" %in% class(contraction)) {
        bdpar.log(message = paste0("Checking the type of the 'contraction' variable: ",
                                   class(contraction)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "findContraction")
      }

      contractionEscaped <- rex::escape(contraction)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><\u00A1?\u00BF!;:,.'-]|^)(",
                                 contractionEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      grepl(pattern = rex::regex(regularExpresion),
            x = data,
            perl = TRUE,
            ignore.case = TRUE)
    },
    #'
    #' @description Replaces the \emph{contraction} in the data for the
    #' \emph{extendedContraction}.
    #'
    #' @param contraction A \code{\link{character}} value. Indicates the
    #' contraction to replace.
    #' @param extendedContraction A \code{\link{character}} value. Indicates the
    #' string to replace for the contractions found.
    #' @param data A \code{\link{character}} value. The text where contraction
    #' will be replaced.
    #'
    #' @return The data with the contractions replaced.
    #'
    replaceContraction = function(contraction, extendedContraction, data) {

      if (!"character" %in% class(contraction)) {
        bdpar.log(message = paste0("Checking the type of the 'contraction' variable: ",
                                   class(contraction)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "replaceContraction")
      }

      if (!"character" %in% class(extendedContraction)) {
        bdpar.log(message = paste0("Checking the type of the 'extendedContraction' variable: ",
                                   class(extendedContraction)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "replaceContraction")
      }

      if (!"character" %in% class(data)) {
        bdpar.log(message = paste0("Checking the type of the 'data' variable: ",
                                   class(data)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "replaceContraction")
      }

      contractionEscaped <- rex::escape(contraction)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><\u00A1?\u00BF!;:,.'-]|^)(",
                                 contractionEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      gsub(rex::regex(regularExpresion),
           paste(" ", extendedContraction, " ", sep = ""),
           data,
           perl = TRUE,
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
    #' @description Gets the path of contractions resources.
    #'
    #' @return Value of path of contractions resources.
    #'
    getResourcesContractionsPath = function() {
      private$resourcesContractionsPath
    },
    #'
    #' @description Sets the path of contractions resources.
    #'
    #' @param path A \code{\link{character}} value. The new value of the path of
    #' contractions resources.
    #'
    setResourcesContractionsPath = function(path) {

      if (!"character" %in% class(path)) {
        bdpar.log(message = paste0("Checking the type of the 'path' variable: ",
                                   class(path)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "setResourcesContractionsPath")
      }

      private$resourcesContractionsPath <- path
    }
  ),

  private = list(
    # A (\emph{character}) value. The name of property about language.
    propertyLanguageName = "",
    # A (\emph{character}) value. Path of resource files (in json format)
    # containing the correspondence between contractions and meaning.
    resourcesContractionsPath = "",
    # A (\emph{logical}) value. Indicates if the contractions are replaced or
    # not.
    replaceContractions = TRUE
  )
)
