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

#' @title Class to find and/or remove the interjections on the data field of an Instance
#'
#' @description \code{\link{InterjectionPipe}} class is responsible for detecting
#' the existing interjections in the \strong{data} field of each \code{\link{Instance}}.
#' Identified interjections are stored inside the \strong{interjection} field of
#' \code{\link{Instance}} class. Moreover if needed, is able to perform inline
#' interjections removement.
#'
#' @section Details:
#' \code{\link{InterjectionPipe}} class requires the resource files (in json format)
#' containing the list of interjections. To this end, the language of the text
#' indicated in the \emph{propertyLanguageName} should be contained in the
#' resource file name (ie. interj.xxx.json where xxx is the value defined in the
#' \emph{propertyLanguageName} ). The location of the resources should be
#' defined in the \strong{"resources.interjections.path"} field of
#' \emph{\link{bdpar.Options}} variable.
#'
#' @section Note:
#' \code{\link{InterjectionPipe}} will automatically invalidate the
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
#'          \code{\link{MeasureLengthPipe}}, \code{\link{GenericPipe}},
#'          \code{\link{ResourceHandler}}, \code{\link{SlangPipe}},
#'          \code{\link{StopWordPipe}}, \code{\link{StoreFileExtPipe}},
#'          \code{\link{TargetAssigningPipe}}, \code{\link{TeeCSVPipe}},
#'          \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export InterjectionPipe

InterjectionPipe <- R6Class(

  "InterjectionPipe",

  inherit = GenericPipe,

  public = list(
    #'
    #' @description Creates a \code{\link{InterjectionPipe}} object.
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
    #' @param removeInterjections A \code{\link{logical}} value. Indicates if
    #' the interjections are removed or not.
    #' @param resourcesInterjectionsPath A \code{\link{character}} value. Path
    #' of resource files (in json format) containing the interjections.
    #'
    initialize = function(propertyName = "interjection",
                          propertyLanguageName = "language",
                          alwaysBeforeDeps = list("GuessLanguagePipe"),
                          notAfterDeps = list(),
                          removeInterjections = TRUE,
                          resourcesInterjectionsPath = NULL) {

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

      if (!"logical" %in% class(removeInterjections)) {
        bdpar.log(message = paste0("Checking the type of the 'removeInterjections' variable: ",
                                   class(removeInterjections)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)

      private$propertyLanguageName <- propertyLanguageName

      if (is.null(resourcesInterjectionsPath)) {
        if (any(!bdpar.Options$isSpecificOption("resources.interjections.path"),
                is.null(bdpar.Options$get("resources.interjections.path")))) {
          bdpar.log(message = paste0("Path of interjections resources is ",
                                     "neither defined in initialize or in ",
                                     "bdpar.Options"),
                    level = "FATAL",
                    className = class(self)[1],
                    methodName = "initialize")
        } else {
          resourcesInterjectionsPath <- bdpar.Options$get("resources.interjections.path")
        }
      }

      if (!"character" %in% class(resourcesInterjectionsPath)) {
        bdpar.log(message = paste0("Checking the type of the 'resourcesInterjectionsPath' variable: ",
                                   class(resourcesInterjectionsPath)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      private$resourcesInterjectionsPath <- resourcesInterjectionsPath

      private$removeInterjections <- removeInterjections
    },
    #'
    #' @description Preprocesses the \code{\link{Instance}} to obtain/remove
    #' the interjections. The interjections found in the data are added to the
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

        message <- c("The file: ", instance$getPath(), " has not language property")

        bdpar.log(message = message,
                  level = "WARN",
                  className = class(self)[1],
                  methodName = "pipe")

        return(instance)
      }

      JsonFile <- paste(self$getResourcesInterjectionsPath(),
                        "/interj.",
                        languageInstance,
                        ".json",
                        sep = "")

      jsonData <- Bdpar[["private_fields"]][["resourceHandler"]]$isLoadResource(JsonFile)

      if (!is.null(jsonData)) {

        #Variable which stores the interjections located in the data
        interjectionsLocated <- list()

        for (interjection in jsonData) {

          if (self$findInterjection(instance$getData(), interjection)) {
            interjectionsLocated <- list.append(interjectionsLocated, interjection)
          }

          if (private$removeInterjections &&
              interjection %in% interjectionsLocated) {
            instance$setData(trimws(x = self$removeInterjection(interjection,
                                                                instance$getData())))
          }
        }

        instance$addProperties(paste(interjectionsLocated), super$getPropertyName())

      } else {

        instance$addProperties(list(), super$getPropertyName())

        bdpar.log(message = paste0("The file: ", instance$getPath(),
                                   " has not an interjectionsJsonFile ",
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
                     " has data empty on pipe Interjection")

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
    #' @description Checks if the interjection is in the data.
    #'
    #' @param data A \code{\link{character}} value. The text where interjection
    #' will be searched.
    #' @param interjection A \code{\link{character}} value. Indicates the
    #' interjection to find.
    #'
    #' @return A \code{\link{logical}} value depending on whether the
    #' interjection is in the data.
    #'
    findInterjection = function(data, interjection) {

      if (!"character" %in% class(data)) {
        bdpar.log(message = paste0("Checking the type of the 'data' variable: ",
                                   class(data)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "findInterjection")
      }

      if (!"character" %in% class(interjection)) {
        bdpar.log(message = paste0("Checking the type of the 'interjection' variable: ",
                                   class(interjection)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "findInterjection")
      }

      interjectionEscaped <- rex::escape(interjection)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><\u00A1?\u00BF!;:,.'-]|^)([\u00A1]*(",
                                 interjectionEscaped,
                                 ")[!]*)[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      grepl(pattern = rex::regex(regularExpresion),
            x = data,
            perl = TRUE)
    },
    #'
    #' @description Removes the \emph{interjection} in the data.
    #'
    #' @param interjection A \code{\link{character}} value. Indicates the
    #' interjection to remove.
    #' @param data A \code{\link{character}} value. The text where interjection
    #' will be removed.
    #'
    #' @return The data with the interjections removed.
    #'
    removeInterjection = function(interjection, data) {

      if (!"character" %in% class(interjection)) {
        bdpar.log(message = paste0("Checking the type of the 'interjection' variable: ",
                                   class(interjection)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "removeInterjection")
      }


      if (!"character" %in% class(data)) {
        bdpar.log(message = paste0("Checking the type of the 'data' variable: ",
                                   class(data)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "removeInterjection")
      }

      interjectionEscaped <- rex::escape(interjection)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><\u00A1?\u00BF!;:,.'-]|^)([\u00A1]*(",
                                 interjectionEscaped,
                                 ")[!]*)[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      gsub(rex::regex(regularExpresion),
           "",
           data ,
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
    #' @description Gets the path of interjections resources.
    #'
    #' @return Value of path of interjections resources.
    #'
    getResourcesInterjectionsPath = function() {
      private$resourcesInterjectionsPath
    },
    #'
    #' @description Sets the path of interjections resources.
    #'
    #' @param path A \code{\link{character}} value. The new value of the path of
    #' interjections resources.
    #'
    setResourcesInterjectionsPath = function(path) {

      if (!"character" %in% class(path)) {
        bdpar.log(message = paste0("Checking the type of the 'path' variable: ",
                                   class(path)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "setResourcesInterjectionsPath")
      }

      private$resourcesInterjectionsPath <- path
    }
  ),

  private = list(
    # A (\emph{character}) value. The name of property about language.
    propertyLanguageName = "",
    # A (\emph{character}) value. Path of resource files (in json format)
    # containing the interjections.
    resourcesInterjectionsPath = "",
    # A (\emph{logical}) value. Indicates if the interjections are removed or
    # not.
    removeInterjections = TRUE
  )
)
