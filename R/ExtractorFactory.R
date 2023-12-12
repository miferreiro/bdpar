#
# Bdpar provide a tool to easily build customized data flows to pre-process
# large volumes of information from different sources. To this end, bdpar allows
# to (i) easily use and create new functionalities and (ii) develop new data
# source extractors according to the user needs. Additionally, the package
# provides by default a predefined data flow to extract and preprocess the most
# relevant information (tokens, dates, ... ) from some textual sources (SMS,
# email, YouTube comments).
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

#' @title Class to handle the creation of Instance types
#'
#' @description \code{\link{ExtractorFactory}} class builds the appropriate
#' \code{\link{Instance}} object according to the file extension. In the case
#' of not finding the registered extension, the default extractor will be used
#' if it has been previously configured.
#'
#' @seealso \code{\link{ExtractorEml}}, \code{\link{ExtractorSms}},
#' \code{\link{Instance}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export ExtractorFactory

ExtractorFactory <- R6Class(

  "ExtractorFactory",

  public = list(
    #'
    #' @description Creates a \code{\link{ExtractorFactory}} object.
    #'
    initialize = function() {
      if (Sys.which("python") == "" &&
          Sys.which("python3 ") == "") {
        bdpar.log(message = paste0("Remember install and configure Python for ",
                                   ".eml files processing"),
                  level = "ERROR",
                  className = class(self)[1],
                  methodName = "initialize")
      }
      private$extractors <- list("eml" = ExtractorEml,
                                 "tsms" = ExtractorSms,
                                 "ytbid" = ExtractorYtbid)
      private$defaultExtractor <- NULL
    },
    #'
    #' @description Adds an extractor to the list of extensions. If the extension
    #' is an empty string (""), the indicated extractor will be the default
    #' when there is no extractor associated with an extension.
    #'
    #' @param extensions A \code{\link{character}} array. The names of the
    #' extension option.
    #' @param extractor A \code{Object} value. The extractor of the new
    #' extension.
    #'
    #' @import rlist
    #'
    registerExtractor = function(extensions, extractor) {
      if (!"character" %in% class(extensions)) {
        bdpar.log(message = paste0("Checking the type of the 'extensions' variable: ",
                                   class(extensions)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "registerExtractor")
      }

      if (!"R6ClassGenerator" %in% class(extractor) || extractor$inherit != "Instance") {
        bdpar.log(message = paste0("Checking the type of the 'extractor' ",
                                   "variable: ", class(extractor)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "registerExtractor")
      }

      lapply(extensions, function(extension) {
        if (self$isSpecificExtractor(extension)) {
          bdpar.log(message = paste0("'", extension, "' extension is already added"),
                    level = "FATAL",
                    className = class(self)[1],
                    methodName = "registerExtractor")
        }
      })

      invisible(lapply(extensions, function(extension, extractor) {
        private$extractors <- list.append(private$extractors, extractor)
        names(private$extractors)[length(private$extractors)] <- extension
      }, extractor))
    },
    #'
    #' @description Modifies the extractor of the one extension.
    #'
    #' @param extension A \code{\link{character}} value. The name of the
    #' extension option.
    #' @param extractor A \code{Object} value. The value of the new
    #' extractor.
    #'
    setExtractor = function(extension, extractor) {
      if (!"character" %in% class(extension)) {
        bdpar.log(message = paste0("Checking the type of the 'extension' ",
                                   "variable: ", class(extension)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "setExtractor")
      }

      if (!self$isSpecificExtractor(extension)) {
        bdpar.log(message = paste0("'", extension,
                                   "' extension is not configured"),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "setExtractor")
      } else {
        if (!"R6ClassGenerator" %in% class(extractor) ||
            extractor$inherit != "Instance") {
          bdpar.log(message = paste0("Checking the type of the 'extractor' ",
                                     "variable: ", class(extractor)),
                    level = "FATAL",
                    className = class(self)[1],
                    methodName = "setExtractor")
        }
        private$extractors[[extension]] <- extractor
      }
    },
    #'
    #' @description Modifies the extractor of the one extension. Assign NULL
    #' value to disable the default extractor.
    #'
    #' @param defaultExtractor A \code{Object} value. The value of the default
    #' extractor.
    #'
    setDefaultExtractor = function(defaultExtractor) {

      if (!is.null(defaultExtractor)) {
        if (!"R6ClassGenerator" %in% class(defaultExtractor) ||
            is.null(defaultExtractor$inherit) ||
            defaultExtractor$inherit != "Instance") {
          bdpar.log(message = paste0("Checking the type of the 'defaultExtractor' ",
                                     "variable: ", class(defaultExtractor)),
                    level = "FATAL",
                    className = class(self)[1],
                    methodName = "setDefaultExtractor")
        }
      }
      private$defaultExtractor <- defaultExtractor
    },
    #'
    #' @description Removes a specific extractor thought the extension.
    #'
    #' @param extension A \code{\link{character}} value. The name of the
    #' extension to remove.
    #'
    #' @import rlist
    #'
    removeExtractor = function(extension) {
      if (!"character" %in% class(extension)) {
        bdpar.log(message = paste0("Checking the type of the 'extension' ",
                                   "variable: ", class(extension)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "removeExtractor")
      }

      if (!self$isSpecificExtractor(extension)) {
        bdpar.log(message = paste0("'", extension, "' extension is not configured"),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "removeExtractor")
      } else {
        private$extractors <- list.remove(private$extractors, extension)
      }
    },
    #'
    #' @description Gets the list of extractors.
    #'
    #' @return Value of extractors.
    #'
    getAllExtractors = function() {
      private$extractors
    },
    #'
    #' @description Gets the default extractor.
    #'
    #' @return Value of default extractor.
    #'
    getDefaultExtractor = function() {
      private$defaultExtractor
    },
    #'
    #' @description Checks if exists an extractor for a specific extension.
    #'
    #' @param extension A \code{\link{character}} value. The name of the
    #' extension to check
    #'
    #' @return Value of extractors.
    #'
    isSpecificExtractor = function(extension) {
      extension %in% names(private$extractors)
    },
    #'
    #' @description Builds the \code{\link{Instance}} object according to the
    #' file extension. In the case of not finding the registered extension, the
    #' default extractor will be used if it has been previously configured.
    #'
    #' @param path  A \code{\link{character}} value. Path of the file to create
    #' an \code{\link{Instance}}.
    #'
    #' @return The \code{\link{Instance}} corresponding object according to the
    #' file extension.
    #'
    #' @importFrom tools file_ext
    #'
    createInstance = function(path) {

      if (!"character" %in% class(path)) {
        bdpar.log(message = paste0("Checking the type of the 'path' variable: ",
                                   class(path)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "createInstance")
      }

      if (!tools::file_ext(path) %in% names(private$extractors)) {

        if (!is.null(self$getDefaultExtractor())) {
          extractor <- private$defaultExtractor$new(path)
          extractor
        } else {
          bdpar.log(message = paste0("The extension '", file_ext(path),
                                     "' is not registered"),
                    level = "WARN",
                    className = class(self)[1],
                    methodName = "createInstance")
        }
      } else {
        extractor <- private$extractors[[file_ext(path)]]
        extractor <- extractor$new(path)
        extractor
      }
    },
    #'
    #' @description Resets list of extractor to default state.
    #'
    reset = function() {
      private$extractors <- list("eml" = ExtractorEml,
                                 "tsms" = ExtractorSms,
                                 "ytbid" = ExtractorYtbid)
    },
    #'
    #' @description Prints pipeline representation. (Override print function)
    #'
    #' @param ... Further arguments passed to or from other methods.
    #'
    print = function(...) {
      print(self$getAllExtractors())
    }
  ),

  private = list(
    extractors = list(),
    defaultExtractor = NULL
  )
)
