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

#' @title Class to handle the creation of Instance types
#'
#' @description \code{\link{ExtractorFactory}} class builds the appropriate
#' \code{\link{Instance}} object according to the file extension.
#'
#' @seealso \code{\link{ExtractorEml}}, \code{\link{ExtractorSms}},
#' \code{\link{ExtractorTwtid}}, \code{\link{ExtractorYtbid}},
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
      private$extractors <- list("eml" = ExtractorEml,
                                 "tsms" = ExtractorSms,
                                 "twtid" = ExtractorTwtid,
                                 "ytbid" = ExtractorYtbid)
    },
    #'
    #' @description Adds an extractor to the list of extensions.
    #'
    #' @param extension A \code{\link{character}} value. The name of the
    #' extension option.
    #' @param extractor A \code{Object} value. The extractor of the new
    #' extension.
    #'
    #' @import rlist
    #'
    registerExtractor = function(extension, extractor) {
      if (!"character" %in% class(extension)) {
        stop("[", class(self)[1], "][registerExtractor][Error] Checking the ",
             "type of the 'extension' variable: ", class(extension))
      }

      if (self$isSpecificExtractor(extension)) {
        stop("[", class(self)[1], "][registerExtractor][Error] '", extension,
             "' extension is already added")
      } else {
        if (!"R6ClassGenerator" %in% class(extractor) || extractor$inherit != "Instance") {
          stop("[", class(self)[1], "][registerExtractor][Error] Checking the ",
               "type of the 'extractor' variable: ", class(extractor))
        }
        private$extractors <- list.append(private$extractors, extractor)
        names(private$extractors)[length(private$extractors)] <- extension
      }
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
        stop("[", class(self)[1], "][setExtractor][Error] Checking the type of ",
             "the 'extension' variable: ", class(extension))
      }

      if (!self$isSpecificExtractor(extension)) {
        stop("[", class(self)[1], "][setExtractor][Error] '", extension,
             "' extension is not configured")
      } else {
        if (!"R6ClassGenerator" %in% class(extractor) ||
            extractor$inherit != "Instance") {
          stop("[", class(self)[1], "][setExtractor][Error] Checking the type ",
               "of the 'extractor' variable: ", class(extractor))
        }
        private$extractors[[extension]] <- extractor
      }
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
        stop("[", class(self)[1], "][removeExtractor][Error] Checking the type ",
             "of the 'extension' variable: ", class(extension))
      }

      if (!self$isSpecificExtractor(extension)) {
        stop("[", class(self)[1], "][removeExtractor][Error] '", extension,
             "' extension is not configured")
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
    #' file extension.
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
        stop("[", class(self)[1], "][createInstance][Error] ",
             "Checking the type of the 'path' variable: ",
             class(path))
      }

      if (!tools::file_ext(path) %in% names(private$extractors)) {
        message("[", class(self)[1], "][createInstance][Warning] ",
                "The extension '", file_ext(path), "' is not registered")
      } else {
        extractor <- private$extractors[[file_ext(path)]]
        extractor <- extractor$new(path)
        return(extractor)
      }
    },
    #'
    #' @description Resets list of extractor to default state.
    #'
    reset = function() {
      private$extractors <- list("eml" = ExtractorEml,
                                 "tsms" = ExtractorSms,
                                 "twtid" = ExtractorTwtid,
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
    extractors = list()
  )
)
