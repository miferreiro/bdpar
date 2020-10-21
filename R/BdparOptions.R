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

#' @import R6 rlist

BdparOptions <- R6Class(

  "BdparOptions",

  public = list(

    initialize = function() {

      private$bdpar.options <- list(extractorEML.mpaPartSelected = "text/plain",
                                    resources.abbreviations.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                             "bdpar",
                                                                             "resources",
                                                                             "abbreviations-json"),
                                    resources.contractions.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                            "bdpar",
                                                                            "resources",
                                                                            "contractions-json"),
                                    resources.interjections.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                             "bdpar",
                                                                             "resources",
                                                                             "interjections-json"),
                                    resources.slangs.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                      "bdpar",
                                                                      "resources",
                                                                      "slangs-json"),
                                    resources.stopwords.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                         "bdpar",
                                                                         "resources",
                                                                         "stopwords-json"),
                                    twitter.consumer.key = NULL,
                                    twitter.consumer.secret = NULL,
                                    twitter.access.token = NULL,
                                    twitter.access.token.secret = NULL,
                                    cache.twitter.path = NULL,
                                    teeCSVPipe.output.path = "teeCSVPipe.output.csv",
                                    youtube.app.id = NULL,
                                    youtube.app.password = NULL,
                                    cache.youtube.path = NULL,
                                    cache = FALSE,
                                    cache.folder = ".cache")
    },

    get = function(key) {
      if (!"character" %in% class(key)) {
        stop("[BdparOptions][get][Error] Checking the type of the 'key' variable: ",
             class(key))
      }

      if (!key %in% names(private$bdpar.options)) {
        stop("[BdparOptions][get][Error] '", key, "' option is not configured")
      }
      private$bdpar.options[[key]]
    },

    add = function(key, value) {
      if (!"character" %in% class(key)) {
        stop("[BdparOptions][add][Error] Checking the type of the 'key' variable: ",
             class(key))
      }

      if (self$isSpecificOption(key)) {
        stop("[BdparOptions][add][Error] '", key, "' option is already ",
             "configured with the value: ", self$get(key))
      } else {
        private$bdpar.options <- list.append(private$bdpar.options, value)
        names(private$bdpar.options)[length(private$bdpar.options)] <- key
      }
    },

    set = function(key, value) {
      if (!"character" %in% class(key)) {
        stop("[BdparOptions][set][Error] Checking the type of the 'key' variable: ",
             class(key))
      }



      if (!self$isSpecificOption(key)) {
        stop("[BdparOptions][set][Error] '", key, "' option is not configured")
      } else {
        if (is.null(value)) {
          private$bdpar.options[key] <- list(value)
        } else {
          private$bdpar.options[[key]] <- value
        }
      }
    },

    remove = function(key) {
      if (!"character" %in% class(key)) {
        stop("[BdparOptions][remove][Error] Checking the type of the 'key' variable: ",
             class(key))
      }

      if (!self$isSpecificOption(key)) {
        stop("[BdparOptions][remove][Error] '", key, "' option is not configured")
      } else {
        private$bdpar.options <- list.remove(private$bdpar.options, key)
      }
    },

    getAll = function() {
      private$bdpar.options
    },

    reset = function() {
      private$bdpar.options <- list(extractorEML.mpaPartSelected = "text/plain",
                                    resources.abbreviations.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                             "bdpar",
                                                                             "resources",
                                                                             "abbreviations-json"),
                                    resources.contractions.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                            "bdpar",
                                                                            "resources",
                                                                            "contractions-json"),
                                    resources.interjections.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                             "bdpar",
                                                                             "resources",
                                                                             "interjections-json"),
                                    resources.slangs.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                      "bdpar",
                                                                      "resources",
                                                                      "slangs-json"),
                                    resources.stopwords.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                         "bdpar",
                                                                         "resources",
                                                                         "stopwords-json"),
                                    twitter.consumer.key = NULL,
                                    twitter.consumer.secret = NULL,
                                    twitter.access.token = NULL,
                                    twitter.access.token.secret = NULL,
                                    cache.twitter.path = NULL,
                                    teeCSVPipe.output.path = "teeCSVPipe.output.csv",
                                    youtube.app.id = NULL,
                                    youtube.app.password = NULL,
                                    cache.youtube.path = NULL,
                                    cache = FALSE,
                                    cache.folder = ".cache")
    },

    isSpecificOption = function(key) {
      key %in% names(private$bdpar.options)
    },

    cleanCache = function() {
      if (any(!self$isSpecificOption("cache.folder"),
              is.null(self$get("cache.folder")))) {
        stop("[cleanCache][Error] Cache folder ",
             "is not defined in bdpar.Options")
      }

      unlink(self$get("cache.folder"),
             recursive = T)

      if (!dir.exists(self$get("cache.folder"))) {
        message("[cleanCache][Info] The cache folder \"",
                self$get("cache.folder"),
                "\" has been deleted successfully!")
      } else {
        stop("[cleanCache][Error] The cache folder \"",
             self$get("cache.folder"),
             "\" could not deleted correctly!")
      }
    },

    print = function(...) {
      print(self$getAll())
    }
  ),

  private = list(
    bdpar.options = list()
  )
)
