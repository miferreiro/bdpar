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

#' @title Class to handle comments of YouTube files with ytbid extension
#'
#' @description This class inherits from the \code{\link{Instance}} class and
#' implements the functions of extracting the text and the date of an ytbid type file.
#'
#' @section Details:
#' YouTube connection is handled through the \code{\link{Connections}} class
#' which loads the YouTube API credentials from the \emph{{bdpar.Options}} object.
#' Additionally, to increase the processing speed, each Youtube query is stored
#' in a cache to avoid the execution of duplicated queries. To enable this option,
#' cache location should be in the \strong{"cache.youtube.path"} field of
#' \emph{\link{bdpar.Options}} variable. This variable has to be the
#' path to store the comments and it is necessary that it has two folder named:
#' "_spam_" and "_ham_"
#'
#' @section Inherit:
#' This class inherits from \code{\link{Instance}} and implements the
#' \code{obtainSource} and \code{obtainDate} abstracts functions.
#'
#' @seealso \code{\link{bdpar.Options}}, \code{\link{Connections}},
#'          \code{\link{ExtractorEml}}, \code{\link{ExtractorSms}},
#'          \code{\link{ExtractorTwtid}}, \code{\link{Instance}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export ExtractorYtbid

ExtractorYtbid <- R6Class(

  classname = "ExtractorYtbid",

  inherit = Instance,

  public = list(
    #'
    #' @description Creates a \code{\link{ExtractorYtbid}} object.
    #'
    #' @param path A \code{\link{character}} value. Path of the ytbid file.
    #' @param cachePath A \code{\link{character}} value. Path of the cache
    #' location. If it is NULL, checks if is defined in the
    #' \strong{"cache.youtube.path"} field of \code{\link{bdpar.Options}}
    #' variable.
    #'
    initialize = function(path,
                          cachePath = NULL) {

      if (!"character" %in% class(path)) {
        bdpar.log(message = paste0("Checking the type of the 'path' variable: ",
                                   class(path)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      super$initialize(path)

      self$obtainId()
      #Singleton
      Bdpar[["private_methods"]][["connections"]]()$startConnectionWithYoutube()

      if (is.null(cachePath)) {
        if (!bdpar.Options$isSpecificOption("cache.youtube.path") ||
            is.null(bdpar.Options$get("cache.youtube.path"))) {
          bdpar.log(message = paste0("Path of YouTube comments' cache is ",
                                     "neither defined in initialize or in bdpar.Options"),
                    level = "FATAL",
                    className = class(self)[1],
                    methodName = "initialize")
        } else {
          cachePath <- bdpar.Options$get("cache.youtube.path")
        }
      }

      if (!"character" %in% class(cachePath)) {
        bdpar.log(message = paste0("Checking the type of the 'cachePath' variable: ",
                                   class(cachePath)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      private$cachePath <- cachePath
    },
    #'
    #' @description Obtains the ID of the specific Youtube's comment. Reads the ID
    #' of the file indicated in the variable path.
    #'
    obtainId = function() {
      private$id <- readLines(super$getPath(), warn = FALSE, n = 1)
    },
    #'
    #' @description Gets the ID of an specific Youtube's comment.
    #'
    #' @return Value of Youtube's comment ID.
    #'
    getId = function() {
      private$id
    },
    #'
    #' @description Obtains the date from a specific comment ID. If the comment
    #' has been previously cached the comment date is loaded from cache path.
    #' Otherwise, the request is perfomed using YouTube API and the date is then
    #' formatted to the established standard.
    #'
    obtainDate = function() {

      if (file.exists(
        paste(
          private$cachePath,
          "/_",
          super$getSpecificProperty("target"),
          "_/",
          self$getId(),
          ".json",
          sep = ""
        )
      )) {

        private$path <-
          paste(
            private$cachePath,
            "/_",
            super$getSpecificProperty("target"),
            "_/",
            self$getId(),
            ".json",
            sep = ""
          )

        dataFromJsonFile <- rjson::fromJSON(file = super$getPath())

        if (!is.na(dataFromJsonFile[["date"]]) &&
              !is.null(dataFromJsonFile[["date"]]) &&
                dataFromJsonFile[["date"]] != "") {

          super$setDate(dataFromJsonFile[["date"]])
          return()

        }
      }

      if (super$getDate() == "") {

        dateYtbid <- ""
        sourceYtbid <- ""

        Bdpar[["private_methods"]][["connections"]]()$checkRequestToYoutube()

        comment <- tryCatch(

          tuber::get_comments(
            filter = c(comment_id = self$getId()),
            textFormat = "plainText"
          ),

          warning = function(w) {
            bdpar.log(message = paste0("Date ytbid warning: ", self$getId(),
                                       " ", paste(w)),
                      level = "WARN",
                      className = class(self)[1],
                      methodName = "obtainDate")
          },

          error = function(e) {
            bdpar.log(message = paste0("Date ytbid error: ", self$getId(),
                                       " ", paste(w)),
                      level = "ERROR",
                      className = class(self)[1],
                      methodName = "obtainDate")
          }
        )
      }

      Bdpar[["private_methods"]][["connections"]]()$addNumRequestToYoutube()

      if (!is.null(comment) && is.data.frame(comment)) {

        dateYtbid  <- levels(comment[["publishedAt"]][["publishedAt"]])
        sourceYtbid <- levels(comment[["textDisplay"]][["textDisplay"]])

      } else {
        dateYtbid <- ""
        sourceYtbid <- ""
      }

      if (dateYtbid != "") {

        dateYtbid <- paste(substring(dateYtbid, 0, 10),
                            substring(dateYtbid, 12, nchar(dateYtbid)),
                              " ")

        StandardizedDate <- tryCatch(

          as.POSIXct(dateYtbid),

          warning = function(w) {
            bdpar.log(message = paste0("Date ytbid warning as.POSIXct: ",
                                       self$getId(), " ", paste(w)),
                      level = "WARN",
                      className = class(self)[1],
                      methodName = "obtainDate")
          },

          error = function(e) {
            bdpar.log(message = paste0("Date ytbid error as.POSIXct: ",
                                       self$getId(), " ", paste(e)),
                      level = "ERROR",
                      className = class(self)[1],
                      methodName = "obtainDate")
          }
        )

        formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
        super$setDate(as.character(format(StandardizedDate,
                                          formatDateGeneric)))

      } else {
        super$setDate("")
        sourceYtbid <- ""
      }

      lista <- list(source = sourceYtbid,
                    date = super$getDate())

      tryCatch({

        exportJSON <- rjson::toJSON(lista)

        cat(
          exportJSON,
          file = paste(
            private$cachePath,
            "/_",
            super$getSpecificProperty("target"),
            "_/",
            self$getId(),
            ".json",
            sep = ""
          ),
          sep = "\n"
        )
      },

      error = function(e) {

        bdpar.log(message = paste0("exportJSON: ", self$getId(),
                                   " " , paste(e)),
                  level = "ERROR",
                  className = class(self)[1],
                  methodName = "obtainDate")

        lista <- list(source = "",
                      date = super$getDate())

        exportJSON <- rjson::toJSON(lista)

        cat(
          exportJSON,
          file = paste(
            private$cachePath,
            "/_",
            super$getSpecificProperty("target"),
            "_/",
            self$getId(),
            ".json",
            sep = ""
          ),
          sep = "\n"
        )
      })
    },
    #'
    #' @description Obtains the source from a specific comment ID. If the
    #' comment has previously been cached the source is loaded from cache path.
    #' Otherwise, the request is performed using on YouTube API.
    #'
    obtainSource = function() {

      if (file.exists(
        paste(
          private$cachePath,
          "/_",
          super$getSpecificProperty("target"),
          "_/",
          self$getId(),
          ".json",
          sep = ""
        )
      )) {
        private$path <-
          paste(
            private$cachePath,
            "/_",
            super$getSpecificProperty("target"),
            "_/",
            self$getId(),
            ".json",
            sep = ""
          )


        dataFromJsonFile <- rjson::fromJSON(file = super$getPath())


        if (!is.na(dataFromJsonFile[["source"]]) &&
              !is.null(dataFromJsonFile[["source"]]) &&
                dataFromJsonFile[["source"]] != "") {

          super$setSource(dataFromJsonFile[["source"]])

          super$setData(super$getSource())

          return()
        }
      }

      if (super$getSource() == "") {

        dateYtbid <- ""
        sourceYtbid <- ""

        Bdpar[["private_methods"]][["connections"]]()$checkRequestToYoutube()

        comment <- tryCatch(

          tuber::get_comments(
            filter = c(comment_id = self$getId()),
            textFormat = "plainText"
          ),

          warning = function(w) {
            bdpar.log(message = paste0("Source ytbid warning: ",
                                       self$getId(), " ", paste(w)),
                      level = "WARN",
                      className = class(self)[1],
                      methodName = "obtainDate")
          },

          error = function(e) {
            bdpar.log(message = paste0("Source ytbid error: ",
                                       self$getId(), " ", paste(e)),
                      level = "ERROR",
                      className = class(self)[1],
                      methodName = "obtainSource")
          }
        )
      }

      Bdpar[["private_methods"]][["connections"]]()$addNumRequestToYoutube()

      if (!is.null(comment) && is.data.frame(comment)) {

        dateYtbid  <- levels(comment[["publishedAt"]][["publishedAt"]])
        sourceYtbid <- levels(comment[["textDisplay"]][["textDisplay"]])

      } else {
        dateYtbid <- ""
        sourceYtbid <- ""
      }

      if (dateYtbid != "") {

        dateYtbid <- paste(substring(dateYtbid, 0, 10),
                           substring(dateYtbid, 12, nchar(dateYtbid)),
                           " ")

        StandardizedDate <- tryCatch(

          as.POSIXct(dateYtbid),

          warning = function(w) {
            bdpar.log(message = paste0("Date ytbid warning as.POSIXct: ",
                                       self$getId(), " " , paste(w)),
                      level = "WARN",
                      className = class(self)[1],
                      methodName = "obtainSource")
          },

          error = function(e) {
            bdpar.log(message = paste0("Date ytbid error as.POSIXct: ",
                                       self$getId(), " " , paste(e)),
                      level = "ERROR",
                      className = class(self)[1],
                      methodName = "obtainSource")
          }
        )

        formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"

        dateYtbid <- as.character(format(StandardizedDate,
                                         formatDateGeneric))

      }

      super$setSource(sourceYtbid)

      super$setData(super$getSource())

      lista <- list(source = super$getSource(),
                    date = dateYtbid)

      tryCatch({

        exportJSON <- rjson::toJSON(lista)

        cat(
          exportJSON,
          file = paste(
            private$cachePath,
            "/_",
            super$getSpecificProperty("target"),
            "_/",
            self$getId(),
            ".json",
            sep = ""
          ),
          sep = "\n"
        )
      },

      error = function(e) {

        bdpar.log(message = paste0("exportJSON: ",
                                   self$getId(), " " , paste(e)),
                  level = "ERROR",
                  className = class(self)[1],
                  methodName = "obtainSource")

        lista <- list(source = "", date = dateYtbid)
        exportJSON <- rjson::toJSON(lista)

        cat(
          exportJSON,
          file = paste(
            private$cachePath,
            "/_",
            super$getSpecificProperty("target"),
            "_/",
            self$getId(),
            ".json",
            sep = ""
          ),
          sep = "\n"
        )
      })
    },
    #'
    #' @description Returns a \code{\link{character}} representing the instance
    #'
    #' @return \code{\link{Instance}} \code{\link{character}} representation
    #'
    toString = function() {
      toRet <- paste0("\tPath: ", as.character(private$path),
                      "\n\tDate: ", as.character(private$date),
                      "\n\tIsValid: ", as.character(private$isValid),
                      "\n\tSource: \"", as.character(private$source), "\"",
                      "\n\tData: \"", as.character(private$data), "\"",
                      "\n\tFlowPipes: ", paste(as.character(unlist(private$flowPipes)), collapse = " "),
                      "\n\tBanPipes: ", paste(as.character(unlist(private$banPipes)), collapse = " "),
                      "\n\tProperties: ")

      properties <- ""
      if (length(private$properties) != 0) {
        properties <- "\n\t\t"
        properties <- paste0(properties, paste0(lapply(names(private$properties), function(propertyName) {
          paste0("- ", propertyName, ": ",
                 paste(as.character(unlist(private$properties[[propertyName]])), collapse = " "),
                 collapse = "")
        }), collapse = "\n\t\t"))
      } else {
        properties <- "Not located"
      }
      toRet <- paste0(toRet, properties, "\n")
      toRet
    }
  ),

  private = list(
    # A \code{\link{character}} value. ID of Youtube's comment.
    id = "",
    # A \code{\link{character}} value. Path of the cache location. If it is NULL,
    # checks if is defined in the \strong{"cache.youtube.path"} field of
    # \emph{\link{bdpar.Options}} variable.
    cachePath = ""
  )
)
