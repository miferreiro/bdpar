#
# Bdpar provide a tool to easily build customized data flows to pre-process
# large volumes of information from different sources. To this end, bdpar allows
# to (i) easily use and create new functionalities and (ii) develop new data
# source extractors according to the user needs. Additionally, the package
# provides by default a predefined data flow to extract and preprocess the most
# relevant information (tokens, dates, ... ) from some textual sources (SMS,
# email, tweets, YouTube comments).
#
# Copyright (C) 2018 Sing Group (University of Vigo)
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
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \code{ExtractorYtbid$new(path)}
#' \itemize{
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{path:}}{
#' (\emph{character}) path of the ytbid type file.
#' }
#' }
#' }
#' }
#'
#' @section Details:
#' YouTube conection is handled through the \code{\link{Connections}} class
#' which loads the YouTube API credentials from the configuration file.
#' Additionally, to increase the processing speed, each youtube query is stored
#' in a cache to avoid the execution of duplicated queries. To enable this option,
#' cache location should be in the \emph{cachePathYtbid} indicated in the
#' \emph{cache} section from the configuration file. This variable has to be the
#' path to store the comments and it is neccesary that it has two folder named:
#' "_spam_" and "_ham_"
#'
#' \strong{[cache]}
#'
#' cachePathYtbid = \emph{<<cache_path_ytbid>>}
#'
#' @section Inherit:
#' This class inherits from \code{\link{Instance}} and implements the
#' \code{obtainSource} and \code{obtainDate} abstracts functions.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{obtainId:}}{
#' obtains the id of the ytbid. Read the id of the file indicated
#' in the variable path.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{obtainId()}
#' }
#' }
#' }
#' \item{\bold{getId:}}{
#' gets of comment ID.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getId()}
#' }
#' \item{\emph{Value:}}{
#' value of comment ID.
#' }
#' }
#' }
#' \item{\bold{obtainDate:}}{
#' obtains the date from a specific comment ID. If the comment has been previously
#' cached the comment date is loaded from cache path. Otherwise, the request is
#' perfomed using YouTube API and the date is then formatted to the established
#' standard.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{obtainDate()}
#' }
#' }
#' }
#' \item{\bold{obtainSource:}}{
#' obtains the source from a specific comment ID. If the comment has previously
#' been cached the source is loaded from cache path. Otherwise, the request is
#' performed using on YouTube API.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{obtainSource()}
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{id:}}{
#'  (\emph{character}) ID of comment.
#' }
#' }
#'
#' @seealso \code{\link{ExtractorEml}}, \code{\link{ExtractorSms}},
#' \code{\link{ExtractorTwtid}}, \code{\link{Instance}}
#'
#' @keywords NULL
#'
#' @import pipeR R6
#' @export ExtractorYtbid

ExtractorYtbid <- R6Class(

  classname = "ExtractorYtbid",

  inherit = Instance,

  public = list(

    initialize = function(path) {

      if (!requireNamespace("tuber", quietly = TRUE)) {
        stop("[ExtractorYtbid][initialize][Error]
                Package \"tuber\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!requireNamespace("rjson", quietly = TRUE)) {
        stop("[ExtractorYtbid][initialize][Error]
                Package \"rjson\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!"character" %in% class(path)) {
        stop("[ExtractorYtbid][initialize][Error]
                Checking the type of the variable: path ",
                  class(path))
      }

      path %>>%
        super$initialize()

      self$obtainId()
      #Singleton
      Bdpar[["private_fields"]][["connections"]]$startConnectionWithYoutube()

      return()
    },

    obtainId = function() {

      private$id <- readLines(super$getPath(), warn = FALSE, n = 1)

      return()
    },

    getId = function() {

      return(private$id)
    },

    obtainDate = function() {

      cachePath <- read.ini(Bdpar[["private_fields"]][["configurationFilePath"]])$cache$cachePathYtbid

      if (file.exists(
        paste(
          cachePath,
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
            cachePath,
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

        Bdpar[["private_fields"]][["connections"]]$checkRequestToYoutube()

        comment <- tryCatch(

          tuber::get_comments(
            filter = c(comment_id = self$getId()),
            textFormat = "plainText"
          ),

          warning = function(w) {
            cat("[ExtractorYtbid][obtainDate][Warning] Date ytbid warning: ",
                      self$getId(), " ", paste(w),"\n")
          },

          error = function(e) {
            cat("[ExtractorYtbid][obtainDate][Error] Date ytbid error: ",
                      self$getId(), " ", paste(e),"\n")
          }
        )
      }

      Bdpar[["private_fields"]][["connections"]]$addNumRequestToYoutube()

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
            cat("[ExtractorYtbid][obtainDate][Warning] Date ytbid warning as.POSIXct: ",
                      self$getId(), " ", paste(w),"\n")
          },

          error = function(e) {
            cat("[ExtractorYtbid][obtainDate][Error] Date ytbid error as.POSIXct: ",
                      self$getId(), " ", paste(e),"\n")
          }
        )

        formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
        format(StandardizedDate, formatDateGeneric) %>>%
          as.character() %>>%
            super$setDate()

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
            cachePath,
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

        cat(paste("[ExtractorYtbid][obtainDate][Error] exportJSON: ",
                  self$getId(), " " , paste(e), "\n"))

        lista <- list(source = "",
                      date = super$getDate())

        exportJSON <- rjson::toJSON(lista)

        cat(
          exportJSON,
          file = paste(
            cachePath,
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

      return()
    },

    obtainSource = function() {

      cachePath <- read.ini(Bdpar[["private_fields"]][["configurationFilePath"]])$cache$cachePathYtbid

      if (file.exists(
        paste(
          cachePath,
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
            cachePath,
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

          dataFromJsonFile[["source"]] %>>%
            super$setSource()

          super$getSource() %>>%
            super$setData()

          return()
        }
      }

      if (super$getSource() == "") {

        dateYtbid <- ""
        sourceYtbid <- ""

        Bdpar[["private_fields"]][["connections"]]$checkRequestToYoutube()

        comment <- tryCatch(

          tuber::get_comments(
            filter = c(comment_id = self$getId()),
            textFormat = "plainText"
          ),

          warning = function(w) {
            cat("[ExtractorYtbid][obtainSource][Warning] Source ytbid warning: ",
                      self$getId(), " ", paste(w),"\n")
          },

          error = function(e) {
            cat("[ExtractorYtbid][obtainSource][Error] Source ytbid error: ",
                      self$getId(), " ", paste(e),"\n")
          }
        )
      }

      Bdpar[["private_fields"]][["connections"]]$addNumRequestToYoutube()

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
            cat("[ExtractorYtbid][obtainSource][Warning] Date ytbid warning as.POSIXct: ",
                      self$getId(), " " , paste(w), "\n")
          },

          error = function(e) {
            cat("[ExtractorYtbid][obtainSource][Error] Date ytbid error as.POSIXct: ",
                      self$getId(), " " , paste(e), "\n")
          }
        )

        formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"

        dateYtbid <- as.character(format(StandardizedDate, formatDateGeneric))

      }

      sourceYtbid %>>%
          super$setSource()

      super$getSource() %>>%
        super$setData()

      lista <- list(source = super$getSource(),
                    date = dateYtbid)

      tryCatch({

        exportJSON <- rjson::toJSON(lista)

        cat(
          exportJSON,
          file = paste(
            cachePath,
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

        cat("[ExtractorYtbid][obtainSource][Error] exportJSON: ",
                  self$getId(), " " , paste(e), "\n")

        lista <- list(source = "", date = dateYtbid)
        exportJSON <- rjson::toJSON(lista)

        cat(
          exportJSON,
          file = paste(
            cachePath,
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

      return()
    }
  ),

  private = list(
    id = ""
  )
)
