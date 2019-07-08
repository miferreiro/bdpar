#
# Bdpa4r provide a tool to easily build customized data flows to pre-process
# large volumes of information from different sources. To this end, bdpa4R allows
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

#' @title Class to handle tweets files with twtid extension
#'
#' @description This class inherits from the \code{\link{Instance}} class and
#' implements the functions of extracting the text and the date of an twtid type file.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \code{ExtractorTwtid$new(path)}
#' \itemize{
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{path:}}{
#' (\emph{character}) path of the twtid type file.
#' }
#' }
#' }
#' }
#'
#' @section Details:
#'
#' Twitter connection is handled through the \code{\link{Connections}} class
#' which loads the Twitter API credentials from the configuration file.
#' Additionally, to increase the processing speed, each twitter query is stored
#' in a cache to avoid the execution of duplicated queries. To enable this option,
#' cache location should be in the \emph{cachePathTwtid} indicated in the
#' \emph{cache} section from the configuration file. This variable has to be the
#' path to store the tweets and it is neccesary that it has two folder named:
#' "_spam_" and "_ham_"
#'
#' \strong{[cache]}
#'
#' cachePathTwtid = \emph{<<cache_path_twtid>>}
#'
#' @section Inherit:
#' This class inherits from \code{\link{Instance}} and implements the
#' \code{obtainSource} and \code{obtainDate} abstracts functions.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{obtainId:}}{
#' obtains the ID of an specific tweet. Reads the ID of the file indicated in
#' the variable path.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{obtainId()}
#' }
#' }
#' }
#' \item{\bold{getId:}}{
#' gets the ID of an specific tweet.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getId()}
#' }
#' \item{\emph{Value:}}{
#' value of tweet ID.
#' }
#' }
#' }
#' \item{\bold{obtainDate:}}{
#' obtains the date from a specific tweet ID. If the tweet has been previously
#' cached the tweet date is loaded from cache path. Otherwise, the request is
#' performed using Twitter API and the date is automatically formatted to
#' "%a %b %d %H:%M:%S %Z %Y" (i.e. "Thu May 02 06:52:36 UTC 2013").
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{obtainDate()}
#' }
#' }
#' }
#' \item{\bold{obtainSource:}}{
#' obtains the source from a specific tweet ID. If the tweet has previously been
#' cached the source is loaded from cache path. Otherwise, the request is
#' performed using on Twitter API.
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
#'  (\emph{character}) ID of tweet.
#' }
#' }
#'
#' @seealso \code{\link{ExtractorEml}}, \code{\link{ExtractorSms}},
#'\code{\link{ExtractorYtbid}}, \code{\link{Instance}},
#'
#' @keywords NULL
#'
#' @import pipeR R6
#' @export ExtractorTwtid

ExtractorTwtid <- R6Class(

  classname = "ExtractorTwtid",

  inherit = Instance,

  public = list(

    initialize = function(path) {

      if (!requireNamespace("rtweet", quietly = TRUE)) {
        stop("[ExtractorTwtid][initialize][Error]
                Package \"rtweet\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!requireNamespace("rjson", quietly = TRUE)) {
        stop("[ExtractorTwtid][initialize][Error]
                Package \"rjson\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!"character" %in% class(path)) {
        stop("[ExtractorTwtid][initialize][Error]
                Checking the type of the variable: path ",
                  class(path))
      }

      path %>>%
        super$initialize()

      self$obtainId()
      #Singleton
      Bdp4R[["private_fields"]][["connections"]]$startConnectionWithTwitter()

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

      cachePath <- read.ini(Bdp4R[["private_fields"]][["configurationFilePath"]])$cache$cachePathTwtid

      if (file.exists(
        paste(cachePath,
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
            cachePath,"/_",
            super$getSpecificProperty("target"),
            "_/",
            self$getId(),
            ".json",
            sep = ""
          )

        dataFromJsonFile <- rjson::fromJSON(file = private$path)

        if (!is.na(dataFromJsonFile[["date"]]) &&
              !is.null(dataFromJsonFile[["date"]]) &&
                dataFromJsonFile[["date"]] != "") {

          dataFromJsonFile[["date"]] %>>%
            super$setDate()

          return()
        }
      }

      if (super$getDate() == "") {

        dateTwtid <- ""
        sourceTwtid <- ""
        langTwtid <- ""

        Bdp4R[["private_fields"]][["connections"]]$checkRequestToTwitter()

        lookup <- tryCatch(

          self$getId() %>>%
            as.character() %>>%
              rtweet::lookup_tweets(.,p = Bdp4R[["private_fields"]][["connections"]]$getTwitterToken()),

          warning = function(w) {
            cat(paste("[ExtractorTwtid][obtainDate][Warning] Date twtid warning: ",
                      self$getId(), " ", paste(w)),"\n")
          },

          error = function(e) {
            cat(paste("[ExtractorTwtid][obtainDate][Error] Date twtid error: ",
                      self$getId(), " ", paste(e)),"\n")
          }
        )

        if (!is.null(lookup) &&
            "tbl_df" %in% class(lookup)) {

          dateTwtid <- lookup$created_at
          sourceTwtid <- lookup$text
          langTwtid <- lookup$lang

        } else {
          dateTwtid <- ""
          sourceTwtid <- ""
          langTwtid <- ""
        }

        formatDateTwtid <- "%Y-%m-%d %H:%M:%S %Z"
        StandardizedDate <-
          as.POSIXct(dateTwtid, format = formatDateTwtid)
        formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"

        format(StandardizedDate, formatDateGeneric) %>>%
          as.character() %>>%
              super$setDate()

        lista <- list(
          source = sourceTwtid,
          date = super$getDate(),
          lang = langTwtid
        )

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
          cat(paste("[ExtractorTwtid][obtainDate][Error] exportJSON: ",
                      self$getId(), " " , paste(e), "\n"))

          lista <- list(source = "",
                        date = super$getDate(),
                        lang = langTwtid)

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
      }

      return()
    },

    obtainSource = function() {

      cachePath <- read.ini(Bdp4R[["private_fields"]][["configurationFilePath"]])$cache$cachePathTwtid

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
        dateTwtid <- ""
        sourceTwtid <- ""
        langTwtid <- ""

        Bdp4R[["private_fields"]][["connections"]]$checkRequestToTwitter()

        lookup <- tryCatch(
          self$getId() %>>%
            as.character() %>>%
              rtweet::lookup_tweets(.,p = Bdp4R[["private_fields"]][["connections"]]$getTwitterToken()),

          warning = function(w) {
            cat(paste("[ExtractorTwtid][obtainSource][Warning] Source twtid warning: ",
                      self$getId(), " ", paste(w)),"\n")
          },

          error = function(e) {
            cat(paste("[ExtractorTwtid][obtainSource][Error] Source twtid error: ",
                      self$getId(), " ", paste(e)),"\n")
          }
        )

        if (!is.null(lookup) &&
            "tbl_df" %in% class(lookup)) {

          dateTwtid <- lookup$created_at
          sourceTwtid <- lookup$text
          langTwtid <- lookup$lang

        } else {
          dateTwtid <- ""
          sourceTwtid <- ""
          langTwtid <- ""
        }

        sourceTwtid %>>%
          super$setSource()

        super$getSource() %>>%
          super$setData()

        formatDateTwtid <- "%Y-%m-%d %H:%M:%S %Z"
        StandardizedDate <-
          as.POSIXct(dateTwtid, format = formatDateTwtid)
        formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"

        dateTwtid <- format(StandardizedDate, formatDateGeneric)

        lista <- list(
          source = super$getSource(),
          date = as.character(dateTwtid),
          lang = langTwtid
        )

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

          cat(paste("[ExtractorTwtid][obtainSource][Error] exportJSON: ",
                    self$getId(), " " , paste(e), "\n"))

          lista <- list(
            source = "",
            date = as.character(dateTwtid),
            lang = langTwtid
          )

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
      }

      return()
    }
  ),

  private = list(
    id = ""
  )
)
