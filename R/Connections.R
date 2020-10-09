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

#' @title Class to manage the connections with Twitter and YouTube
#'
#' @description The tasks of the functions that the \code{\link{Connections}} class has are to
#' establish the connections and control the number of requests that have been made
#' with the APIs of Twitter and YouTube.
#'
#' @section Details:
#' The way to indicate the keys of YouTube and Twitter has to be
#' through fields of \emph{\link{bdpar.Options}} variable:
#'
#' \strong{[twitter]}
#'
#' - \code{bdpar.Options$set("twitter.consumer.key", <<consumer_key>>)}
#'
#' - \code{bdpar.Options$set("twitter.consumer.secret", <<consumer_secret>>)}
#'
#' - \code{bdpar.Options$set("twitter.access.token", <<access_token>>)}
#'
#' - \code{bdpar.Options$set("twitter.access.token.secret", <<access_token_secret>>)}
#'
#' \strong{[youtube]}
#'
#' - \code{bdpar.Options$set("youtube.app.id", <<app_id>>)}
#'
#' - \code{bdpar.Options$set("youtube.app.password", <<app_password>>)}
#'
#' @section Note:
#' Fiels of unused connections will be automatically ignored by the platform.
#'
#' @seealso \code{\link{bdpar.Options}}, \code{\link{ExtractorTwtid}},
#'          \code{\link{ExtractorYtbid}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export Connections

Connections <- R6Class(

  "Connections",

  public = list(
    #'
    #' @description Creates a \code{\link{Connections}} object.
    #'
    initialize = function() { },

    ######################################################################
    #####                    Twitter connections                    ######
    ######################################################################
    #'
    #' @description Gets the Twitter token ID.
    #'
    #' @return Value of \code{twitterToken}.
    #'
    getTwitterToken = function() {
      private$twitterToken
    },
    #'
    #' @description Responsible of establishing the connection to Twitter.
    #'
    startConnectionWithTwitter = function() {

      if (!private$connectionWithTwitter) {
        if (!file.exists(file.path(Sys.getenv("HOME"), ".rtweet_token.rds"))) {
          if (any(!bdpar.Options$isSpecificOption("twitter.consumer.key"),
                  !bdpar.Options$isSpecificOption("twitter.consumer.secret"),
                  !bdpar.Options$isSpecificOption("twitter.access.token"),
                  !bdpar.Options$isSpecificOption("twitter.access.token.secret"),
                  is.null(bdpar.Options$get("twitter.consumer.key")),
                  is.null(bdpar.Options$get("twitter.consumer.secret")),
                  is.null(bdpar.Options$get("twitter.access.token")),
                  is.null(bdpar.Options$get("twitter.access.token.secret")))) {
            stop("[Connections][startConnectionWithTwitter][Error] Twitter API keys are ",
                 "not defined on bdpar.Options")
          }
          tryCatch(
            {
              private$twitterToken <- rtweet::create_token(
                app = "my_twitter_research_app",
                consumer_key = bdpar.Options$get("twitter.consumer.key"),
                consumer_secret = bdpar.Options$get("twitter.consumer.secret"),
                access_token = bdpar.Options$get("twitter.access.token"),
                access_secret = bdpar.Options$get("twitter.access.token.secret"),
                set_renv = TRUE)
              saveRDS(object = private$twitterToken,
                      file = file.path(Sys.getenv("HOME"),".rtweet_token.rds"))
              private$connectionWithTwitter <- TRUE

              message("[Connections][startConectionWithTwitter][Info] Twitter: established ",
                      "connection")
            }
            ,
            error = function(e) {
              message("[Connections][startConnectionWithTwitter][Error] Error on create_token: ",
                      paste(e))
            })
        } else {
          private$twitterToken <- readRDS(file.path(Sys.getenv("HOME"),
                                                    ".rtweet_token.rds"))
          private$connectionWithTwitter <- TRUE

          message("[Connections][startConectionWithTwitter][Info] Twitter: established ",
                  "connection")
        }
      }
    },
    #'
    #' @description Function in charge of handling the connection with Twitter.
    #'
    checkRequestToTwitter = function() {

      tryCatch(
      {
        if (rtweet::rate_limit(token = self$getTwitterToken())[[3]][[54]] == 0) {
          message("[Connections][checkRequestToTwitter][Info] ", paste(Sys.time()))

          message("[Connections][checkRequestToTwitter][Info] ",
                  "Waiting 15 min to be able to make new requests from twitter...")

          Sys.sleep(900)
        } else{
          message("[Connections][checkRequestToTwitter][Info] ",
                    "There are ", rtweet::rate_limit(token = self$getTwitterToken())[[3]][[54]],
                      " twitter requests to be consumed")
        }
      }
      ,
        warning = function(w) {

          warning("[Connections][checkRequestToTwitter][Warning]", paste(w))

          message("[Connections][checkRequestToTwitter][Info] ", paste(Sys.time()))

          message("[Connections][checkRequestToTwitter][Info] ",
                  "Waiting 15 min to be able to make new requests from twitter...")

          Sys.sleep(900)
        }
      )
    },
    ######################################################################
    #####                   YouTube connections                     ######
    ######################################################################
    #'
    #' @description Function able to establish the connection with YouTube.
    #'
    startConnectionWithYoutube = function() {

      if (!private$connectionWithYoutube) {

        if (any(!bdpar.Options$isSpecificOption("youtube.app.id"),
                !bdpar.Options$isSpecificOption("youtube.app.password"),
                is.null(bdpar.Options$get("youtube.app.id")),
                is.null(bdpar.Options$get("youtube.app.password")))) {
          stop("[Connections][startConnectionWithYoutube][Error] Youtube API keys are ",
               "not defined on bdpar.Options")
        }

        tuber::yt_oauth(bdpar.Options$get("youtube.app.id"),
                        bdpar.Options$get("youtube.app.password"))

        private$connectionWithYoutube <- TRUE

        message("[Connections][startConnectionWithYoutube][Info] Youtube: established",
                "connection")
      }
    },
    #'
    #' @description Function that increases in one the number of request to YouTube.
    #'
    addNumRequestToYoutube = function() {

      private$numRequestToYoutube <- private$numRequestToYoutube + 1
    },
    #'
    #' @description Handles the connection with YouTube.
    #'
    checkRequestToYoutube = function() {

      if (private$numRequestToYoutube >= self$getNumRequestMaxToYoutube()) {
        message("[Connections][checkRequestToYoutube][Info] ",
                "Waiting 15 min to be able to make new requests from youtube...")
        Sys.sleep(900)
        private$numRequestToYoutube <- 0
      }
    },
    #'
    #' @description Gets the number of maximum requests allowed by YouTube API.
    #'
    #' @return Value of number maximum of request to YouTube.
    #'
    getNumRequestMaxToYoutube = function() {
      private$numRequestMaxToYoutube
    }
  ),
  private = list(
    # A (\emph{numeric}) value. Indicates the number of requests made to YouTube.
    numRequestToYoutube = 0,
    # A (\emph{numeric}) value. Indicates the maximum number of requests with
    # YouTube.
    numRequestMaxToYoutube = 900,
    # A (\emph{numeric}) value. Indicates if the connection has been established
    # with YouTube.
    connectionWithYoutube = FALSE,
    # A (\emph{numeric}) value. Indicates if the connection has been established
    # with Twitter.
    connectionWithTwitter = FALSE,
    # A (\emph{Token}) value. Token to establish the connection to Twitter.
    twitterToken = NULL
  )
)
