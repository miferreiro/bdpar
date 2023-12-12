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

#' @title Class to manage the connections with YouTube
#'
#' @description The tasks of the functions that the \code{\link{Connections}}
#' class has are to establish the connections and control the number of requests
#' that have been made with the API of YouTube.
#'
#' @section Details:
#' The way to indicate the keys of YouTube has to be
#' through fields of \emph{\link{bdpar.Options}} variable:
#'
#' \strong{[youtube]}
#'
#' - \code{bdpar.Options$set("youtube.app.id", <<app_id>>)}
#'
#' - \code{bdpar.Options$set("youtube.app.password", <<app_password>>)}
#'
#' @section Note:
#' Fields of unused connections will be automatically ignored by the platform.
#'
#' @seealso \code{\link{bdpar.Options}}, \code{\link{ExtractorYtbid}}
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
    #####                   YouTube connections                     ######
    ######################################################################
    #'
    #' @description Function able to establish the connection with YouTube.
    #'
    startConnectionWithYoutube = function() {

      if (!private$connectionWithYoutube) {

        if (!bdpar.Options$isSpecificOption("youtube.app.id") ||
            !bdpar.Options$isSpecificOption("youtube.app.password") ||
            is.null(bdpar.Options$get("youtube.app.id")) ||
            is.null(bdpar.Options$get("youtube.app.password"))) {
          bdpar.log(message = "Youtube API keys are not defined on bdpar.Options",
                    level = "FATAL",
                    className = class(self)[1],
                    methodName = "startConnectionWithYoutube")
        }

        tuber::yt_oauth(bdpar.Options$get("youtube.app.id"),
                        bdpar.Options$get("youtube.app.password"))

        private$connectionWithYoutube <- TRUE

        bdpar.log(message = "Youtube: established connection",
                  level = "INFO",
                  className = class(self)[1],
                  methodName = "startConnectionWithYoutube")
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

        bdpar.log(message = "Waiting 15 min to be able to make new requests from youtube...",
                  level = "INFO",
                  className = class(self)[1],
                  methodName = "checkRequestToYoutube")

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
    connectionWithYoutube = FALSE
  )
)
