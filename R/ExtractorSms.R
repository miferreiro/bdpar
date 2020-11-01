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

#' @title Class to handle SMS files with tsms extension
#'
#' @description This class that inherits from the \code{\link{Instance}} class and
#' implements the functions of extracting the text and the date of an tsms type file.
#'
#' @section Details:
#' Due to the fact that the creation date of the message can not be
#' extracted from the text of an SMS, the date will be initialized to empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{Instance}} and implements the
#' \code{obtainSource} and \code{obtainDate} abstracts functions.
#'
#' @seealso \code{\link{ExtractorEml}}, \code{\link{ExtractorTwtid}},
#' \code{\link{ExtractorYtbid}}, \code{\link{Instance}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export ExtractorSms

ExtractorSms <- R6Class(

  classname = "ExtractorSms",

  inherit = Instance,

  public = list(
    #'
    #' @description Creates a \code{\link{ExtractorSms}} object.
    #'
    #' @param path A \code{\link{character}} value. Path of the tsms file.
    #'
    #' @import pipeR
    #'
    initialize = function(path) {

      if (!"character" %in% class(path)) {
        bdpar.log(message = paste0("Checking the type of the 'path' variable: ",
                                   class(path)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }
      path %>>%
        super$initialize()
    },
    #'
    #' @description Obtains the date of the SMS file.
    #'
    #' @import pipeR
    #'
    obtainDate = function() {
      "" %>>%
        super$setDate()
    },
    #'
    #' @description Obtains the source of the SMS file. Reads the file indicated
    #' in the path. In addition, it initializes the data field with the initial
    #' source.
    #'
    #' @import pipeR
    #'
    obtainSource = function() {

      super$getPath() %>>%
        readLines(warn = FALSE) %>>%
          super$setSource()

      super$getSource() %>>%
        super$setData()
    }
  )
)
