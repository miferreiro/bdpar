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

#' @title Class to handle SMS files with tsms extension
#'
#' @description This class that inherits from the \code{\link{Instance}} class and
#' implements the functions of extracting the text and the date of an tsms type file.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \code{ExtractorSms$new(path)}
#' \itemize{
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{path:}}{
#' (\emph{character}) path of the tsms type file.
#' }
#' }
#' }
#' }
#'
#' @section Details:
#' Due to the fact that the creation date of the message can not be
#' extracted from the text of an SMS, the date will be initialized to empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{Instance}} and implements the
#' \code{obtainSource} and \code{obtainDate} abstracts functions.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{obtainDate:}}{
#' function that obtains the date of the SMS file.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{obtainDate()}
#' }
#' }
#' }
#' \item{\bold{obtainSource:}}{
#' obtains the source of the SMS file. Reads the file indicated in
#' the path. In addition, it initializes the data with the initial source.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{obtainSource()}
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{ExtractorEml}}, \code{\link{ExtractorTwtid}},
#' \code{\link{ExtractorYtbid}}, \code{\link{Instance}}
#'
#' @keywords NULL
#'
#' @import pipeR R6
#' @export ExtractorSms

ExtractorSms <- R6Class(

  classname = "ExtractorSms",

  inherit = Instance,

  public = list(

    initialize = function(path) {

      if (!"character" %in% class(path)) {
        stop("[ExtractorSms][initialize][Error] ",
             "Checking the type of the 'path' variable: ",
             class(path))
      }
      path %>>%
        super$initialize()

    },

    obtainDate = function() {

      "" %>>%
        super$setDate()

      return()
    },

    obtainSource = function() {

      super$getPath() %>>%
        readr::read_file() %>>%
          super$setSource()

      super$getSource() %>>%
        super$setData()

      return()
    }
  )
)
