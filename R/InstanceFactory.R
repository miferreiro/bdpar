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

#' @title Class to handle the creation of Instance types
#'
#' @description \code{\link{InstanceFactory}} class builds the appropriate
#' \code{\link{Instance}} object according to the file extension.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \code{InstanceFactory$new()}
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{createInstance:}}{
#' builds the \code{\link{Instance}} object according to the file extension.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{createInstance(path)}
#' }
#' \item{\emph{Value:}}{
#' the \code{\link{Instance}} corresponding object according to the file extension.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{path:}}{
#' (\emph{character}) path of the file to create an \code{\link{Instance}}.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{ExtractorEml}}, \code{\link{ExtractorSms}},
#' \code{\link{ExtractorTwtid}}, \code{\link{ExtractorYtbid}},
#' \code{\link{Instance}}
#'
#' @keywords NULL
#'
#' @import R6 tools
#' @export InstanceFactory

InstanceFactory <- R6Class(

  "InstanceFactory",

  public = list(

    initialize = function() {

    },

    createInstance = function(path) {

      if (!"character" %in% class(path)) {
        stop("[InstanceFactory][createInstance][Error] ",
             "Checking the type of the 'path' variable: ",
             class(path))
      }

      switch(tools::file_ext(path),
       `eml` =  return(ExtractorEml$new(path)),
       `tsms` = return(ExtractorSms$new(path)),
       `twtid` = return(ExtractorTwtid$new(path)),
       `ytbid` = return(ExtractorYtbid$new(path))
      )

      return()
    }
  )
)
