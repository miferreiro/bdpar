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

#' @title Class implementing a default pipelining proccess.
#'
#' @description This \code{\link{SerialPipe}} class inherits from the
#' \code{\link{TypePipe}} class. Includes the \strong{pipeAll} method which
#' provides a default pipelining implementation.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \code{SerialPipe$new()}
#'
#' @section Details:
#' This class uses the default implementation provided by its parent class
#' (\code{\link{TypePipe}}) and handles all the unexpected errors that may
#' appear during the execution of the pipelining process.
#'
#' @section Inherit:
#' This class inherits from \code{\link{TypePipe}} and implements the
#' \code{pipeAll} abstract function.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{pipeAll:}}{
#' implementation of the pipeAll function provided by the super class
#' (\code{\link{TypePipe}}). In this case, the pipeAll function is called
#' so that the default pipelining is executed.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{pipeAll(instance)}
#' }
#' \item{\emph{Value:}}{
#' The preprocessed \code{\link{Instance}}.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{instance:}}{
#' (\emph{Instance}) The \code{\link{Instance}} that is going to be processed.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{Instance}}, \code{\link{TypePipe}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export SerialPipe

SerialPipe <- R6Class(

  "SerialPipe",

  inherit = TypePipe,

  public = list(

    initialize = function() {

    },

    pipeAll = function(instance) {

      if (!"Instance" %in% class(instance)) {
        stop("[SerialPipe][pipeAll][Error]
                Checking the type of the variable: instance ",
                  class(instance));
      }

      message("[SerialPipe][pipeAll][Info] ", instance$getPath(), "\n")

      tryCatch(
        instance <- super$pipeAll(instance)
      ,
        error = function(e) {
          message("[SerialPipe][pipeAll][Error]", instance$getPath()," :", paste(e), "\n")
          instance$invalidate()
        }
      )

      return(instance)
    }
  )
)
