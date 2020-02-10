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

#' @title Absctract super class implementing the pipelining proccess.
#'
#' @description Abstract super class to establish the flow of Pipes.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \code{GenericPipeline$new()}
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{execute:}}{
#' function where is implemented the flow of the pipes.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{execute(instance)}
#' }
#' \item{\emph{Value:}}{
#' the preprocessed \code{\link{Instance}}.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{instance:}}{
#' (\emph{Instance}) the \code{\link{Instance}} that is going to be processed.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{get:}}{
#' gets a list with containinig the set of pipes of the pipeline,
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{get()}
#' }
#' \item{\emph{Value:}}{
#' the set of pipes containing the pipeline.
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{DefaultPipeline}}, \code{\link{DynamicPipeline}},
#'          \code{\link{Instance}}, \code{\link{GenericPipe}}, \code{\link{\%>I\%}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export GenericPipeline

GenericPipeline <- R6Class(

  "GenericPipeline",

  public = list(

    initialize = function() { },

    execute = function(instance) {
      stop("[GenericPipeline][execute][Error] I am an abstract interface method")
    },

    get = function() {
      stop("[GenericPipeline][get][Error] I am an abstract interface method")
    }
  )
)
