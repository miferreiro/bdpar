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

#' @title Class implementing a dynamic pipelining proccess.
#'
#' @description This \code{\link{DynamicPipeline}} class inherits from the
#' \code{\link{GenericPipeline}} class. Includes the \strong{execute} method which
#' provides a dynamic pipelining implementation.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \code{DynamicPipeline$new(pipeline = NULL)}
#'
#' @section Inherit:
#' This class inherits from \code{\link{GenericPipeline}} and implements the
#' \code{execute} abstract function.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{add:}}{
#' adds a pipe or a pipe list to the pipeline
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{add(pipe, pos)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{pipe:}}{
#' (\emph{GenericPipe}) pipe objects or a list of pipes to add
#' }
#' \item{\strong{pos:}}{
#' (\emph{numeric}) the value of the pos to add. If it is NULL, pipe is appended to the pipeline
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{removeByPos:}}{
#' removes pipes by the position on the pipeline
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{removeByPos(pos)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{pos:}}{
#' (\emph{numeric}) the pipe positions to remove.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{removeByPipe:}}{
#' removes pipes by its name on the pipeline
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{removeByPipe(pipe.name)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{pipe.name:}}{
#' (\emph{character}) the pipe name to remove.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{removeAll:}}{
#' removes all pipes included on pipeline
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{removeAll()}
#' }
#' }
#' }
#'
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
#' @seealso \code{\link{Instance}}, \code{\link{DefaultPipeline}},
#'          \code{\link{GenericPipeline}}, \code{\link{GenericPipe}},
#'          \code{\link{\%>|\%}}
#'
#' @keywords NULL
#'
#' @import R6 rlist
#' @export DynamicPipeline

DynamicPipeline <- R6Class(

  "DynamicPipeline",

  inherit = GenericPipeline,

  public = list(

    initialize = function(pipeline = NULL) {

      if (!is.null(pipeline)) {
        if (!"list" %in% class(pipeline)) {
          stop("[DynamicPipeline][initialize][Error] ",
               "Checking the type of the 'pipeline' variable: ",
               class(pipeline))
        }

        if (!any(sapply(pipeline, inherits, "GenericPipe"))) {
          stop("[DynamicPipeline][initialize][Error] ",
               "Define pipes are not correct. Must be inherit from 'GenericPipe' ",
               "class. Aborting...")
        }

        private$pipeline <- pipeline
      } else {
        private$pipeline <- list()
      }
    },

    add = function(pipe, pos = NULL) {

      if (!is.list(pipe) || !is.vector(pipe)) {
        pipe <- list(pipe)
      }

      if (!any(sapply(pipe, inherits, "GenericPipe"))) {
        stop("[DynamicPipeline][add][Error] Checking the type of the 'pipe' variable: ",
             class(pipe))
      }

      if (!is.null(pos)) {

        if (!"numeric" %in% class(pos)) {
          stop("[DynamicPipeline][add][Error] Checking the type of the 'pos' variable: ",
               class(pos))
        }

        if (length(private$pipeline) == 0) {
          warning("[DynamicPipeline][add][Warning] Pipeline empty, adding in ",
                  "the first position")
          private$pipeline <- list.flatten(list.append(private$pipeline, pipe))
        } else {
          if (length(private$pipeline) < pos) {
            warning("[DynamicPipeline][add][Warning] The position exceeds the ",
                    "length of the pipeline, adding at the end of it")
            private$pipeline <- list.flatten(list.append(private$pipeline, pipe))
          } else {

            if (!all(0 < pos, pos <= length(private$pipeline))) {
              stop("[DynamicPipeline][add][Error] It can only be added between ",
                   "positions '0' and '", length(private$pipeline), "'")
            }

            private$pipeline <- list.flatten(list.insert(private$pipeline, pos, pipe))
          }
        }

      } else {
        private$pipeline <- list.flatten(list.append(private$pipeline, pipe))
      }
    },

    removeByPos = function(pos) {

      if (!"numeric" %in% class(pos)) {
        stop("[DynamicPipeline][removeByPos][Error] Checking the type of the 'pos' variable: ",
             class(pos))
      }

      if (length(private$pipeline) == 0) {
        warning("[DynamicPipeline][removeByPos][Warning] Pipeline empty. Imposible remove")
      } else {
        if (!any(sapply(pos, function(p) { all(0 < p, p <= length(private$pipeline)) }))) {
          stop("[DynamicPipeline][removeByPos][Error] It can only be deleted between ",
               "positions '0' and '", length(private$pipeline), "'")
        }
        private$pipeline <- list.remove(private$pipeline, pos)
      }
    },

    removeByPipe = function(pipe.name) {

      if (!is.list(pipe.name) || !is.vector(pipe.name)) {
        pipe.name <- list(pipe.name)
      }

      if (!any(sapply(pipe.name, inherits, "character"))) {
        stop("[DynamicPipeline][removeByPipe][Error] Checking the type of the 'pipe.name' variable (must be a character list)")
      }

      if (length(private$pipeline) == 0) {
        warning("[DynamicPipeline][removeByPipe][Warning] Pipeline empty. Imposible remove")
      } else {
        pos <- which(pipe.name %in% lapply(private$pipeline, function(p) class(p)[1]))
        if (length(pos) == 0) {
          warning("[DynamicPipeline][removeByPipe][Warning] Not found elements to remove")
        } else {
          private$pipeline <- list.remove(private$pipeline, pos)
        }
      }
    },

    removeAll = function() {
      private$pipeline <- list()
    },

    execute = function(instance) {

      if (!"Instance" %in% class(instance)) {
        stop("[DynamicPipeline][execute][Error] ",
             "Checking the type of the 'instance' variable: ",
             class(instance))
      }

      if (length(private$pipeline) == 0) {
        stop("[DynamicPipeline][execute][Error] ",
             "Pipeline is empty")
      }

      call <- "instance"
      for (pipe in 1:length(private$pipeline)) {
        call <- paste(call, "%>|%", "private$pipeline[[", pipe, "]]")
      }

      tryCatch(
        instance <- eval(parse(text = call))
        ,
        error = function(e) {
          message("[DynamicPipeline][pipeAll][Error]", instance$getPath()," :", paste(e))
          instance$invalidate()
        }
      )

      return(instance)
    },

    get = function() {
      private$pipeline
    },

    print = function(...) {

      call <- "instance"
      for (pipe in private$pipeline) {
        call <- paste(call, "%>|%", class(pipe)[1])
      }

      print(call)
    }
  ),

  private = list(
    pipeline = list()
  )
)
