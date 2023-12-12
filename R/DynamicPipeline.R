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

#' @title Class implementing a dynamic pipelining process
#'
#' @description This \code{\link{DynamicPipeline}} class inherits from the
#' \code{\link{GenericPipeline}} class. Includes the \strong{execute} method
#' which provides a dynamic pipelining implementation.
#''
#' @section Inherit:
#' This class inherits from \code{\link{GenericPipeline}} and implements the
#' \code{execute} abstract function.
#'
#' @seealso \code{\link{bdpar.log}}, \code{\link{Instance}},
#'          \code{\link{DefaultPipeline}}, \code{\link{GenericPipeline}},
#'          \code{\link{GenericPipe}}, \code{\link{\%>|\%}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export DynamicPipeline

DynamicPipeline <- R6Class(

  "DynamicPipeline",

  inherit = GenericPipeline,

  public = list(
    #'
    #' @description Creates a \code{\link{DynamicPipeline}} object.
    #'
    #' @param pipeline A \code{\link{list}} of \code{\link{GenericPipe}}
    #' objects. Initializes the flow of \code{\link{GenericPipe}}.
    #'
    initialize = function(pipeline = NULL) {

      if (!is.null(pipeline)) {
        if (!"list" %in% class(pipeline)) {
          bdpar.log(message = paste0("Checking the type of the 'pipeline' variable: ",
                                     class(pipeline)),
                    level = "FATAL",
                    className = class(self)[1],
                    methodName = "initialize")
        }

        if (!any(sapply(pipeline, inherits, "GenericPipe"))) {
          bdpar.log(message = paste0("Define pipes are not correct. Must be ",
                                     "inherit from 'GenericPipe' class. ",
                                     "Aborting..."),
                    level = "FATAL",
                    className = class(self)[1],
                    methodName = "initialize")
        }

        private$pipeline <- pipeline
      } else {
        private$pipeline <- list()
      }
    },
    #'
    #' @description Adds a \code{\link{GenericPipe}} or a
    #' \code{\link{GenericPipe}} list to the pipeline.
    #'
    #' @param pipe A \code{\link{GenericPipe}} object or a \code{\link{list}} of
    #' \code{\link{GenericPipe}} objects.
    #' @param pos A (\emph{numeric}) value. The value of the position to add.
    #' If it is NULL, \code{\link{GenericPipe}} is appended to the pipeline.
    #'
    #' @import rlist
    #'
    add = function(pipe, pos = NULL) {

      if (!is.list(pipe) || !is.vector(pipe)) {
        pipe <- list(pipe)
      }

      if (!any(sapply(pipe, inherits, "GenericPipe"))) {
        bdpar.log(message = paste0("Checking the type of the 'pipe' variable: ",
                                   class(pipe)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "add")
      }

      if (!is.null(pos)) {

        if (!"numeric" %in% class(pos)) {
          bdpar.log(message = paste0("Checking the type of the 'pos' variable: ",
                                     class(pos)),
                    level = "FATAL",
                    className = class(self)[1],
                    methodName = "add")
        }

        if (length(private$pipeline) == 0) {
          bdpar.log(message = "Pipeline empty, adding in the first position",
                    level = "WARN",
                    className = class(self)[1],
                    methodName = "add")
          private$pipeline <- list.flatten(list.append(private$pipeline, pipe))
        } else {
          if (length(private$pipeline) < pos) {
            bdpar.log(message = paste0("The position exceeds the length of ",
                                       "the pipeline, adding at the end of it"),
                      level = "WARN",
                      className = class(self)[1],
                      methodName = "add")
            private$pipeline <- list.flatten(list.append(private$pipeline, pipe))
          } else {

            if (!all(0 < pos, pos <= length(private$pipeline))) {
              bdpar.log(message = paste0("It can only be added between positions ",
                                         "'0' and '", length(private$pipeline),
                                         "'"),
                        level = "FATAL",
                        className = class(self)[1],
                        methodName = "add")
            }

            private$pipeline <- list.flatten(list.insert(private$pipeline, pos, pipe))
          }
        }

      } else {
        private$pipeline <- list.flatten(list.append(private$pipeline, pipe))
      }
    },
    #'
    #' @description Removes \code{\link{GenericPipe}s} by the position on the
    #' pipeline.
    #'
    #' @param pos A (\emph{numeric}) value. The value of the position to remove.
    #'
    #' @import rlist
    #'
    removeByPos = function(pos) {

      if (!"numeric" %in% class(pos)) {
        bdpar.log(message = paste0("Checking the type of the 'pos' variable: ",
                                   class(pos)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "removeByPos")
      }

      if (length(private$pipeline) == 0) {
        bdpar.log(message = "Pipeline empty. Imposible remove",
                  level = "WARN",
                  className = class(self)[1],
                  methodName = "removeByPos")
      } else {
        if (!any(sapply(pos, function(p) { all(0 < p, p <= length(private$pipeline)) }))) {
          bdpar.log(message = paste0("It can only be deleted between positions ",
                                     "'0' and '", length(private$pipeline), "'"),
                    level = "FATAL",
                    className = class(self)[1],
                    methodName = "removeByPos")
        }
        private$pipeline <- list.remove(private$pipeline, pos)
      }
    },
    #'
    #' @description Removes \code{\link{GenericPipe}s} by its name on the
    #' pipeline.
    #'
    #' @param pipe.name A (\emph{character}) value. The
    #' \code{\link{GenericPipe}s} name to remove.
    #'
    #' @import rlist
    #'
    removeByPipe = function(pipe.name) {

      if (!is.list(pipe.name) || !is.vector(pipe.name)) {
        pipe.name <- list(pipe.name)
      }

      if (!any(sapply(pipe.name, inherits, "character"))) {
        bdpar.log(message = paste0("Checking the type of the 'pipe.name' ",
                                   "variable (must be a character list)"),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "removeByPipe")
      }

      if (length(private$pipeline) == 0) {
        bdpar.log(message = "Pipeline empty. Imposible remove",
                  level = "WARN",
                  className = class(self)[1],
                  methodName = "removeByPipe")
      } else {
        pos <- which(pipe.name %in% lapply(private$pipeline, function(p) class(p)[1]))
        if (length(pos) == 0) {
          bdpar.log(message = "Not found elements to remove",
                    level = "WARN",
                    className = class(self)[1],
                    methodName = "removeByPipe")
        } else {
          private$pipeline <- list.remove(private$pipeline, pos)
        }
      }
    },
    #'
    #' @description Removes all \code{\link{GenericPipe}s} included on pipeline.
    #'
    removeAll = function() {
      private$pipeline <- list()
    },
    #'
    #' @description Function where is implemented the flow of the
    #' \code{\link{GenericPipe}s}.
    #'
    #' @param instance A (\emph{Instance}) value. The \code{\link{Instance}}
    #' that is going to be processed.
    #'
    execute = function(instance) {

      if (!"Instance" %in% class(instance)) {
        bdpar.log(message = paste0("Checking the type of the 'instance' variable: ",
                                   class(instance)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "execute")
      }

      if (length(private$pipeline) == 0) {
        bdpar.log(message = "Pipeline is empty",
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "execute")
      }

      call <- "instance"
      for (pipe in 1:length(private$pipeline)) {
        call <- paste(call, "%>|%", "private$pipeline[[", pipe, "]]")
      }

      tryCatch(
        instance <- eval(parse(text = call))
        ,
        error = function(e) {
          bdpar.log(message = paste0(instance$getPath()," :", paste(e)),
                    level = "ERROR",
                    className = class(self)[1],
                    methodName = "execute")

          instance$invalidate()
        }
      )

      instance
    },
    #'
    #' @description Gets a list with containing the set of \code{\link{GenericPipe}s}
    #' of the pipeline.
    #'
    #' @return The set of \code{\link{GenericPipe}s} containing the pipeline.
    #'
    get = function() {
      private$pipeline
    },
    #'
    #' @description Prints pipeline representation. (Override print function)
    #'
    #' @param ... Further arguments passed to or from other methods.
    #'
    print = function(...) {

      call <- "instance"
      for (pipe in private$pipeline) {
        call <- paste(call, "%>|%\n\t", class(pipe)[1])
      }
      call <- paste0(call, '\n')
      cat(call)
    },
    #'
    #' @description Returns a \code{\link{character}} representing the pipeline
    #'
    #' @return \code{\link{DynamicPipeline}} \code{\link{character}} representation
    #'
    toString = function() {
      call <- "instance"
      for (pipe in private$pipeline) {
        call <- paste(call, "%>|%\n\t", class(pipe)[1])
      }
      call
    }
  ),

  private = list(
    pipeline = list()
  )
)
