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

#' @title Abstract super class implementing the pipelining process
#'
#' @description Abstract super class to establish the flow of Pipes.
#'
#' @seealso \code{\link{bdpar.log}}, \code{\link{DefaultPipeline}},
#'          \code{\link{DynamicPipeline}}, \code{\link{Instance}},
#'          \code{\link{GenericPipe}}, \code{\link{\%>|\%}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export GenericPipeline

GenericPipeline <- R6Class(

  "GenericPipeline",

  public = list(
    #'
    #' @description Creates a \code{\link{GenericPipeline}} object.
    #'
    initialize = function() { },
    #'
    #' @description Function where is implemented the flow of the
    #' \code{\link{GenericPipe}s}.
    #'
    #' @param instance A \code{\link{Instance}} value. The \code{\link{Instance}}
    #' that is going to be processed.
    #'
    #' @return The preprocessed \code{\link{Instance}}.
    #'
    execute = function(instance) {
      bdpar.log(message = "I am an abstract interface method",
                level = "FATAL",
                className = class(self)[1],
                methodName = "execute")
    },
    #'
    #' @description Gets a list with containing the set of \code{\link{GenericPipe}s}
    #'  of the pipeline.
    #'
    #' @return The set of \code{\link{GenericPipe}s} containing the pipeline.
    #'
    get = function() {
      bdpar.log(message = "I am an abstract interface method",
                level = "FATAL",
                className = class(self)[1],
                methodName = "get")
    },
    #'
    #' @description Returns a \code{\link{character}} representing the pipeline.
    #'
    #' @details This function allows to set a place to define a \code{\link{character}}
    #' representation of the structure of a pipeline.
    #'
    #' @return \code{\link{GenericPipeline}} \code{\link{character}} representation
    #'
    toString = function() {
      bdpar.log(message = "Character representation of the pipeline not implemented",
                level = "WARN",
                className = class(self)[1],
                methodName = "get")
    }
  )
)
