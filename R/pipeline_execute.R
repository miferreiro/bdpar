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

#' @title Initiates the pipelining process
#'
#' @description \strong{pipeline_execute} is responsible for easily initialize
#' the pipelining preprocessing proccess.
#'
#' @docType methods
#'
#' @format NULL
#'
#' @usage pipeline_execute(path, pipe = SerialPipe$new(), instanceFactory = InstanceFactory$new())
#'
#' @param path (\emph{character}) path where the files to be preprocessed
#' are located.
#' @param pipe (\emph{TypePipe}) subclass of \code{\link{TypePipe}}, which
#' implements the whole pipeling process.
#' @param instanceFactory (\emph{InstanceFactory}) object implementing
#' the method \code{createInstance} to choose which type of \code{\link{Instance}}
#' is created.
#'
#' @section Details:
#' In the case that some pipe, defined on the workflow, needs some type of configuration,
#' it can be defined throught \emph{\link{bdpar.Options}} variable
#' which have differents methods to support the funcionality of different pipes.
#'
#' @return List of \code{\link{Instance}} that have been preprocessed.
#'
#' @examples
#' \dontrun{
#'
#' #If it is necessary to indicate any existing configuration key, do it through:
#' #bdpar.Options$set(key, value)
#' #If the key is not initialized, do it through:
#' #bdpar.Options$add(key, value)
#'
#' #Folder with the files to preprocess
#' path <- system.file(file.path("examples",
#'                               "testFiles"),
#'                     package = "bdpar")
#'
#' #Object which indicates the pipes' flow
#' pipe <- SerialPipe$new()
#'
#' #Object which decides how creates the instances
#' instanceFactory <- InstanceFactory$new()
#'
#' #Starting file preprocessing...
#' pipeline_execute(path = path,
#'                  pipe = pipe,
#'                  instanceFactory = instanceFactory)
#' }
#' @keywords NULL
#' @export pipeline_execute
#' @seealso \code{\link{Bdpar}}, \code{\link{bdpar.Options}},
#'          \code{\link{Connections}}, \code{\link{Instance}},
#'          \code{\link{InstanceFactory}}, \code{\link{ResourceHandler}},
#'          \code{\link{TypePipe}}, \code{\link{SerialPipe}}


pipeline_execute = function(path,
                            pipe = SerialPipe$new(),
                            instanceFactory = InstanceFactory$new()) {

  if (!"character" %in% class(path)) {
    stop("[pipeline_execute][Error] ",
         "Checking the type of the 'path' variable: ",
         class(path))
  }

  if (!"TypePipe" %in% class(pipe)) {
    stop("[pipeline_execute][Error] ",
         "Checking the type of the 'pipe' variable: ",
         class(pipe))
  }

  if (!"InstanceFactory" %in% class(instanceFactory)) {
    stop("[pipeline_execute][Error] ",
         "Checking the type of the 'instanceFactory' variable: ",
         class(instanceFactory))
  }

  bdpar_object <- Bdpar$new()
  bdpar_object$proccess_files(path, pipe, instanceFactory)
}
