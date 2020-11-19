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
#' @description \strong{runPipeline} is responsible for easily initialize
#' the pipelining preprocessing process.
#'
#' @docType methods
#'
#' @format NULL
#'
#' @usage runPipeline(path, extractors = ExtractorFactory$new(),
#' pipeline = DefaultPipeline$new(), summary = FALSE)
#'
#' @param path (\emph{character}) path where the files to be preprocessed
#' are located.
#' @param extractors (\emph{ExtractorFactory}) object implementing
#' the method \code{createInstance} to choose which type of \code{\link{Instance}}
#' is created.
#' @param pipeline (\emph{GenericPipeline}) subclass of \code{\link{GenericPipeline}}, which
#' implements the whole pipeling process.
#' @param summary (\emph{logical}) flag indicating if a summary of the
#' pipeline execution is provided or not.
#'
#' @section Details:
#' In the case that some pipe, defined on the workflow, needs some type of configuration,
#' it can be defined thought \emph{\link{bdpar.Options}} variable
#' which have different methods to support the functionality of different pipes.
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
#' #If it is neccesary parallelize, do it through:
#' #bdpar.Options$set("numCores", numCores)
#'
#' #If it is necessary to change the behavior of the log, do it through:
#' #bdpar.Options$configureLog(console = TRUE, threshold = "INFO", file = NULL)
#'
#' #Folder with the files to preprocess
#' path <- system.file(file.path("example"),
#'                     package = "bdpar")
#'
#' #Object which decides how creates the instances
#' extractors <- ExtractorFactory$new()
#'
#' #Object which indicates the pipes' flow
#' pipeline <- DefaultPipeline$new()
#'
#' #Starting file preprocessing...
#' runPipeline(path = path,
#'             extractors = extractors,
#'             pipeline = pipeline,
#'             summary = TRUE)
#' }
#' @keywords NULL
#' @export runPipeline
#' @seealso \code{\link{Bdpar}}, \code{\link{bdpar.Options}},
#'          \code{\link{Connections}}, \code{\link{DefaultPipeline}},
#'          \code{\link{DynamicPipeline}}, \code{\link{GenericPipeline}},
#'          \code{\link{Instance}}, \code{\link{ExtractorFactory}},
#'          \code{\link{ResourceHandler}}
#'

runPipeline <- function(path,
                        extractors = ExtractorFactory$new(),
                        pipeline = DefaultPipeline$new(),
                        summary = FALSE) {

  if (!"character" %in% class(path)) {
    bdpar.log(message = paste0("Checking the type of the 'path' variable: ",
                               class(path)),
             level = "FATAL",
             className = NULL,
             methodName = "runPipeline")
  }

  if (!"ExtractorFactory" %in% class(extractors)) {
    bdpar.log(message = paste0("Checking the type of the 'extractors' variable: ",
                               class(extractors)),
              level = "FATAL",
              className = NULL,
              methodName = "runPipeline")
  }

  if (!inherits(pipeline, c("GenericPipeline"))) {
    bdpar.log(message = paste0("Checking the type of the 'pipeline' variable: ",
                               class(pipeline)),
              level = "FATAL",
              className = NULL,
              methodName = "runPipeline")
  }

  if (!"logical" %in% class(summary)) {
    bdpar.log(message = paste0("Checking the type of the 'summary' variable: ",
                               class(summary)),
              level = "FATAL",
              className = NULL,
              methodName = "runPipeline")
  }

  bdpar_object <- Bdpar$new()
  bdpar_object$execute(path, extractors, pipeline, summary)
}
