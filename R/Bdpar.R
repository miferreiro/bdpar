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

#' @title Class to manage the preprocess of the files throughout the flow of pipes
#'
#' @description \code{\link{Bdpar}} class provides the static variables required
#' to perform the whole data flow process. To this end \code{\link{Bdpar}} is
#' in charge of (i) initialize the objects of handle the connections to APIs
#' (\code{\link{Connections}}) and handles json resources (\code{\link{ResourceHandler}})
#' and (ii) executing the flow of pipes (inherited from \code{\link{GenericPipeline}} class)
#' passed as argument.
#'
#' @section Details:
#' In the case that some pipe, defined on the workflow, needs some type of
#' configuration, it can be defined through \emph{\link{bdpar.Options}} variable
#' which have different methods to support the functionality of different pipes.
#'
#' @section Static variables:
#' \itemize{
#' \item{\bold{connections}:}{
#' (\emph{Connections}) object that handles the connections with YouTube and
#' Twitter.
#' }
#' \item{\bold{resourceHandler}:}{
#' (\emph{ResourceHandler}) object that handles the json resources files.
#' }
#' }
#'
#' @seealso \code{\link{bdpar.Options}}, \code{\link{Connections}},
#'          \code{\link{DefaultPipeline}}, \code{\link{DynamicPipeline}},
#'          \code{\link{GenericPipeline}}, \code{\link{Instance}},
#'          \code{\link{ExtractorFactory}}, \code{\link{ResourceHandler}},
#'          \code{\link{runPipeline}}
#'
#' @examples
#' \dontrun{
#'
#' #If it is necessary to indicate any configuration, do it through:
#' #bdpar.Options$set(key, value)
#' #If the key is not initialized, do it through:
#' #bdpar.Options$add(key, value)
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
#' objectBdpar <- Bdpar$new()
#'
#' #Starting file preprocessing...
#' objectBdpar$execute(path = path,
#'                     extractors = extractors,
#'                     pipeline = pipeline)
#' }
#' @keywords NULL
#'
#' @import R6
#' @export Bdpar

Bdpar <- R6Class(

  "Bdpar",

  public = list(
    #'
    #' @description Creates a \link{Bdpar} object. Initializes the static
    #' variables: \emph{connections} and \emph{resourceHandler}.
    #'
    initialize = function() {

      Bdpar[["private_fields"]][["connections"]] <- Connections$new()
      Bdpar[["private_fields"]][["resourceHandler"]] <- ResourceHandler$new()
    },
    #'
    #' @description Preprocess files through the indicated flow of pipes.
    #'
    #' @param path A \code{\link{character}} value. The path where the files to
    #' be processed are located.
    #' @param extractors A \code{\link{ExtractorFactory}} value. Class which
    #' implements the \code{createInstance} method to choose which type of
    #' \code{\link{Instance}} is created.
    #' @param pipeline A \code{\link{GenericPipeline}} value. Subclass of
    #' \code{\link{GenericPipeline}}, which implements the \code{execute} method.
    #' By default, it is the \code{\link{DefaultPipeline}} pipeline.
    #'
    #' @return The list of \code{Instances} that have been preprocessed.
    #'
    execute = function(path,
                       extractors = ExtractorFactory$new(),
                       pipeline = DefaultPipeline$new()) {

      if (!"character" %in% class(path)) {
        bdpar.log(message = paste0("Checking the type of the 'path' variable: ",
                                   class(path)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "execute")
      }

      if (!"ExtractorFactory" %in% class(extractors)) {
        bdpar.log(message = paste0("Checking the type of the 'extractors' variable: ",
                                   class(extractors)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "execute")
      }

      if (!inherits(pipeline, c("GenericPipeline"))) {
        bdpar.log(message = paste0("Checking the type of the 'pipeline' variable: ",
                                   class(pipeline)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "execute")
      }

      if (all(sapply(path, function(p) file.exists(p) || dir.exists(p)))) {
        files <- unlist(lapply(path, function(p) {
                                       ifelse(dir.exists(p),
                                              return(list.files(path = p,
                                                                recursive = TRUE,
                                                                full.names = TRUE,
                                                                all.files = TRUE)),
                                              return(p))
          }))
      } else {
        bdpar.log(message = paste0("Path parameter must be an existing file ",
                                   "or directory"),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "execute")
      }

      # Create the list of instances, which will contain the date, source, path,
      # data and a list of properties of the file that is in the indicated path.
      InstancesList <- sapply(files, extractors$createInstance)

      bdpar.log(message = paste0("Has been created: ", length(InstancesList),
                                 " instances."),
                level = "INFO",
                className = class(self)[1],
                methodName = "execute")

      listInstances <- sapply(InstancesList, pipeline$execute)

      listInstances
    }
  ),

  private = list(
    # Initialize the object that handles the different types of connections with
    # Youtube and Twitter
    connections = NULL,
    # Object that handles the json resources files.
    resourceHandler = NULL
  )
)
