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

#' @title Class to manage the preprocess of the files throughout the flow of pipes
#'
#' @description \code{Bdpar} class provides the static variables required
#' to perform the whole data flow process. To this end \code{Bdpar} is
#' in charge of (i) initialize the objects of handle the connections to APIs
#' (\code{\link{Connections}}) and handles json resources (\code{\link{ResourceHandler}})
#' and (ii) executing the flow of pipes (inherited from \code{\link{TypePipe}} class)
#' passed as argument.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \code{Bdpar$new()}
#'
#' @section Details:
#' In the case that some pipe, defined on the workflow, needs some type of configuration,
#' it can be defined throught \emph{\link{bdpar.Options}} variable
#' which have differents methods to support the funcionality of different pipes.
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
#' @section Methods:
#' \itemize{
#' \item{\bold{proccess_files}:}{
#' preprocess files through the indicated flow of pipes.
#' \itemize{
#' \item{\emph{Usage}:}{
#' \preformatted{
#' proccess_files(path,
#'                pipe = SerialPipe$new(),
#'                instanceFactory = InstanceFactory$new())}
#'
#' }
#' \item{\emph{Value}:}{
#' list of \code{Instances} that have been preprocessed.
#' }
#' \item{\emph{Arguments}:}{
#' \itemize{
#' \item{\strong{path}:}{
#' (\emph{character}) path where the files to be processed are located.
#' }
#' \item{\strong{pipe}:}{
#' (\emph{TypePipe}) subclass of \code{\link{TypePipe}}, which implements the \code{pipe} method.
#' }
#' \item{\strong{instanceFactory}:}{
#' (\emph{InstanceFactory}) class which implements the method \code{createInstance}
#' to choose which type of \code{\link{Instance}} is created.
#' }
#' }
#' }
#' }
#' }
#' }
#' @seealso \code{\link{bdpar.Options}}, \code{\link{Connections}},
#'          \code{\link{Instance}}, \code{\link{InstanceFactory}},
#'          \code{\link{ResourceHandler}}, \code{\link{pipeline_execute}},
#'          \code{\link{TypePipe}}, \code{\link{SerialPipe}}
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
#' objectBdpar <- Bdpar$new()
#'
#' #Starting file preprocessing...
#' objectBdpar$proccess_files(path, pipe, instanceFactory)
#' }
#' @keywords NULL
#'
#' @import R6
#' @export Bdpar

Bdpar <- R6Class(

  "Bdpar",

  public = list(

    initialize = function() {

      Bdpar[["private_fields"]][["connections"]] <- Connections$new()
      Bdpar[["private_fields"]][["resourceHandler"]] <- ResourceHandler$new()

    },

    proccess_files = function(path,
                              pipe = SerialPipe$new(),
                              instanceFactory = InstanceFactory$new()) {

      if (!"character" %in% class(path)) {
        stop("[Bdpar][proccess_files][Error] ",
             "Checking the type of the 'path' variable: ",
             class(path))
      }

      if (!"TypePipe" %in% class(pipe)) {
        stop("[Bdpar][proccess_files][Error] ",
             "Checking the type of the 'pipe' variable: ",
             class(pipe))
      }

      if (!"InstanceFactory" %in% class(instanceFactory)) {
        stop("[Bdpar][proccess_files][Error] ",
             "Checking the type of the 'instanceFactory' variable: ",
             class(instanceFactory))
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
        stop("[Bdpar][proccess_files][Error] Path parameter must be an existing ",
             "file or directory")
      }

      #Create the list of instances, which will contain the date, source, path, data
      #and a list of properties of the file that is in the indicated path
      InstancesList <- sapply(files, instanceFactory$createInstance)

      message("[Bdpar][proccess_files][Info] ", "Has been created: ", length(InstancesList)," instances.")
      listInstances <- sapply(InstancesList, pipe$pipeAll)

      return(listInstances)
    }
  ),

  private = list(
    #Initialize the object that handles the different types of connections with youtube and twitter
    connections = NULL,
    #Object that handles the json resources files.
    resourceHandler = NULL
  )
)
