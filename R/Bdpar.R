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
#' in charge of (i) loading the configuration parameters (from configurationsTemplate.ini
#' or the user configuration file) and (ii) executing the flow of pipes
#' (inherited from \code{\link{TypePipe}} class) passed as argument.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \code{Bdpar$new(configurationFilePath = NULL, editConfigurationFile = FALSE)}
#' \itemize{
#' \item{\emph{Arguments}:}{
#' \itemize{
#' \item{\strong{configurationFilePath}:}{
#' (\emph{character}) path where the configuration file is located. The file
#' must have the .ini extension. In the case that the argument is null, the
#' default configuration file (configurationsTemplate.ini) will be used.
#' }
#' \item{\strong{editConfigurationFile}:}{
#' (\emph{boolean}) indicates if open an editor to change the configuration
#' file or not.
#' }
#' }
#' }
#' }
#'
#' @section Details:
#' The configuration file can be indicated by the user or use the default
#' configuration file (configurationsTemplate.ini). In addition, once we call
#' the \code{Bdpar} constructor, it will be possible to choose if the user
#' wants to edit the file of indicated configurations or not.
#'
#' The \emph{configurationFilePath} file should have the following structure
#' (Depends on the Pipes used). Also the configurationsTemplate.ini has this
#' structure:
#'
#' \strong{[twitter]}
#'
#' ConsumerKey = \emph{<<consumer_key>>}
#'
#' ConsumerSecret = \emph{<<consumer_secret>>}
#'
#' AccessToken = \emph{<<access_token>>}
#'
#' AccessTokenSecret = \emph{<<access_token_secret>>}
#'
#' \strong{[youtube]}
#'
#' app_id = \emph{<<app_id>>}
#'
#' app_password = \emph{<<app_password>>}
#'
#' \strong{[eml]}
#'
#' PartSelectedOnMPAlternative = \emph{<<part_selected>>} (text/html or text/plain)
#'
#' \strong{[resourcesPath]}
#'
#' resourcesAbbreviationsPath = \emph{<<resources_abbreviations_path>>}
#'
#' resourcesContractionsPath = \emph{<<resources_contractions_path>>}
#'
#' resourcesInterjectionsPath = \emph{<<resources_interjections_path>>}
#'
#' resourcesSlangsPath = \emph{<<resources_slangs_path>>}
#'
#' resourcesStopWordsPath = \emph{<<resources_stopWords_path>>}
#'
#' \strong{[CSVPath]}
#'
#' outPutTeeCSVPipePath = \emph{<<out_put_teeCSVPipe_path>>}
#'
#' \strong{[cache]}
#'
#' cachePathTwtid = \emph{<<cache_path_twtid>>}
#'
#' cachePathYtbid = \emph{<<cache_path_ytbid>>}
#'
#' @section Note:
#' In the case of choosing to edit the configuration file, the default editor
#' will be opened.
#'
#' @section Static variables:
#' \itemize{
#' \item{\bold{connections}:}{
#' (\emph{Connections}) object that handles the connections with YouTube and
#' Twitter.
#' }
#' \item{\bold{configurationFilePath}:}{
#' (\emph{character}) path where the configuration file is located. The file
#' must have the .ini extension. In the case that the argument is null, the
#' default configuration file (configurationsTemplate.ini) will be used.
#' },
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
#' @seealso \code{\link{Connections}}, \code{\link{Instance}},
#' \code{\link{InstanceFactory}}, \code{\link{pipeline_execute}},
#' \code{\link{TypePipe}}, \code{\link{SerialPipe}}
#'
#' @examples
#' \dontrun{
#' #Path where the configuration file are located
#' configurationFilePath <- system.file(file.path("examples",
#'                                                "configurationsExample.ini"),
#'                                      package ="bdpar")
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
#' objectBdpar <- Bdpar$new(configurationFilePath)
#'
#' #Starting file preprocessing...
#' objectBdpar$proccess_files(path, pipe, instanceFactory)
#' }
#' @keywords NULL
#'
#' @import ini R6 tools
#' @importFrom svMisc file_edit
#' @importFrom utils write.table
#' @export Bdpar

Bdpar <- R6Class(

  "Bdpar",

  public = list(

    initialize = function(configurationFilePath = NULL,
                          editConfigurationFile = FALSE) {

      if (!is.null(configurationFilePath)) {
        if (!"character" %in% class(configurationFilePath)) {
          stop("[Bdpar][initialize][Error]
                  Checking the type of the variable: configurationFilePath ",
                    class(configurationFilePath))
        }

        if (!"ini" %in% tools::file_ext(configurationFilePath)) {
          stop("[Bdpar][initialize][Error]
                  Checking the extension of the file: configurationFilePath ",
                    tools::file_ext(configurationFilePath))
        }
      }

      if (!"logical" %in% class(editConfigurationFile)) {
        stop("[Bdpar][initialize][Error]
                Checking the type of the variable: editConfigurationFile ",
                  class(editConfigurationFile))
      }

      if (editConfigurationFile) {

        if (!is.null(configurationFilePath)) {

          file_edit(file = configurationFilePath,
                    wait = TRUE)

        } else {
          file_edit(file = system.file("configurations",
                                       "configurationsTemplate.ini",
                                       package = "bdpar"),
                    wait = TRUE)
        }
      } else {

        if (is.null(configurationFilePath)) {

          configurationFilePath <- system.file("configurations",
                                               "configurationsTemplate.ini",
                                               package = "bdpar")
        }
      }

      Bdpar[["private_fields"]][["configurationFilePath"]] <- configurationFilePath
      Bdpar[["private_fields"]][["connections"]] <- Connections$new(configurationFilePath)
      Bdpar[["private_fields"]][["resourceHandler"]] <- ResourceHandler$new()

    },

    proccess_files = function(path,
                              pipe = SerialPipe$new(),
                              instanceFactory = InstanceFactory$new()) {

      if (!"TypePipe" %in% class(pipe)) {
        stop("[Bdpar][proccess_files][Error]
                Checking the type of the variable: pipe ",
                  class(pipe))
      }

      if (!"InstanceFactory" %in% class(instanceFactory)) {
        stop("[Bdpar][proccess_files][Error]
                Checking the type of the variable: instanceFactory ",
                  class(instanceFactory))
      }

      if (!"character" %in% class(path)) {
        stop("[Bdpar][proccess_files][Error]
                Checking the type of the variable: path ",
                  class(path))
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
    #Path where the file with keys are located.
    configurationFilePath = NULL,
    #Object that handles the json resources files.
    resourceHandler = NULL
  )
)
