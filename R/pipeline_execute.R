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

#' @title Initiates the pipelining process.
#'
#' @description \strong{pipeline_execute} is responsible for easily initialize
#' the pipelining preprocessing proccess.
#'
#' @docType methods
#'
#' @format NULL
#'
#' @usage pipeline_execute(configurationFilePath = NULL, editConfigurationFile = FALSE, filesPath,
#' pipe = SerialPipe$new(), instanceFactory = InstanceFactory$new())
#'
#' @param configurationFilePath (\emph{character}) path where the configuration
#' file is located. The file must have the .ini extension. In the case that the
#' argument is null, the default configuration file (configurationsTemplate.ini)
#' will be used.
#' @param editConfigurationFile (\emph{boolean}) indicates whether a file is
#' opened to modify the configuration file or not.
#' @param filesPath (\emph{character}) path where the files to be preprocessed
#' are located.
#' @param pipe (\emph{TypePipe}) subclass of \code{\link{TypePipe}}, which
#' implements the whole pipeling process.
#' @param instanceFactory (\emph{InstanceFactory}) object implementing
#' the method \code{createInstance} to choose which type of \code{\link{Instance}}
#' is created.
#'
#' @section Details:
#' The configuration file can be indicated by the user or use the default
#' configuration file (configurationsTemplate.ini). In addition, once we call
#' the function, it will be possible to choose if the user
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
#' @section Notes:
#' In the case of choosing to edit the configuration file, the default editor
#' will be opened.
#'
#' @return List of \code{\link{Instance}} that have been preprocessed.
#'
#' @examples
#' \dontrun{
#' #Path where the configuration file are located
#' configurationFilePath <- system.file("configurations",
#'                                      "configurationsExample.ini",
#'                                      package ="bdpar")
#'
#' #Folder with the files to preprocess
#' filesPath <- system.file("testFiles",
#'                           package = "bdpar")
#'
#' #Object which indicates the pipes' flow
#' pipe <- SerialPipe$new()
#'
#' #Object which decides how creates the instances
#' instanceFactory <- InstanceFactory$new()
#'
#' #Starting file preprocessing...
#' pipeline_execute(configurationFilePath = configurationFilePath,
#'                  filesPath = filesPath,
#'                  pipe = pipe,
#'                  instanceFactory = instanceFactory)
#' }
#' @keywords NULL
#' @importFrom svMisc file_edit
#' @importFrom tools file_ext
#' @export pipeline_execute

pipeline_execute = function(configurationFilePath = NULL,
                            editConfigurationFile = FALSE,
                            filesPath,
                            pipe = SerialPipe$new(),
                            instanceFactory = InstanceFactory$new()) {

  if (!is.null(configurationFilePath)) {
    if (!"character" %in% class(configurationFilePath)) {
      stop("[pipeline_execute][Error]
              Checking the type of the variable: configurationFilePath ",
                class(configurationFilePath))
    }

    if (!"ini" %in% tools::file_ext(configurationFilePath)) {
      stop("[pipeline_execute][Error]
              Checking the extension of the file: configurationFilePath ",
                tools::file_ext(configurationFilePath))
    }
  }

  if (!"logical" %in% class(editConfigurationFile)) {
    stop("[pipeline_execute][Error]
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

  if (!"character" %in% class(filesPath)) {
    stop("[pipeline_execute][Error]
            Checking the type of the variable: filesPath ",
              class(filesPath))
  }

  if (!"TypePipe" %in% class(pipe)) {
    stop("[pipeline_execute][Error]
            Checking the type of the variable: pipe ",
              class(pipe))
  }

  if (!"InstanceFactory" %in% class(instanceFactory)) {
    stop("[pipeline_execute][Error]
            Checking the type of the variable: instanceFactory ",
              class(instanceFactory))
  }

  bdpar_object <- Bdpar$new(configurationFilePath)
  bdpar_object$proccess_files(filesPath, pipe, instanceFactory)
}
