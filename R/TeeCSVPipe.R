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

#' @title Class to handle a CSV with the properties field of the preprocessed
#' Instance
#'
#' @description Complete a CSV with the properties of the preprocessed
#' \code{\link{Instance}}.
#'
#' @section Details:
#' The path to save the properties should be defined in the
#' \strong{"teeCSVPipe.output.path"} field of \emph{\link{bdpar.Options}}
#' variable.
#'
#' @section Inherit:
#' This class inherits from \code{\link{GenericPipe}} and implements the
#' \code{pipe} abstract function.
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{bdpar.Options}},
#'          \code{\link{ContractionPipe}}, \code{\link{File2Pipe}},
#'          \code{\link{FindEmojiPipe}}, \code{\link{FindEmoticonPipe}},
#'          \code{\link{FindHashtagPipe}}, \code{\link{FindUrlPipe}},
#'          \code{\link{FindUserNamePipe}}, \code{\link{GuessDatePipe}},
#'          \code{\link{GuessLanguagePipe}}, \code{\link{Instance}},
#'          \code{\link{InterjectionPipe}}, \code{\link{MeasureLengthPipe}},
#'          \code{\link{GenericPipe}}, \code{\link{ResourceHandler}},
#'          \code{\link{SlangPipe}}, \code{\link{StopWordPipe}},
#'          \code{\link{StoreFileExtPipe}}, \code{\link{TargetAssigningPipe}},
#'          \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export TeeCSVPipe

TeeCSVPipe <- R6Class(

  "TeeCSVPipe",

  inherit = GenericPipe,

  public = list(
    #'
    #' @description Creates a \code{\link{TeeCSVPipe}} object.
    #'
    #' @param propertyName A \code{\link{character}} value. Name of the property
    #' associated with the \code{\link{GenericPipe}}.
    #' @param alwaysBeforeDeps A \code{\link{list}} value. The dependencies
    #' alwaysBefore (\code{\link{GenericPipe}s} that must be executed before
    #' this one).
    #' @param notAfterDeps A \code{\link{list}} value. The dependencies
    #' notAfter (\code{\link{GenericPipe}s} that cannot be executed after
    #' this one).
    #' @param withData A \code{\link{logical}} value. Indicates if the data is
    #' added to CSV.
    #' @param withSource A \code{\link{logical}} value. Indicates if the source
    #' is added to CSV.
    #' @param outputPath A \code{\link{character}} value. The path of CSV.
    #'
    #' @import tools
    #'
    initialize = function(propertyName = "",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list(),
                          withData = TRUE,
                          withSource = TRUE,
                          outputPath = NULL) {

      if (!"character" %in% class(propertyName)) {
        bdpar.log(message = paste0("Checking the type of the 'propertyName' variable: ",
                                   class(propertyName)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        bdpar.log(message = paste0("Checking the type of the 'alwaysBeforeDeps' variable: ",
                                   class(alwaysBeforeDeps)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      if (!"list" %in% class(notAfterDeps)) {
        bdpar.log(message = paste0("Checking the type of the 'notAfterDeps' variable: ",
                                   class(notAfterDeps)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      if (!"logical" %in% class(withSource)) {
        bdpar.log(message = paste0("Checking the type of the 'withSource' variable: ",
                                   class(withSource)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      if (!"logical" %in% class(withData)) {
        bdpar.log(message = paste0("Checking the type of the 'withData' variable: ",
                                   class(withData)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      if (is.null(outputPath)) {
        if (!bdpar.Options$isSpecificOption("teeCSVPipe.output.path") ||
            is.null(bdpar.Options$get("teeCSVPipe.output.path"))) {
          bdpar.log(message = paste0("Path of TeeCSVPipe output is neither ",
                                     "defined in initialize or in bdpar.Options"),
                    level = "FATAL",
                    className = class(self)[1],
                    methodName = "initialize")

        } else {
          outputPath <- bdpar.Options$get("teeCSVPipe.output.path")
        }
      }

      if (!"character" %in% class(outputPath)) {
        bdpar.log(message = paste0("Checking the type of the 'outputPath' variable: ",
                                   class(outputPath)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      if (!"csv" %in% file_ext(outputPath)) {
        bdpar.log(message = paste0("Checking the extension of the file: ",
                                   file_ext(outputPath)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      private$outputPath <- outputPath


      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)

      private$withSource <- withSource
      private$withData <- withData
    },
    #'
    #' @description Completes the CSV with the preprocessed
    #' \code{\link{Instance}}.
    #'
    #' @param instance A \code{\link{Instance}} value. The \code{\link{Instance}}
    #' to preprocess.
    #'
    #' @return The \code{\link{Instance}} with the modifications that have
    #' occurred in the pipe.
    #'
    #' @importFrom utils read.csv

    pipe = function(instance) {

      if (!"Instance" %in% class(instance)) {
        bdpar.log(message = paste0("Checking the type of the 'instance' variable: ",
                                   class(instance)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "pipe")
      }

      if (!instance$isInstanceValid()) {
        return(instance)
      }

      if (file.exists(private$outputPath)) {
        dataFrameAll <- read.csv(file = private$outputPath, header = TRUE,
                                 sep = ";", dec = ".", fill = FALSE, stringsAsFactors = FALSE)
      } else {
        dataFrameAll <- data.frame()
      }

      pos <- dim(dataFrameAll)[1] + 1

      dataFrameAll[pos, "path"] <- instance$getPath()

      if (private$withData) {
        dataFrameAll[pos, "data"] <- instance$getData()
      }

      if (private$withSource) {
        dataFrameAll[pos, "source"] <-
          as.character(paste0(unlist(instance$getSource())))
      }

      dataFrameAll[pos, "date"] <- instance$getDate()

      namesPropertiesList <- as.list(instance$getNamesOfProperties())
      names(namesPropertiesList) <- instance$getNamesOfProperties()

      for (name in namesPropertiesList) {
        dataFrameAll[pos, name] <-
          paste0(unlist(instance$getSpecificProperty(name)), collapse = "|")
      }

      write.table(x = dataFrameAll,
                  file = private$outputPath,
                  sep = ";",
                  dec = ".",
                  quote = TRUE,
                  col.names = TRUE,
                  row.names = FALSE,
                  qmethod = c("double"),
                  fileEncoding = "UTF-8")

      instance
    }
  ),

  private = list(
    # A (\emph{logical}) value. Indicates if the source is added to CSV.
    withSource = TRUE,
    # A (\emph{logical}) value. Indicates if the data is added to CSV.
    withData = TRUE,
    # A (\emph{character}) value. The path of CSV.
    outputPath = ""
  )
)
