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

#' @title Class to handle a CSV with the properties field of the preprocessed Instance
#'
#' @description Complete a CSV with the properties of the preprocessed \code{\link{Instance}}.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \preformatted{
#' TeeCSVPipe$new(propertyName = "",
#'                alwaysBeforeDeps = list(),
#'                notAfterDeps = list(),
#'                withData = TRUE,
#'                withSource = TRUE)
#' }
#' \itemize{
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{propertyName:}}{
#' (\emph{character}) name of the property associated with the Pipe.
#' }
#' \item{\strong{alwaysBeforeDeps:}}{
#' (\emph{list}) the dependences alwaysBefore (Pipes that must be executed before this
#' one).
#' }
#' \item{\strong{notAfterDeps:}}{
#' (\emph{list}) the dependences notAfter (Pipes that cannot be executed after this one).
#' }
#' \item{\strong{withData:}}{
#' (\emph{logical}) indicates if the data is added to CSV.
#' }
#' \item{\strong{withSource:}}{
#' (\emph{logical}) indicates if the source is added to CSV.
#' }
#' }
#' }
#' }
#'
#' @section Details:
#' The path to save the properties should be defined in the
#' \strong{"teeCSVPipe.output.path"} field of
#' \emph{\link{bdpar.Options}} variable.
#'
#' @section Inherit:
#' This class inherits from \code{\link{GenericPipe}} and implements the
#' \code{pipe} abstract function.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe:}}{
#' completes the CSV with the preprocessed \code{\link{Instance}}.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{pipe(instance)}
#' }
#' \item{\emph{Value:}}{
#' the \code{\link{Instance}} with the modifications that have occurred in the Pipe.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{instance:}}{
#' (\emph{Instance}) \code{\link{Instance}} to preproccess.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{withSource:}}{
#'  (\emph{logical}) indicates if the source is added to CSV.
#' }
#' \item{\bold{withData:}}{
#'  (\emph{logical}) indicates if the data is added to CSV.
#' }
#' }
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
#' @import R6 tools utils
#' @export TeeCSVPipe

TeeCSVPipe <- R6Class(

  "TeeCSVPipe",

  inherit = GenericPipe,

  public = list(

    initialize = function(propertyName = "",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list(),
                          withData = TRUE,
                          withSource = TRUE,
                          outputPath = NULL) {

      if (!"character" %in% class(propertyName)) {
        stop("[TeeCSVPipe][initialize][Error] ",
             "Checking the type of the 'propertyName' variable: ",
             class(propertyName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[TeeCSVPipe][initialize][Error] ",
             "Checking the type of the 'alwaysBeforeDeps' variable: ",
             class(alwaysBeforeDeps))
      }

      if (!"list" %in% class(notAfterDeps)) {
        stop("[TeeCSVPipe][initialize][Error] ",
             "Checking the type of the 'notAfterDeps' variable: ",
             class(notAfterDeps))
      }

      if (!"logical" %in% class(withSource)) {
        stop("[TeeCSVPipe][initialize][Error] ",
             "Checking the type of the 'withSource' variable: ",
             class(withSource))
      }

      if (!"logical" %in% class(withData)) {
        stop("[TeeCSVPipe][initialize][Error] ",
             "Checking the type of the 'withData' variable: ",
             class(withData))
      }

      if (is.null(outputPath)) {
        if (!all(bdpar.Options$isSpecificOption("teeCSVPipe.output.path"),
                 !is.null(bdpar.Options$get("teeCSVPipe.output.path")))) {
          stop("[TeeCSVPipe][initialize][Error] Path of TeeCSVPipe output ",
               "is neither defined in initialize or in bdpar.Options")
        } else {
          outputPath <- bdpar.Options$get("teeCSVPipe.output.path")
        }
      }

      if (!"character" %in% class(outputPath)) {
        stop("[TeeCSVPipe][initialize][Error] ",
             "Checking the type of the 'outputPath' variable: ",
             class(outputPath))
      }

      if (!"csv" %in% file_ext(outputPath)) {
        stop("[TeeCSVPipe][initialize][Error] ",
             "Checking the extension of the file: ",
             file_ext(outputPath))
      }

      private$outputPath <- outputPath


      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)

      private$withSource <- withSource
      private$withData <- withData

    },

    pipe = function(instance) {

      if (!"Instance" %in% class(instance)) {
        stop("[TeeCSVPipe][pipe][Error] ",
             "Checking the type of the 'instance' variable: ",
             class(instance))
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

      return(instance)
    }
  ),

  private = list(
    withSource = TRUE,
    withData = TRUE,
    outputPath = ""
  )
)
