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
#'                notAfterDeps = list())
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
#' }
#' }
#' }
#'
#' @section Details:
#' The path to save the properties have to indicate in the configuration file.
#'
#' The way to indicate is the following:
#'
#' \strong{[CSVPath]}
#'
#' outPutTeeCSVPipePath = <<out_put_TeeCSVPipe_path>>
#'
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe:}}{
#' completes the CSV with the preprocessed \code{\link{Instance}}.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{pipe(instance, withData = TRUE, withSource = TRUE)}
#' }
#' \item{\emph{Value:}}{
#' the \code{\link{Instance}} with the modifications that have occurred in the Pipe.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{instance:}}{
#' (\emph{Instance}) \code{\link{Instance}} to preproccess.
#' }
#' \item{\strong{withData:}}{
#' (\emph{logical}) indicate if the data is added to CSV.
#' }
#' \item{\strong{withSource:}}{
#' (\emph{logical}) indicate if the source is added to CSV.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{ContractionPipe}},
#'          \code{\link{File2Pipe}}, \code{\link{FindEmojiPipe}},
#'          \code{\link{FindEmoticonPipe}}, \code{\link{FindHashtagPipe}},
#'          \code{\link{FindUrlPipe}}, \code{\link{FindUserNamePipe}},
#'          \code{\link{GuessDatePipe}}, \code{\link{GuessLanguagePipe}},
#'          \code{\link{Instance}}, \code{\link{InterjectionPipe}},
#'          \code{\link{MeasureLengthPipe}}, \code{\link{PipeGeneric}},
#'          \code{\link{ResourceHandler}}, \code{\link{SlangPipe}},
#'          \code{\link{StopWordPipe}}, \code{\link{StoreFileExtPipe}},
#'          \code{\link{TargetAssigningPipe}}, \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import ini R6 tools utils
#' @export TeeCSVPipe

TeeCSVPipe <- R6Class(

  "TeeCSVPipe",

  inherit = PipeGeneric,

  public = list(

    initialize = function(propertyName = "",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list()) {

      if (!"character" %in% class(propertyName)) {
        stop("[TeeCSVPipe][initialize][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[TeeCSVPipe][initialize][Error]
                Checking the type of the variable: alwaysBeforeDeps ",
                  class(alwaysBeforeDeps))
      }

      if (!"list" %in% class(notAfterDeps)) {
        stop("[TeeCSVPipe][initialize][Error]
                Checking the type of the variable: notAfterDeps ",
                  class(notAfterDeps))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },

    pipe = function(instance, withData = TRUE, withSource = TRUE) {

      outPutPath <- read.ini(Bdpar[["private_fields"]][["configurationFilePath"]])$CSVPath$outPutTeeCSVPipePath

      if (!"Instance" %in% class(instance)) {
        stop("[TeeCSVPipe][pipe][Error]
                Checking the type of the variable: instance ",
                  class(instance))
      }

      if (!"logical" %in% class(withSource)) {
        stop("[TeeCSVPipe][pipe][Error]
                Checking the type of the variable: withSource ",
                  class(withSource))
      }

      if (!"logical" %in% class(withData)) {
        stop("[TeeCSVPipe][pipe][Error]
                Checking the type of the variable: withData ",
                  class(withData))
      }

      if (!"character" %in% class(outPutPath)) {
        stop("[TeeCSVPipe][pipe][Error]
                Checking the type of the variable: outPutPath ",
                  class(outPutPath))
      }

      if (!"csv" %in% file_ext(outPutPath)) {
        stop("[TeeCSVPipe][pipe][Error]
                Checking the extension of the file: outPutPath ",
                  file_ext(outPutPath))
      }

      instance$addFlowPipes("TeeCSVPipe")

      if (!instance$checkCompatibility("TeeCSVPipe", self$getAlwaysBeforeDeps())) {
        stop("[TeeCSVPipe][pipe][Error] Bad compatibility between Pipes.")
      }

      instance$addBanPipes(unlist(super$getNotAfterDeps()))

      if (!instance$isInstanceValid()) {
        return(instance)
      }

      if (file.exists(outPutPath)) {
        dataFrameAll <- read.csv(file = outPutPath, header = TRUE,
                                 sep = ";", dec = ".", fill = FALSE, stringsAsFactors = FALSE)
      } else {
        dataFrameAll <- data.frame()
      }

      pos <- dim(dataFrameAll)[1] + 1

      dataFrameAll[pos, "path"] <- instance$getPath()

      if (withData) {
        dataFrameAll[pos, "data"] <- instance$getData()
      }

      if (withSource) {
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
                  file = outPutPath,
                  sep = ";",
                  dec = ".",
                  quote = TRUE,
                  col.names = TRUE,
                  row.names = FALSE,
                  qmethod = c("double"),
                  fileEncoding = "UTF-8")

      return(instance)
    }
  )
)
