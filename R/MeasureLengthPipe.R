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

#' @title Class to obtain the length of the data field of an Instance
#'
#' @description This class is responsible of obtain the length of the\strong{data}
#' field of each \code{\link{Instance}}. Creates the \strong{length} property
#' which indicates the length of the text. The property's name is customize
#' throught the class constructor.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \preformatted{
#' MeasureLengthPipe$new(propertyName = "length",
#'                       alwaysBeforeDeps = list(),
#'                       notAfterDeps = list(),
#'                       nchar_conf = TRUE)
#' }
#' \itemize{
#' \item{\emph{Arguments:}}{
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
#' \item{\strong{nchar_conf}}{
#' (\emph{logical}) indicates if the Pipe uses nchar or object.size.
#' }
#' }
#' }
#' }
#'
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe:}}{
#' preprocesses the \code{\link{Instance}} to obtain the length of data.
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
#'
#' \item{\bold{getLength:}}{
#' obtains the length of the data.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getLength(data, nchar_conf = TRUE)}
#' }
#' \item{\emph{Value:}}{
#' the \code{\link{Instance}} with the modifications that have occurred in the Pipe.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{data:}}{
#' (\emph{character}) text to preproccess.
#' }
#' \item{\strong{nchar_conf:}}{
#' (\emph{logical}) indicates if the Pipe uses nchar or object.size.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{nchar_conf:}}{
#'  (\emph{logical}) indicates if the Pipe uses nchar or object.size.
#' }
#' }
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{ContractionPipe}},
#'          \code{\link{File2Pipe}}, \code{\link{FindEmojiPipe}},
#'          \code{\link{FindEmoticonPipe}}, \code{\link{FindHashtagPipe}},
#'          \code{\link{FindUrlPipe}}, \code{\link{FindUserNamePipe}},
#'          \code{\link{GuessDatePipe}}, \code{\link{GuessLanguagePipe}},
#'          \code{\link{Instance}}, \code{\link{InterjectionPipe}},
#'          \code{\link{PipeGeneric}}, \code{\link{ResourceHandler}},
#'          \code{\link{SlangPipe}}, \code{\link{StopWordPipe}},
#'          \code{\link{StoreFileExtPipe}}, \code{\link{TargetAssigningPipe}},
#'          \code{\link{TeeCSVPipe}}, \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import pipeR R6
#' @importFrom utils object.size
#' @export MeasureLengthPipe

MeasureLengthPipe <- R6Class(

  "MeasureLengthPipe",

  inherit = PipeGeneric,

  public = list(

    initialize = function(propertyName = "length",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list(),
                          nchar_conf = TRUE) {

      if (!"character" %in% class(propertyName)) {
        stop("[MeasureLengthPipe][initialize][Error] ",
             "Checking the type of the 'propertyName' variable: ",
             class(propertyName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[MeasureLengthPipe][initialize][Error] ",
             "Checking the type of the 'alwaysBeforeDeps' variable: ",
             class(alwaysBeforeDeps))
      }

      if (!"list" %in% class(notAfterDeps)) {
        stop("[MeasureLengthPipe][initialize][Error] ",
             "Checking the type of the 'notAfterDeps' variable: ",
             class(notAfterDeps))
      }

      if (!"logical" %in% class(nchar_conf)) {
        stop("[MeasureLengthPipe][initialize][Error] ",
             "Checking the type of the 'nchar_conf' variable: ",
             class(nchar_conf))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      private$nchar_conf <- nchar_conf
    },

    pipe = function(instance) {

        if (!"Instance" %in% class(instance)) {
            stop("[MeasureLengthPipe][pipe][Error] ",
                 "Checking the type of the 'instance' variable: ",
                 class(instance))
        }

        instance$addFlowPipes("MeasureLengthPipe")

        if (!instance$checkCompatibility("MeasureLengthPipe", self$getAlwaysBeforeDeps())) {
          stop("[MeasureLengthPipe][pipe][Error] Bad compatibility between Pipes")
        }

        instance$addBanPipes(unlist(super$getNotAfterDeps()))

        instance$getData() %>>%
          {self$getLength(.,private$nchar_conf)} %>>%
            {instance$addProperties(.,private$propertyName)}

        return(instance);
    },

    getLength = function(data, nchar_conf = TRUE) {

      if (!"character" %in% class(data)) {
        stop("[MeasureLengthPipe][getLength][Error] ",
             "Checking the type of the 'data' variable: ",
             class(data))
      }

      if (!"logical" %in% class(nchar_conf)) {
        stop("[MeasureLengthPipe][getLength][Error] ",
             "Checking the type of the 'nchar_conf' variable: ",
             class(nchar_conf))
      }

      return(ifelse(private$nchar_conf, nchar(data), object.size(data)))
    }
  ),

  private = list(
    nchar_conf = TRUE
  )
)
