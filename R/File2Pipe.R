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

#' @title Class to obtain the source field of an Instance
#'
#' @description Obtains the \strong{source} using the method which implements the
#' subclass of \code{\link{Instance}}.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \preformatted{
#' File2Pipe$new(propertyName = "source",
#'               alwaysBeforeDeps = list("TargetAssigningPipe"),
#'               notAfterDeps = list())
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
#' }
#' }
#' }
#'
#' @section Note:
#' \code{\link{File2Pipe}} will automatically invalidate the
#' \code{\link{Instance}} whenever the obtained source is empty or not in UTF-8 format.
#'
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe:}}{
#' preprocesses the \code{\link{Instance}} to obtain the source.
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
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{ContractionPipe}},
#'          \code{\link{FindEmojiPipe}}, \code{\link{FindEmoticonPipe}},
#'          \code{\link{FindHashtagPipe}}, \code{\link{FindUrlPipe}},
#'          \code{\link{FindUserNamePipe}}, \code{\link{GuessDatePipe}},
#'          \code{\link{GuessLanguagePipe}}, \code{\link{Instance}},
#'          \code{\link{InterjectionPipe}}, \code{\link{MeasureLengthPipe}},
#'          \code{\link{PipeGeneric}}, \code{\link{SlangPipe}},
#'          \code{\link{StopWordPipe}}, \code{\link{StoreFileExtPipe}},
#'          \code{\link{TargetAssigningPipe}}, \code{\link{TeeCSVPipe}},
#'          \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export File2Pipe

File2Pipe <- R6Class(

  "File2Pipe",

  inherit = PipeGeneric,

  public = list(

    initialize = function(propertyName = "source",
                          alwaysBeforeDeps = list("TargetAssigningPipe"),
                          notAfterDeps = list()) {

      if (!"character" %in% class(propertyName)) {
        stop("[File2Pipe][initialize][Error] ",
             "Checking the type of the 'propertyName' variable: ",
             class(propertyName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[File2Pipe][initialize][Error] ",
             "Checking the type of the 'alwaysBeforeDeps' variable: ",
             class(alwaysBeforeDeps))
      }

      if (!"list" %in% class(notAfterDeps)) {
        stop("[File2Pipe][initialize][Error] ",
             "Checking the type of the 'notAfterDeps' variable: ",
             class(notAfterDeps))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },

    pipe = function(instance){

      if (!"Instance" %in% class(instance)) {
        stop("[File2Pipe][pipe][Error] ",
             "Checking the type of the 'instance' variable: ",
             class(instance))
      }

      instance$addFlowPipes("File2Pipe")

      if (!instance$checkCompatibility("File2Pipe", self$getAlwaysBeforeDeps())) {
        stop("[File2Pipe][pipe][Error] Bad compatibility between Pipes")
      }

      instance$addBanPipes(unlist(super$getNotAfterDeps()))

      instance$obtainSource()

      if (is.na(instance$getSource()) || all(instance$getSource() == "") || is.null(instance$getSource())) {
        message <- c( "The file: " , instance$getPath() , " has source empty")

        instance$addProperties(message, "reasonToInvalidate")

        warning("[File2Pipe][pipe][Warning] ", message)

        instance$invalidate()

        return(instance)
      }

      if (!validUTF8(instance$getSource())) {
        message <- c( "The file: " , instance$getPath() , " is not utf8")

        instance$addProperties(message, "reasonToInvalidate")

        warning("[File2Pipe][pipe][Warning] ", message)

        instance$invalidate()

        return(instance)
      }

      return(instance)
    }
  )
)
