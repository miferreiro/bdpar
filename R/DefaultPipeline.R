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

#' @title Class implementing a default pipelining proccess.
#'
#' @description This \code{\link{DefaultPipeline}} class inherits from the
#' \code{\link{GenericPipeline}} class. Includes the \strong{execute} method which
#' provides a default pipelining implementation.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \code{DefaultPipeline$new()}
#'
#' @section Details:
#' The default flow is:
#'
#' \preformatted{
#' instance \%>I\%
#'
#'   TargetAssigningPipe$new() \%>I\%
#'
#'   StoreFileExtPipe$new() \%>I\%
#'
#'   GuessDatePipe$new() \%>I\%
#'
#'   File2Pipe$new() \%>I\%
#'
#'   MeasureLengthPipe$new(propertyName = "length_before_cleaning_text") \%>I\%
#'
#'   FindUserNamePipe$new() \%>I\%
#'
#'   FindHashtagPipe$new() \%>I\%
#'
#'   FindUrlPipe$new() \%>I\%
#'
#'   FindEmoticonPipe$new() \%>I\%
#'
#'   FindEmojiPipe$new() \%>I\%
#'
#'   GuessLanguagePipe$new() \%>I\%
#'
#'   ContractionPipe$new() \%>I\%
#'
#'   AbbreviationPipe$new() \%>I\%
#'
#'   SlangPipe$new() \%>I\%
#'
#'   ToLowerCasePipe$new() \%>I\%
#'
#'   InterjectionPipe$new() \%>I\%
#'
#'   StopWordPipe$new() \%>I\%
#'
#'   MeasureLengthPipe$new(propertyName = "length_after_cleaning_text") \%>I\%
#'
#'   TeeCSVPipe$new()
#' }
#'
#' @section Inherit:
#' This class inherits from \code{\link{GenericPipeline}} and implements the
#' \code{execute} abstract function.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{execute:}}{
#' function where is implemented the flow of the pipes.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{execute(instance)}
#' }
#' \item{\emph{Value:}}{
#' the preprocessed \code{\link{Instance}}.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{instance:}}{
#' (\emph{Instance}) the \code{\link{Instance}} that is going to be processed.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{get:}}{
#' gets a list with containinig the set of pipes of the pipeline,
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{get()}
#' }
#' \item{\emph{Value:}}{
#' the set of pipes containing the pipeline.
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{Instance}}, \code{\link{DynamicPipeline}},
#'          \code{\link{GenericPipeline}}, \code{\link{GenericPipe}},
#'          \code{\link{\%>I\%}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export DefaultPipeline

DefaultPipeline <- R6Class(

  "DefaultPipeline",

  inherit = GenericPipeline,

  public = list(

    initialize = function() { },

    execute = function(instance) {

      if (!"Instance" %in% class(instance)) {
        stop("[DefaultPipeline][execute][Error] ",
             "Checking the type of the 'instance' variable: ",
             class(instance))
      }

      message("[DefaultPipeline][execute][Info] ", instance$getPath())

      tryCatch(
        instance %>I%
          TargetAssigningPipe$new() %>I%
          StoreFileExtPipe$new() %>I%
          GuessDatePipe$new() %>I%
          File2Pipe$new() %>I%
          MeasureLengthPipe$new(propertyName = "length_before_cleaning_text") %>I%
          FindUserNamePipe$new() %>I%
          FindHashtagPipe$new() %>I%
          FindUrlPipe$new() %>I%
          FindEmoticonPipe$new() %>I%
          FindEmojiPipe$new() %>I%
          GuessLanguagePipe$new() %>I%
          ContractionPipe$new() %>I%
          AbbreviationPipe$new() %>I%
          SlangPipe$new() %>I%
          ToLowerCasePipe$new() %>I%
          InterjectionPipe$new() %>I%
          StopWordPipe$new() %>I%
          MeasureLengthPipe$new(propertyName = "length_after_cleaning_text") %>I%
          TeeCSVPipe$new()
        ,
        error = function(e) {
          message("[DefaultPipeline][execute][Error]", instance$getPath()," :", paste(e))
          instance$invalidate()
        }
      )
      return(instance)
    },

    get = function() {
      list(TargetAssigningPipe$new(),
           StoreFileExtPipe$new(),
           GuessDatePipe$new(),
           File2Pipe$new(),
           MeasureLengthPipe$new(propertyName = "length_before_cleaning_text"),
           FindUserNamePipe$new(),
           FindHashtagPipe$new(),
           FindUrlPipe$new(),
           FindEmoticonPipe$new(),
           FindEmojiPipe$new(),
           GuessLanguagePipe$new(),
           ContractionPipe$new(),
           AbbreviationPipe$new(),
           SlangPipe$new(),
           ToLowerCasePipe$new(),
           InterjectionPipe$new(),
           StopWordPipe$new(),
           MeasureLengthPipe$new(propertyName = "length_after_cleaning_text"),
           TeeCSVPipe$new())
    },

    print = function(...) {
      cat("instance %>I%
          TargetAssigningPipe$new() %>I%
          StoreFileExtPipe$new() %>I%
          GuessDatePipe$new() %>I%
          File2Pipe$new() %>I%
          MeasureLengthPipe$new(propertyName = \"length_before_cleaning_text\") %>I%
          FindUserNamePipe$new() %>I%
          FindHashtagPipe$new() %>I%
          FindUrlPipe$new() %>I%
          FindEmoticonPipe$new() %>I%
          FindEmojiPipe$new() %>I%
          GuessLanguagePipe$new() %>I%
          ContractionPipe$new() %>I%
          AbbreviationPipe$new() %>I%
          SlangPipe$new() %>I%
          ToLowerCasePipe$new() %>I%
          InterjectionPipe$new() %>I%
          StopWordPipe$new() %>I%
          MeasureLengthPipe$new(propertyName = \"length_after_cleaning_text\") %>I%
          TeeCSVPipe$new()")
    }
  )
)
