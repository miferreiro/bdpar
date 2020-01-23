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

#' @title Absctract super class implementing the pipelining proccess.
#'
#' @description Class to establish the flow of Pipes.
#'
#' @docType class
#'
#' @section Constructor:
#' \code{TypePipe$new()}
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
#' @section Methods:
#' \itemize{
#' \item{\bold{pipeAll:}}{
#' function where is implemented the flow of the pipes.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{pipeAll(instance)}
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
#' }
#'
#' @seealso \code{\link{Instance}}, \code{\link{SerialPipe}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export TypePipe

TypePipe <- R6Class(

  "TypePipe",

  public = list(

    initialize = function() {

    },

    pipeAll = function(instance) {

      if (!"Instance" %in% class(instance)) {
        stop("[TypePipe][pipeAll][Error]
                Checking the type of the variable: instance ",
                  class(instance));
      }

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

      return(instance)
    }
  )
)
