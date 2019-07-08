#
# Bdpa4r provide a tool to easily build customized data flows to pre-process
# large volumes of information from different sources. To this end, bdpa4R allows
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
#'   TargetAssigningPipe$new()$pipe() \%>I\%
#'
#'   StoreFileExtPipe$new()$pipe() \%>I\%
#'
#'   GuessDatePipe$new()$pipe() \%>I\%
#'
#'   File2Pipe$new()$pipe() \%>I\%
#'
#'   MeasureLengthPipe$new()$pipe("length_before_cleaning_text") \%>I\%
#'
#'   FindUserNamePipe$new()$pipe() \%>I\%
#'
#'   FindHashtagPipe$new()$pipe() \%>I\%
#'
#'   FindUrlPipe$new()$pipe() \%>I\%
#'
#'   FindEmoticonPipe$new()$pipe() \%>I\%
#'
#'   FindEmojiPipe$new()$pipe() \%>I\%
#'
#'   GuessLanguagePipe$new()$pipe() \%>I\%
#'
#'   ContractionPipe$new()$pipe() \%>I\%
#'
#'   AbbreviationPipe$new()$pipe() \%>I\%
#'
#'   SlangPipe$new()$pipe() \%>I\%
#'
#'   ToLowerCasePipe$new()$pipe() \%>I\%
#'
#'   InterjectionPipe$new()$pipe() \%>I\%
#'
#'   StopWordPipe$new()$pipe() \%>I\%
#'
#'   MeasureLengthPipe$new()$pipe("length_after_cleaning_text") \%>I\%
#'
#'   TeeCSVPipe$new()$pipe()
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
        TargetAssigningPipe$new()$pipe() %>I%
        StoreFileExtPipe$new()$pipe() %>I%
        GuessDatePipe$new()$pipe() %>I%
        File2Pipe$new()$pipe() %>I%
        MeasureLengthPipe$new()$pipe("length_before_cleaning_text") %>I%
        FindUserNamePipe$new()$pipe() %>I%
        FindHashtagPipe$new()$pipe() %>I%
        FindUrlPipe$new()$pipe() %>I%
        FindEmoticonPipe$new()$pipe() %>I%
        FindEmojiPipe$new()$pipe() %>I%
        GuessLanguagePipe$new()$pipe() %>I%
        ContractionPipe$new()$pipe() %>I%
        AbbreviationPipe$new()$pipe() %>I%
        SlangPipe$new()$pipe() %>I%
        ToLowerCasePipe$new()$pipe() %>I%
        InterjectionPipe$new()$pipe() %>I%
        StopWordPipe$new()$pipe() %>I%
        MeasureLengthPipe$new()$pipe("length_after_cleaning_text") %>I%
        TeeCSVPipe$new()$pipe()

      return(instance)
    }
  )
)
