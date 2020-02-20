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

#' @title Class to find and/or remove the emoticons on the data field of an Instance
#'
#' @description This class is responsible of detecting the existing emoticons in the
#' \strong{data} field of each \code{\link{Instance}}. Identified emoticons are
#' stored inside the \strong{emoticon} field of \code{\link{Instance}} class.
#' Moreover if required, is able to perform inline emoticon removement.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \preformatted{
#' FindEmoticonPipe$new(propertyName = "emoticon",
#'                      alwaysBeforeDeps = list(),
#'                      notAfterDeps = list("FindHashtagPipe"),
#'                      removeEmoticons = TRUE)
#' }
#'
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
#' \item{\strong{removeEmoticons:}}{
#' (\emph{logical}) indicates if the emoticons are replaced.
#' }
#' }
#' }
#' }
#'
#' @section Details:
#' The regular expression indicated in the \code{emoticonPattern}
#' variable is used to identify emoticons.
#'
#' @section Note:
#' \code{\link{FindEmoticonPipe}} will automatically invalidate the
#' \code{\link{Instance}} whenever the obtained data is empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{GenericPipe}} and implements the
#' \code{pipe} abstract function.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe:}}{
#' preprocesses the \code{\link{Instance}} to obtain/remove the emoticons.
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
#' \item{\bold{findEmoticon:}}{
#' finds the emoticons in the data.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{findEmoticon(data)}
#' }
#' \item{\emph{Value:}}{
#' list with emoticons found.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{data:}}{
#' (\emph{character}) text to search the emoticons.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{removeEmoticon:}}{
#' removes the emoticons in the data.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{removeEmoticon(data)}
#' }
#' \item{\emph{Value:}}{
#' the data with emoticons removed.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{data:}}{
#' (\emph{character}) text in which emoticons will be removed.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @section Public fields:
#' \itemize{
#' \item{\bold{emoticonPattern:}}{
#'  (\emph{character}) regular expression to detect emoticons.
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{removeEmoticons:}}{
#'  (\emph{logical}) indicates if the emoticons are replaced.
#' }
#' }
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{ContractionPipe}},
#'          \code{\link{File2Pipe}}, \code{\link{FindEmojiPipe}},
#'          \code{\link{FindHashtagPipe}}, \code{\link{FindUrlPipe}},
#'          \code{\link{FindUserNamePipe}}, \code{\link{GuessDatePipe}},
#'          \code{\link{GuessLanguagePipe}}, \code{\link{Instance}},
#'          \code{\link{InterjectionPipe}}, \code{\link{MeasureLengthPipe}},
#'          \code{\link{GenericPipe}}, \code{\link{SlangPipe}},
#'          \code{\link{StopWordPipe}}, \code{\link{StoreFileExtPipe}},
#'          \code{\link{TargetAssigningPipe}}, \code{\link{TeeCSVPipe}},
#'          \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import pipeR R6 rlist
#' @export FindEmoticonPipe

FindEmoticonPipe <- R6Class(

  "FindEmoticonPipe",

  inherit = GenericPipe,

  public = list(

    initialize = function(propertyName = "emoticon",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list("FindHashtagPipe"),
                          removeEmoticons = TRUE) {

      if (!"character" %in% class(propertyName)) {
        stop("[FindEmoticonPipe][initialize][Error] ",
             "Checking the type of the 'propertyName' variable: ",
             class(propertyName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[FindEmoticonPipe][initialize][Error] ",
             "Checking the type of the 'alwaysBeforeDeps' variable: ",
             class(alwaysBeforeDeps))
      }

      if (!"list" %in% class(notAfterDeps)) {
        stop("[FindEmoticonPipe][initialize][Error] ",
             "Checking the type of the 'notAfterDeps' variable: ",
             class(notAfterDeps))
      }

      if (!"logical" %in% class(removeEmoticons)) {
        stop("[FindEmoticonPipe][initialize][Error] ",
             "Checking the type of the 'removeEmoticons' variable: ",
             class(removeEmoticons))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      private$removeEmoticons <- removeEmoticons
    },

    emoticonPattern = '(\\:\\w+\\:|\\<[\\/\\\\]?3|[\\(\\)\\\\\\D|\\*\\$][\\-\\^]?[\\:\\;\\=]|[\\:\\;\\=B8][\\-\\^]?[3DOPp\\@\\$\\*\\\\\\)\\(\\/\\|])(?=\\s|[\\!\\.\\?\\:\\w<>]|$)',

    pipe = function(instance){

      if (!"Instance" %in% class(instance)) {
        stop("[FindEmoticonPipe][pipe][Error] ",
             "Checking the type of the 'instance' variable: ",
             class(instance))
      }

      instance$getData() %>>%
        self$findEmoticon() %>>%
          unique() %>>%
            unlist() %>>%
              {instance$addProperties(.,super$getPropertyName())}

      if (private$removeEmoticons) {
          instance$getData()  %>>%
            self$removeEmoticon() %>>%
              textutils::trim() %>>%
                instance$setData()
      }

      if (is.na(instance$getData()) ||
          all(instance$getData() == "") ||
          is.null(instance$getData())) {

        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Emoticon")

        instance$addProperties(message, "reasonToInvalidate")

        warning("[FindEmoticonPipe][pipe][Warning] ", message)

        instance$invalidate()

        return(instance)
      }

      return(instance)
    },

    findEmoticon = function(data){

      if (!"character" %in% class(data)) {
        stop("[FindEmoticonPipe][findEmoticon][Error] ",
             "Checking the type of the 'data' variable: ",
             class(data))
      }

      return(stringr::str_match_all(data,
                           rex::regex(self$emoticonPattern,
                                 ignore_case = TRUE,
                                 multiline = TRUE))[[1]][,2])
    },

    removeEmoticon = function(data){

      if (!"character" %in% class(data)) {
        stop("[FindEmoticonPipe][removeEmoticon][Error] ",
             "Checking the type of the 'data' variable: ",
             class(data))
      }

      return(stringr::str_replace_all(data,
                             rex::regex(self$emoticonPattern,
                                   ignore_case = TRUE,
                                   multiline = TRUE), " "))
    }
  ),

  private = list(
    removeEmoticons = TRUE
  )
)
