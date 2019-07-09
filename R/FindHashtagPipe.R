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

#' @title Class to find and/or remove the hashtags on the data field of an Instance
#'
#' @description This class is responsible of detecting the existing hashtags in the
#' \strong{data} field of each \code{\link{Instance}}. Identified hashtags are
#' stored inside the \strong{hashtag} field of \code{\link{Instance}} class.
#' Moreover if required, is able to perform inline hashtag removement.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \preformatted{
#' FindHashtagPipe$new(propertyName = "hashtag",
#'                     alwaysBeforeDeps = list(),
#'                     notAfterDeps = list())
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
#' }
#' }
#' }
#'
#' @section Details:
#' The regular expression indicated in the \code{hashtagPattern}
#' variable is used to identify hashtags.
#'
#' @section Note:
#' \code{\link{FindHashtagPipe}} will automatically invalidate the
#' \code{\link{Instance}} whenever the obtained data is empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe:}}{
#' preprocesses the \code{\link{Instance}} to obtain/remove the hashtags.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{pipe(instance, removeHashtag = TRUE)}
#' }
#' \item{\emph{Value}}{
#' the \code{\link{Instance}} with the modifications that have occurred in the Pipe.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{instance:}}{
#' (\emph{Instance}) \code{\link{Instance}} to preproccess.
#' }
#' \item{\strong{removeHashtag:}}{
#' (\emph{logical}) indicates if the hashstags are removed
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{findHashtag:}}{
#' finds the hashtags in the data.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{findHashtag(data)}
#' }
#' \item{\emph{Value:}}{
#' list with hashtags found.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{data:}}{
#' (\emph{character}) text to search the hashtags.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{removeHashtag:}}{
#' removes the hashtags in the data.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{removeHashtag(data)}
#' }
#' \item{\emph{Value:}}{
#' the data with hashtags removed.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{data:}}{
#' (\emph{character}) text to remove the hashtags.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @section Public fields:
#' \itemize{
#' \item{\bold{hashtagPattern:}}{
#'  (\emph{character}) regular expression to detect hashtags.
#' }
#' }
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{ContractionPipe}},
#'          \code{\link{File2Pipe}}, \code{\link{FindEmojiPipe}},
#'          \code{\link{FindEmoticonPipe}}, \code{\link{FindUrlPipe}},
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
#' @import pipeR R6 rlist
#' @export FindHashtagPipe

FindHashtagPipe <- R6Class(

  "FindHashtagPipe",

  inherit = PipeGeneric,

  public = list(

    initialize = function(propertyName = "hashtag",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list()) {

      if (!requireNamespace("rex", quietly = TRUE)) {
        stop("[FindHashtagPipe][initialize][Error]
                Package \"rex\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!requireNamespace("textutils", quietly = TRUE)) {
        stop("[FindHashtagPipe][initialize][Error]
                Package \"textutils\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!requireNamespace("stringr", quietly = TRUE)) {
        stop("[FindHashtagPipe][initialize][Error]
                Package \"stringr\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!"character" %in% class(propertyName)) {
        stop("[FindHashtagPipe][initialize][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[FindHashtagPipe][initialize][Error]
                Checking the type of the variable: alwaysBeforeDeps ",
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[FindHashtagPipe][initialize][Error]
                Checking the type of the variable: notAfterDeps ",
                  class(notAfterDeps))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },

    hashtagPattern = "(?:\\s|^|[\"><\u00A1\u00BF?!;:,.'-])(#[^[:cntrl:][:space:]!\"#$%&'()*+\\\\,\\/:;<=>?@\\[\\]^`{|}~.-]+)[;:?\"!,.'>-]?(?=(?:\\s|$|>))",

    pipe = function(instance, removeHashtag = TRUE){

      if (!"Instance" %in% class(instance)) {
        stop("[FindHashtagPipe][pipe][Error]
                Checking the type of the variable: instance ",
                  class(instance))
      }

      if (!"logical" %in% class(removeHashtag)) {
        stop("[FindHashtagPipe][pipe][Error]
                Checking the type of the variable: removeHashtag ",
                  class(removeHashtag))
      }

      instance$addFlowPipes("FindHashtagPipe")

      if (!instance$checkCompatibility("FindHashtagPipe", self$getAlwaysBeforeDeps())) {
        stop("[FindHashtagPipe][pipe][Error] Bad compatibility between Pipes.")
      }

      instance$addBanPipes(unlist(super$getNotAfterDeps()))

      instance$getData() %>>%
        self$findHashtag() %>>%
          unique() %>>%
            unlist() %>>%
              {instance$addProperties(.,super$getPropertyName())}

      if (removeHashtag) {
          instance$getData()  %>>%
            self$removeHashtag() %>>%
              textutils::trim() %>>%
                {instance$setData(.)}
      }

      if (is.na(instance$getData()) ||
          all(instance$getData() == "") ||
          is.null(instance$getData())) {
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Hashtag")

        instance$addProperties(message, "reasonToInvalidate")

        cat("[FindHashtagPipe][pipe][Warning] ", message, " \n")

        instance$invalidate()

        return(instance)
      }

      return(instance);
    },

    findHashtag = function(data){

      if (!"character" %in% class(data)) {
        stop("[FindHashtagPipe][findHashtag][Error]
                Checking the type of the variable: data ",
                  class(data))
      }

      return(stringr::str_match_all(data,
                           rex::regex(self$hashtagPattern,
                                 ignore_case = TRUE,
                                 multiline = TRUE))[[1]][,2])
    },

    removeHashtag = function(data){

      if (!"character" %in% class(data)) {
        stop("[FindHashtagPipe][removeHashtag][Error]
                Checking the type of the variable: data ",
                  class(data))
      }

      return(stringr::str_replace_all(data,
                             rex::regex(self$hashtagPattern,
                                   ignore_case = TRUE,
                                   multiline = TRUE), " "))
    }
  )
)
