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

#' @title Class to find and/or replace the emoji on the data field of an Instance
#'
#' @description This class is responsible of detecting the existing emojis in the
#' \strong{data} field of each \code{\link{Instance}}. Identified emojis are
#' stored inside the \strong{emoji} field of \code{\link{Instance}} class.
#' Moreover if required, is able to perform inline emoji replacement.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \preformatted{
#' FindEmojiPipe$new(propertyName = "emoji",
#'                   alwaysBeforeDeps = list(),
#'                   notAfterDeps = list())
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
#' \code{\link{FindEmojiPipe}} use the emoji list provided by rtweet package.
#'
#' @section Note:
#' \code{\link{FindEmojiPipe}} will automatically invalidate the
#' \code{\link{Instance}} whenever the obtained data is empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe:}}{
#' preprocesses the \code{\link{Instance}} to obtain/replace the emojis.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{pipe(instance, replaceEmoji = TRUE)}
#' }
#' \item{\emph{Value:}}{
#' the \code{\link{Instance}} with the modifications that have occurred in the Pipe.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{instance:}}{
#' (\emph{Instance}) \code{\link{Instance}} to preproccess.
#' }
#' \item{\strong{replaceEmoji:}}{
#' (\emph{logical}) indicates if the emojis are replaced.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{findEmoji:}}{
#' checks for the existence of an specific emoji.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{findEmoji(data, emoji)}
#' }
#' \item{\emph{Value:}}{
#' boolean, depending on whether the emoji is on the data.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{data:}}{
#' (\emph{character}) text to search the emoji.
#' }
#' \item{\strong{emoji:}}{
#' (\emph{character}) indicates the emoji to find.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{replaceEmoji:}}{
#' replaces the emoji in the data for the extendedEmoji.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{replaceEmoji(emoji, extendedEmoji, data)}
#' }
#' \item{\emph{Value:}}{
#' the data with emoji replaced.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{emoji:}}{
#' (\emph{character}) indicates the emoji to remove.
#' }
#' \item{\strong{extendedEmoji:}}{
#' (\emph{character}) determines the text source to replace the emoji found.
#' }
#' \item{\strong{data:}}{
#' (\emph{character}) text where emojis will be replaced.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{ContractionPipe}},
#'          \code{\link{File2Pipe}}, \code{\link{FindEmoticonPipe}},
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
#' @import pipeR R6 rlist
#' @export FindEmojiPipe

FindEmojiPipe <- R6Class(

  "FindEmojiPipe",

  inherit = PipeGeneric,

  public = list(

    initialize = function(propertyName = "Emojis",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list()) {

      if (!requireNamespace("rex", quietly = TRUE)) {
        stop("[FindEmojiPipe][initialize][Error]
                Package \"rex\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!requireNamespace("textutils", quietly = TRUE)) {
        stop("[FindEmojiPipe][initialize][Error]
                Package \"textutils\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!requireNamespace("rtweet", quietly = TRUE)) {
        stop("[FindEmojiPipe][initialize][Error]
                Package \"rtweet\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!"character" %in% class(propertyName)) {
        stop("[FindEmojiPipe][initialize][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[FindEmojiPipe][initialize][Error]
                Checking the type of the variable: alwaysBeforeDeps ",
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[FindEmojiPipe][initialize][Error]
                Checking the type of the variable: notAfterDeps ",
                  class(notAfterDeps))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)

    },

    pipe = function(instance, replaceEmoji = TRUE) {

      if (!"Instance" %in% class(instance)) {
        stop("[FindEmojiPipe][pipe][Error]
                Checking the type of the variable: instance ",
                  class(instance))
      }

      if (!"logical" %in% class(replaceEmoji)) {
        stop("[FindEmojiPipe][pipe][Error]
                Checking the type of the variable: replaceEmoji ",
                  class(replaceEmoji))
      }

      instance$addFlowPipes("FindEmojiPipe")

      if (!instance$checkCompatibility("FindEmojiPipe", self$getAlwaysBeforeDeps())) {
        stop("[FindEmojiPipe][pipe][Error] Bad compatibility between Pipes.")
      }

      instance$addBanPipes(unlist(super$getNotAfterDeps()))

      emojisLocated <- list()

      emojisList <- as.list(rtweet::emojis[2][[1]])
      names(emojisList) <- as.list(rtweet::emojis[[1]][])

      for (emoji in names(emojisList)) {

        if (self$findEmoji(instance$getData(), emoji)) {

          emojisLocated <- list.append(emojisLocated, emoji)
        }

        if (replaceEmoji && emoji %in% emojisLocated) {

          instance$getData() %>>%
            {self$replaceEmoji(emoji, emojisList[[emoji]], .)} %>>%
              textutils::trim() %>>%
                instance$setData()
        }
      }

      instance$addProperties(paste(emojisLocated),super$getPropertyName())

      if (is.na(instance$getData()) ||
          all(instance$getData() == "") ||
          is.null(instance$getData())) {

        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Emoji")

        instance$addProperties(message, "reasonToInvalidate")

        cat("[FindEmojiPipe][pipe][Warning] ", message, " \n")

        instance$invalidate()

        return(instance)
      }
      return(instance)
    },

    findEmoji = function(data, emoji) {

      if (!"character" %in% class(data)) {
        stop("[FindEmojiPipe][findEmoji][Error]
                Checking the type of the variable: data ",
                  class(data))
      }

      if (!"character" %in% class(emoji)) {
        stop("[FindEmojiPipe][findEmoji][Error]
                Checking the type of the variable: emoji ",
                  class(emoji))
      }

      return(grepl(pattern = rex::escape(emoji), x = data, fixed = TRUE, useBytes = TRUE))

    },

    replaceEmoji = function(emoji, extendedEmoji, data) {

      if (!"character" %in% class(data)) {
        stop("[FindEmojiPipe][replaceEmoji][Error]
                Checking the type of the variable: data ",
                  class(data))
      }

      if (!"character" %in% class(emoji)) {
        stop("[FindEmojiPipe][replaceEmoji][Error]
                Checking the type of the variable: emoji ",
                  class(emoji))
      }

      if (!"character" %in% class(extendedEmoji)) {
        stop("[FindEmojiPipe][replaceEmoji][Error]
                Checking the type of the variable: extendedEmoji ",
                  class(extendedEmoji))
      }

      return(gsub(rex::escape(emoji),
                  paste(" ", extendedEmoji, " ", sep = ""), data, perl = TRUE))
    }
  )
)
