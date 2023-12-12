#
# Bdpar provide a tool to easily build customized data flows to pre-process
# large volumes of information from different sources. To this end, bdpar allows
# to (i) easily use and create new functionalities and (ii) develop new data
# source extractors according to the user needs. Additionally, the package
# provides by default a predefined data flow to extract and preprocess the most
# relevant information (tokens, dates, ... ) from some textual sources (SMS,
# email, YouTube comments).
#
# Copyright (C) 2020-2022 Sing Group (University of Vigo)
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
#' @section Details:
#' \code{\link{FindEmojiPipe}} use the emoji list provided by data(emojisData).
#'
#' @section Note:
#' \code{\link{FindEmojiPipe}} will automatically invalidate the
#' \code{\link{Instance}} whenever the obtained data is empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{GenericPipe}} and implements the
#' \code{pipe} abstract function.
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{ContractionPipe}},
#'          \code{\link{File2Pipe}}, \code{\link{FindEmoticonPipe}},
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
#' @import R6
#' @export FindEmojiPipe

FindEmojiPipe <- R6Class(

  "FindEmojiPipe",

  inherit = GenericPipe,

  public = list(
    #'
    #' @description Creates a \code{\link{FindEmojiPipe}} object.
    #'
    #' @param propertyName A \code{\link{character}} value. Name of the property
    #' associated with the \code{\link{GenericPipe}}.
    #' @param propertyLanguageName A \code{\link{character}} value. Name of the
    #' language property.
    #' @param alwaysBeforeDeps A \code{\link{list}} value. The dependencies
    #' alwaysBefore (\code{\link{GenericPipe}s} that must be executed before
    #' this one).
    #' @param notAfterDeps A \code{\link{list}} value. The dependencies
    #' notAfter (\code{\link{GenericPipe}s} that cannot be executed after
    #' this one).
    #' @param replaceEmojis A \code{\link{logical}} value. Indicates if the
    #' emojis are replaced.
    #'
    initialize = function(propertyName = "Emojis",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list(),
                          replaceEmojis = TRUE) {

      if (!"character" %in% class(propertyName)) {
        bdpar.log(message = paste0("Checking the type of the 'propertyName' variable: ",
                                   class(propertyName)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        bdpar.log(message = paste0("Checking the type of the 'alwaysBeforeDeps' variable: ",
                                   class(alwaysBeforeDeps)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }
      if (!"list" %in% class(notAfterDeps)) {
        bdpar.log(message = paste0("Checking the type of the 'notAfterDeps' variable: ",
                                   class(notAfterDeps)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      if (!"logical" %in% class(replaceEmojis)) {
        bdpar.log(message = paste0("Checking the type of the 'replaceEmojis' variable: ",
                                   class(replaceEmojis)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      private$replaceEmojis <- replaceEmojis
    },
    #'
    #' @description Preprocesses the \code{\link{Instance}} to obtain/replace
    #' the emojis. The emojis found in the data are added to the
    #' list of properties of the \code{\link{Instance}}.
    #'
    #' @param instance A \code{\link{Instance}} value. The \code{\link{Instance}}
    #' to preprocess.
    #'
    #' @return The \code{\link{Instance}} with the modifications that have
    #' occurred in the pipe.
    #'
    #' @import rlist
    #'
    pipe = function(instance) {

      if (!"Instance" %in% class(instance)) {
        bdpar.log(message = paste0("Checking the type of the 'instance' variable: ",
                                   class(instance)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "pipe")
      }

      emojisLocated <- list()

      data(emojisData, package = "bdpar", envir = environment())

      emojisList <- as.list(emojisData[2][[1]])
      names(emojisList) <- as.list(emojisData[[1]][])

      for (emoji in names(emojisList)) {

        if (self$findEmoji(instance$getData(), emoji)) {

          emojisLocated <- list.append(emojisLocated, emoji)
        }

        if (private$replaceEmojis && emoji %in% emojisLocated) {
          instance$setData(trimws(x = self$replaceEmoji(emoji,
                                                        emojisList[[emoji]],
                                                        instance$getData())))
        }
      }

      instance$addProperties(paste(emojisLocated),super$getPropertyName())

      instance
    },
    #'
    #' @description Checks if the emoji is in the data.
    #'
    #' @param data A \code{\link{character}} value. The text where emoji
    #' will be searched.
    #' @param emoji A \code{\link{character}} value. Indicates the
    #' emoji to find.
    #'
    #' @return A \code{\link{logical}} value depending on whether the
    #' emoji is in the data.
    #'
    findEmoji = function(data, emoji) {

      if (!"character" %in% class(data)) {
        bdpar.log(message = paste0("Checking the type of the 'data' variable: ",
                                   class(data)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "findEmoji")
      }

      if (!"character" %in% class(emoji)) {
        bdpar.log(message = paste0("Checking the type of the 'emoji' variable: ",
                                   class(emoji)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "findEmoji")
      }

      grepl(pattern = rex::escape(emoji),
            x = data,
            fixed = TRUE,
            useBytes = TRUE)
    },
    #'
    #' @description Replaces the \emph{emoji} in the data for the
    #' \emph{extendedEmoji}.
    #'
    #' @param emoji A \code{\link{character}} value. Indicates the
    #' emoji to replace.
    #' @param extendedEmoji A \code{\link{character}} value. Indicates the
    #' string to replace for the emojis found.
    #' @param data A \code{\link{character}} value. The text where emoji
    #' will be replaced.
    #'
    #' @return The data with the emojis replaced.
    #'
    replaceEmoji = function(emoji, extendedEmoji, data) {

      if (!"character" %in% class(data)) {
        bdpar.log(message = paste0("Checking the type of the 'data' variable: ",
                                   class(data)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "replaceEmoji")
      }

      if (!"character" %in% class(emoji)) {
        bdpar.log(message = paste0("Checking the type of the 'emoji' variable: ",
                                   class(emoji)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "replaceEmoji")
      }

      if (!"character" %in% class(extendedEmoji)) {
        bdpar.log(message = paste0("Checking the type of the 'extendedEmoji' variable: ",
                                   class(extendedEmoji)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "replaceEmoji")
      }

      gsub(rex::escape(emoji),
           paste(" ", extendedEmoji, " ", sep = ""),
           data,
           perl = TRUE)
    }
  ),

  private = list(
    # A (\emph{logical}) value. Indicates if the emojis are replaced or
    # not.
    replaceEmojis = TRUE
  )
)
