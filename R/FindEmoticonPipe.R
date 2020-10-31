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
#' @import R6
#' @export FindEmoticonPipe

FindEmoticonPipe <- R6Class(

  "FindEmoticonPipe",

  inherit = GenericPipe,

  public = list(
    #'
    #' @description Creates a \code{\link{FindEmoticonPipe}} object.
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
    #' @param removeEmoticons A \code{\link{logical}} value. Indicates if the
    #' emoticons are removed.
    #'
    initialize = function(propertyName = "emoticon",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list("FindHashtagPipe"),
                          removeEmoticons = TRUE) {

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

      if (!"logical" %in% class(removeEmoticons)) {
        bdpar.log(message = paste0("Checking the type of the 'removeEmoticons' variable: ",
                                   class(removeEmoticons)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      private$removeEmoticons <- removeEmoticons
    },
    #' @field emoticonPattern  A \code{\link{character}} value. The regular
    #' expression to detect emoticons.
    emoticonPattern = '(\\:\\w+\\:|\\<[\\/\\\\]?3|[\\(\\)\\\\\\D|\\*\\$][\\-\\^]?[\\:\\;\\=]|[\\:\\;\\=B8][\\-\\^]?[3DOPp\\@\\$\\*\\\\\\)\\(\\/\\|])(?=\\s|[\\!\\.\\?\\:\\w<>]|$)',
    #'
    #' @description Preprocesses the \code{\link{Instance}} to obtain/remove
    #' the emoticons. The emoticons found in the data are added to the
    #' list of properties of the \code{\link{Instance}}.
    #'
    #' @param instance A \code{\link{Instance}} value. The \code{\link{Instance}}
    #' to preprocess.
    #'
    #' @return The \code{\link{Instance}} with the modifications that have
    #' occurred in the pipe.
    #'
    #' @import pipeR rlist
    #'
    pipe = function(instance){

      if (!"Instance" %in% class(instance)) {
        bdpar.log(message = paste0("Checking the type of the 'instance' variable: ",
                                   class(instance)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "pipe")
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

        message <- c("The file: ", instance$getPath(), " has data empty on pipe Emoticon")

        instance$addProperties(message, "reasonToInvalidate")

        bdpar.log(message = message,
                  level = "WARN",
                  className = class(self)[1],
                  methodName = "pipe")

        instance$invalidate()

        return(instance)
      }

      instance
    },
    #'
    #' @description Finds the \emph{emoticons} in the data.
    #'
    #' @param data A \code{\link{character}} value. The text to search the
    #' emoticons.
    #'
    #' @return The \code{\link{list}} with emoticons found.
    #'
    findEmoticon = function(data){

      if (!"character" %in% class(data)) {
        bdpar.log(message = paste0("Checking the type of the 'data' variable: ",
                                   class(data)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "findEmoticon")
      }

      stringr::str_match_all(data,
                             rex::regex(self$emoticonPattern,
                                        ignore_case = TRUE,
                                        multiline = TRUE))[[1]][,2]
    },
    #'
    #' @description Removes the \emph{emoticons} in the data.
    #'
    #' @param data A \code{\link{character}} value. The text where emoticons
    #' will be removed.
    #'
    #' @return The data with the emoticons removed.
    #'
    removeEmoticon = function(data){

      if (!"character" %in% class(data)) {
        bdpar.log(message = paste0("Checking the type of the 'data' variable: ",
                                   class(data)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "removeEmoticon")
      }

      stringr::str_replace_all(data,
                               rex::regex(self$emoticonPattern,
                                          ignore_case = TRUE,
                                          multiline = TRUE),
                               " ")
    }
  ),

  private = list(
    # A (\emph{logical}) value. Indicates if the emoticons are removed or
    # not.
    removeEmoticons = TRUE
  )
)
