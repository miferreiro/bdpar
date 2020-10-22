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

#' @title Class to find and/or remove the hashtags on the data field of an Instance
#'
#' @description This class is responsible of detecting the existing hashtags in the
#' \strong{data} field of each \code{\link{Instance}}. Identified hashtags are
#' stored inside the \strong{hashtag} field of \code{\link{Instance}} class.
#' Moreover if required, is able to perform inline hashtag removement.
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
#' This class inherits from \code{\link{GenericPipe}} and implements the
#' \code{pipe} abstract function.
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{ContractionPipe}},
#'          \code{\link{File2Pipe}}, \code{\link{FindEmojiPipe}},
#'          \code{\link{FindEmoticonPipe}}, \code{\link{FindUrlPipe}},
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
#' @export FindHashtagPipe

FindHashtagPipe <- R6Class(

  "FindHashtagPipe",

  inherit = GenericPipe,

  public = list(
    #'
    #' @description Creates a \code{\link{FindHashtagPipe}} object.
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
    #' @param removeHashtags A \code{\link{logical}} value. Indicates if the
    #' hashtags are removed.
    #'
    initialize = function(propertyName = "hashtag",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list(),
                          removeHashtags = TRUE) {

      if (!"character" %in% class(propertyName)) {
        stop("[", class(self)[1], "][initialize][Error] ",
             "Checking the type of the 'propertyName' variable: ",
             class(propertyName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[", class(self)[1], "][initialize][Error] ",
             "Checking the type of the 'alwaysBeforeDeps' variable: ",
             class(alwaysBeforeDeps))
      }

      if (!"list" %in% class(notAfterDeps)) {
        stop("[", class(self)[1], "][initialize][Error] ",
             "Checking the type of the 'notAfterDeps' variable: ",
             class(notAfterDeps))
      }

      if (!"logical" %in% class(removeHashtags)) {
        stop("[", class(self)[1], "][initialize][Error] ",
             "Checking the type of the 'removeHashtags' variable: ",
             class(removeHashtags))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      private$removeHashtags <- removeHashtags
    },
    #' @field hashtagPattern  A \code{\link{character}} value. The regular
    #' expression to detect hashtags.
    hashtagPattern = "(?:\\s|^|[\"><\u00A1\u00BF?!;:,.'-])(#[^[:cntrl:][:space:]!\"#$%&'()*+\\\\,\\/:;<=>?@\\[\\]^`{|}~.-]+)[;:?\"!,.'>-]?(?=(?:\\s|$|>))",
    #'
    #' @description Preprocesses the \code{\link{Instance}} to obtain/remove
    #' the hashtags. The hashtags found in the data are added to the
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
        stop("[", class(self)[1], "][pipe][Error] ",
             "Checking the type of the 'instance' variable: ",
             class(instance))
      }

      instance$getData() %>>%
        self$findHashtag() %>>%
          unique() %>>%
            unlist() %>>%
              {instance$addProperties(.,super$getPropertyName())}

      if (private$removeHashtags) {
          instance$getData()  %>>%
            self$removeHashtag() %>>%
              textutils::trim() %>>%
                {instance$setData(.)}
      }

      if (is.na(instance$getData()) ||
          all(instance$getData() == "") ||
          is.null(instance$getData())) {
        message <- c("The file: ", instance$getPath(), " has data empty on pipe Hashtag")

        instance$addProperties(message, "reasonToInvalidate")

        warning("[", class(self)[1], "][pipe][Warning] ", message)

        instance$invalidate()

        return(instance)
      }

      instance
    },
    #'
    #' @description Finds the \emph{hashtags} in the data.
    #'
    #' @param data A \code{\link{character}} value. The text to search the
    #' hashtags.
    #'
    #' @return The \code{\link{list}} with hashtags found.
    #'
    findHashtag = function(data){

      if (!"character" %in% class(data)) {
        stop("[", class(self)[1], "][findHashtag][Error] ",
             "Checking the type of the 'data' variable: ",
             class(data))
      }

      stringr::str_match_all(data,
                             rex::regex(self$hashtagPattern,
                                        ignore_case = TRUE,
                                        multiline = TRUE))[[1]][,2]
    },
    #'
    #' @description Removes the \emph{hashtags} in the data.
    #'
    #' @param data A \code{\link{character}} value. The text where hashtags
    #' will be removed.
    #'
    #' @return The data with the hashtags removed.
    #'
    removeHashtag = function(data){

      if (!"character" %in% class(data)) {
        stop("[", class(self)[1], "][removeHashtag][Error] ",
             "Checking the type of the 'data' variable: ",
             class(data))
      }

      stringr::str_replace_all(data,
                               rex::regex(self$hashtagPattern,
                                          ignore_case = TRUE,
                                          multiline = TRUE),
                               " ")
    }
  ),

  private = list(
    # A (\emph{logical}) value. Indicates if the hashtags are removed or
    # not.
    removeHashtags = TRUE
  )
)
