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

#' @title Class to obtain the length of the data field of an Instance
#'
#' @description This class is responsible of obtain the length of the\strong{data}
#' field of each \code{\link{Instance}}. Creates the \strong{length} property
#' which indicates the length of the text. The property's name is customize
#' thought the class constructor.
#'
#' @section Inherit:
#' This class inherits from \code{\link{GenericPipe}} and implements the
#' \code{pipe} abstract function.
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{ContractionPipe}},
#'          \code{\link{File2Pipe}}, \code{\link{FindEmojiPipe}},
#'          \code{\link{FindEmoticonPipe}}, \code{\link{FindHashtagPipe}},
#'          \code{\link{FindUrlPipe}}, \code{\link{FindUserNamePipe}},
#'          \code{\link{GuessDatePipe}}, \code{\link{GuessLanguagePipe}},
#'          \code{\link{Instance}}, \code{\link{InterjectionPipe}},
#'          \code{\link{GenericPipe}}, \code{\link{ResourceHandler}},
#'          \code{\link{SlangPipe}}, \code{\link{StopWordPipe}},
#'          \code{\link{StoreFileExtPipe}}, \code{\link{TargetAssigningPipe}},
#'          \code{\link{TeeCSVPipe}}, \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export MeasureLengthPipe

MeasureLengthPipe <- R6Class(

  "MeasureLengthPipe",

  inherit = GenericPipe,

  public = list(
    #'
    #' @description Creates a \code{\link{File2Pipe}} object.
    #'
    #' @param propertyName A \code{\link{character}} value. Name of the property
    #' associated with the \code{\link{GenericPipe}}.
    #' @param alwaysBeforeDeps A \code{\link{list}} value. The dependencies
    #' alwaysBefore (\code{\link{GenericPipe}s} that must be executed before
    #' this one).
    #' @param notAfterDeps A \code{\link{list}} value. The dependencies
    #' notAfter (\code{\link{GenericPipe}s} that cannot be executed after
    #' this one).
    #' @param nchar_conf A \code{\link{logical}} value. indicates if the pipe
    #' uses nchar or object.size.
    #'
    initialize = function(propertyName = "length",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list(),
                          nchar_conf = TRUE) {

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

      if (!"logical" %in% class(nchar_conf)) {
        stop("[", class(self)[1], "][initialize][Error] ",
             "Checking the type of the 'nchar_conf' variable: ",
             class(nchar_conf))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      private$nchar_conf <- nchar_conf
    },
    #'
    #' @description Preprocesses the \code{\link{Instance}} to obtain the
    #' length of data.
    #'
    #' @param instance A \code{\link{Instance}} value. The \code{\link{Instance}}
    #' to preprocess.
    #'
    #' @return The \code{\link{Instance}} with the modifications that have
    #' occurred in the pipe.
    #'
    #' @import pipeR
    #'
    pipe = function(instance) {

      if (!"Instance" %in% class(instance)) {
          stop("[", class(self)[1], "][pipe][Error] ",
               "Checking the type of the 'instance' variable: ",
               class(instance))
      }

      instance$getData() %>>%
        {self$getLength(.,private$nchar_conf)} %>>%
          {instance$addProperties(.,private$propertyName)}

      instance
    },
    #'
    #' @description Preprocesses the \code{\link{Instance}} to obtain the
    #' length of data.
    #'
    #' @param data A \code{\link{character}} value. The text to preprocess.
    #' @param nchar_conf A \code{\link{logical}} value. Indicates if the pipe
    #' uses nchar or object.size.
    #'
    #' @return The \code{\link{Instance}} with the modifications that have
    #' occurred in the pipe.
    #'
    #' @importFrom utils object.size
    #'
    getLength = function(data, nchar_conf = TRUE) {

      if (!"character" %in% class(data)) {
        stop("[", class(self)[1], "][getLength][Error] ",
             "Checking the type of the 'data' variable: ",
             class(data))
      }

      if (!"logical" %in% class(nchar_conf)) {
        stop("[", class(self)[1], "][getLength][Error] ",
             "Checking the type of the 'nchar_conf' variable: ",
             class(nchar_conf))
      }

      ifelse(private$nchar_conf, nchar(data), object.size(data))
    }
  ),

  private = list(
    # A (\emph{logical}) value. Indicates if the Pipe uses nchar or object.size.
    nchar_conf = TRUE
  )
)
