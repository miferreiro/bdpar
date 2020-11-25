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

#' @title Class to find and/or remove the users on the data field of an Instance
#'
#' @description This class is responsible of detecting the existing use names in the
#' \strong{data} field of each \code{\link{Instance}}. Identified user names are
#' stored inside the \strong{userName} field of \code{\link{Instance}} class.
#' Moreover if required, is able to perform inline user name removement.
#
#' @section Details:
#' The regular expressions indicated in the \code{userPattern}
#' variable are used to identify user names.
#'
#' @section Note:
#' \code{\link{FindUserNamePipe}} will automatically invalidate the
#' \code{\link{Instance}} whenever the obtained data is empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{GenericPipe}} and implements the
#' \code{pipe} abstract function.
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{ContractionPipe}},
#'          \code{\link{File2Pipe}}, \code{\link{FindEmojiPipe}},
#'          \code{\link{FindEmoticonPipe}}, \code{\link{FindHashtagPipe}},
#'          \code{\link{FindUrlPipe}}, \code{\link{GuessDatePipe}},
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
#' @export FindUserNamePipe

FindUserNamePipe <- R6Class(

  "FindUserNamePipe",

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
    #' @param removeUser A \code{\link{logical}} value. Indicates if the
    #' name users are removed.
    #'
    initialize = function(propertyName = "userName",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list(),
                          removeUser = TRUE) {

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

      if (!"logical" %in% class(removeUser)) {
        bdpar.log(message = paste0("Checking the type of the 'removeUser' variable: ",
                                   class(removeUser)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      private$removeUser <- removeUser
    },
    #' @field userPattern  A \code{\link{character}} value. The regular
    #' expression to detect name users.
    userPattern = "(?:\\s|^|[\"><\u00A1\u00BF?!;:,.'-])(@[^[:cntrl:][:space:]!\"#$%&'()*+\\\\,\\/:;<=>?@\\[\\]^`{|}~]+)[;:?\"!,.'>-]?(?=(?:\\s|$|>))",
    #'
    #' @description Preprocesses the \code{\link{Instance}} to obtain/remove
    #' the name users. The emoticons found in the data are added to the
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
    pipe = function(instance){

      if (!"Instance" %in% class(instance)) {
        bdpar.log(message = paste0("Checking the type of the 'instance' variable: ",
                                   class(instance)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "pipe")
      }

      instance$addProperties(unlist(unique(
        self$findUserName(instance$getData()))),
        super$getPropertyName())

      if (private$removeUser) {
        instance$setData(trimws(x = self$removeUserName(instance$getData())))
      }

      if (is.na(instance$getData()) ||
          all(instance$getData() == "") ||
          is.null(instance$getData())) {
        message <- paste0("The file: ", instance$getPath(), " has data empty on pipe UserName")

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
    #' @description Finds the \emph{name users} in the data.
    #'
    #' @param data A \code{\link{character}} value. The text to search the
    #' name users.
    #'
    #' @return The \code{\link{list}} with name users found.
    #'
    findUserName = function(data) {

      if (!"character" %in% class(data)) {
        bdpar.log(message = paste0("Checking the type of the 'data' variable: ",
                                   class(data)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "findUserName")
      }

      stringr::str_match_all(data,
                             rex::regex(self$userPattern,
                                        ignore_case = TRUE,
                                        multiline = TRUE))[[1]][,2]
    },
    #'
    #' @description Removes the \emph{name users} in the data.
    #'
    #' @param data A \code{\link{character}} value. The text where name users
    #' will be removed.
    #'
    #' @return The data with the name users removed.
    #'
    removeUserName = function(data) {

      if (!"character" %in% class(data)) {
        bdpar.log(message = paste0("Checking the type of the 'data' variable: ",
                                   class(data)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "removeUserName")
      }

      stringr::str_replace_all(data,
                               rex::regex(self$userPattern,
                                          ignore_case = TRUE,
                                          multiline = TRUE),
                               " ")
    }
  ),

  private = list(
    # A (\emph{logical}) value. Indicates if the name users are removed or
    # not.
    removeUser = TRUE
  )
)
