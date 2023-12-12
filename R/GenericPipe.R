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

#' @title Abstract super class that handles the management of the Pipes
#'
#' @description Provides the required methods to successfully handle each
#' \code{\link{GenericPipe}} class.
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{bdpar.log}},
#'          \code{\link{ContractionPipe}}, \code{\link{File2Pipe}},
#'          \code{\link{FindEmojiPipe}}, \code{\link{FindEmoticonPipe}},
#'          \code{\link{FindHashtagPipe}}, \code{\link{FindUrlPipe}},
#'          \code{\link{FindUserNamePipe}}, \code{\link{GuessDatePipe}},
#'          \code{\link{GuessLanguagePipe}}, \code{\link{Instance}},
#'          \code{\link{InterjectionPipe}}, \code{\link{MeasureLengthPipe}},
#'          \code{\link{ResourceHandler}}, \code{\link{SlangPipe}},
#'          \code{\link{StopWordPipe}}, \code{\link{StoreFileExtPipe}},
#'          \code{\link{TargetAssigningPipe}}, \code{\link{TeeCSVPipe}},
#'          \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export GenericPipe

GenericPipe <- R6Class(

  "GenericPipe",

  public = list(
    #'
    #' @description Creates a \link{GenericPipe} object.
    #'
    #' @param propertyName A \code{\link{character}} value. Name of the property
    #' associated with the Pipe.
    #' @param alwaysBeforeDeps A \code{\link{list}} value. The dependencies
    #' alwaysBefore (Pipes that must be executed before this one).
    #' @param notAfterDeps A \code{\link{list}} value. The dependencies notAfter
    #' (Pipes that cannot be executed after this one).
    #'
    initialize = function(propertyName, alwaysBeforeDeps, notAfterDeps) {

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
      private$propertyName <- propertyName
      private$alwaysBeforeDeps <- alwaysBeforeDeps
      private$notAfterDeps <- notAfterDeps
    },
    #'
    #' @description Abstract method to preprocess the \code{\link{Instance}}.
    #'
    #' @param instance A \code{\link{Instance}} value. The \code{\link{Instance}}
    #' to preprocess.
    #'
    #' @return The preprocessed \code{\link{Instance}}.
    #'
    pipe = function(instance) {
      bdpar.log(message = "I am an abstract interface method",
                level = "FATAL",
                className = class(self)[1],
                methodName = "pipe")
    },
    #'
    #' @description Gets of name of property.
    #'
    #' @return Value of name of property.
    #'
    getPropertyName = function() {
      private$propertyName
    },
    #'
    #' @description Gets of the dependencies always before.
    #'
    #' @return Value of dependencies always before.
    #'
    getAlwaysBeforeDeps = function() {
      private$alwaysBeforeDeps
    },
    #'
    #' @description Gets of the dependencies not after.
    #'
    #' @return Value of dependencies not after.
    #'
    getNotAfterDeps = function() {
      private$notAfterDeps
    },
    #'
    #' @description Changes the value of property's name.
    #'
    #' @param propertyName A \code{\link{character}} value. The new value of the
    #' property's name.
    #'
    setPropertyName = function(propertyName) {

      if (!"character" %in% class(propertyName)) {
        bdpar.log(message = paste0("Checking the type of the 'propertyName' variable: ",
                                   class(propertyName)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "setPropertyName")
      }

      private$propertyName <- propertyName
    },
    #'
    #' @description Changes the value of dependencies always before.
    #'
    #' @param alwaysBeforeDeps A \code{\link{list}} value. The new value of the
    #' dependencies always before.
    #'
    setAlwaysBeforeDeps = function(alwaysBeforeDeps) {

      if (!"list" %in% class(alwaysBeforeDeps)) {
        bdpar.log(message = paste0("Checking the type of the 'alwaysBeforeDeps' variable: ",
                                   class(alwaysBeforeDeps)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "setAlwaysBeforeDeps")
      }

      private$alwaysBeforeDeps <- alwaysBeforeDeps
    },
    #'
    #' @description Changes the value of dependencies not after.
    #'
    #' @param notAfterDeps A \code{\link{list}} value. The new value of the
    #' dependencies not after.
    #'
    setNotAfterDeps = function(notAfterDeps) {

      if (!"list" %in% class(notAfterDeps)) {
        bdpar.log(message = paste0("Checking the type of the 'notAfterDeps' variable: ",
                                   class(notAfterDeps)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "setNotAfterDeps")
      }

      private$notAfterDeps <- notAfterDeps
    },
    #'
    #' @description Generates an identification of pipe based on its fields.
    #'
    #' @param algo Algorithm to be applied. Options: "md5", "sha1", "crc32",
    #' "sha256", "sha512", "xxhash32", "xxhash64", "murmur32", "spookyhash
    #'
    #' @importFrom digest digest
    #'
    hash = function(algo = "md5") {
      x <- lapply(ls(private), function(x) {
        if (!inherits((private[[x]]),"function")) {
          private[[x]]
        }
      })
      y <- lapply(ls(self), function(x) {
        if (!inherits(self[[x]], "function")) {
          self[[x]]
        }
      })
      l <- append(x,y)
      names(l) <- c(ls(private), ls(self))

      digest(toString(l),
             algo = algo,
             serialize = FALSE)
    }
  ),

  private = list(
    # A (\emph{character}) value. The name of property.
    propertyName = "",
    # A (\emph{list}) dependencies of the type alwaysBefore. These dependencies
    # indicate what Pipes must be executed before the current one.
    alwaysBeforeDeps = list(),
    # A (\emph{list}) dependencies of the type notAfter. These dependencies
    # indicate what Pipes must not be executed after the current one.
    notAfterDeps = list()
  )
)
