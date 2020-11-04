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

#' @title Class to get the target field of the Instance
#'
#' @description This class allows searching in the path the \strong{target} of
#' the \code{\link{Instance}}.
#'
#' @section Details:
#' The targets that are searched can be controlled through the
#' constructor of the class where \emph{targetsName} will be the string that is
#' searched within the path and targets has the values that the property can
#' take.
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
#'          \code{\link{MeasureLengthPipe}}, \code{\link{GenericPipe}},
#'          \code{\link{ResourceHandler}}, \code{\link{SlangPipe}},
#'          \code{\link{StopWordPipe}}, \code{\link{StoreFileExtPipe}},
#'          \code{\link{TeeCSVPipe}}, \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export TargetAssigningPipe

TargetAssigningPipe <- R6Class(

  "TargetAssigningPipe",

  inherit = GenericPipe,

  public = list(
    #'
    #' @description Creates a \code{\link{TargetAssigningPipe}} object.
    #'
    #' @param targets A \code{\link{list}} value. Name of the targets property.
    #' @param targetsName A \code{\link{list}} value. The name of folders.
    #' @param propertyName A \code{\link{character}} value. Name of the property
    #' associated with the \code{\link{GenericPipe}}.
    #' @param alwaysBeforeDeps A \code{\link{list}} value. The dependencies
    #' alwaysBefore (\code{\link{GenericPipe}s} that must be executed before
    #' this one).
    #' @param notAfterDeps A \code{\link{list}} value. The dependencies
    #' notAfter (\code{\link{GenericPipe}s} that cannot be executed after
    #' this one).
    #'
    initialize = function(targets = list("ham","spam"),
                          targetsName = list("_ham_","_spam_"),
                          propertyName = "target",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list()) {

      if (!"list" %in% class(targets)) {
        bdpar.log(message = paste0("Checking the type of the 'targets' variable: ",
                                   class(targets)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      if (!"list" %in% class(targetsName)) {
        bdpar.log(message = paste0("Checking the type of the 'targetsName' variable: ",
                                   class(targetsName)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

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

      private$targets <- targets
      names(private$targets) <- targetsName

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },
    #'
    #' @description Preprocesses the \code{\link{Instance}} to obtain the
    #' target.
    #'
    #' @param instance A \code{\link{Instance}} value. The \code{\link{Instance}}
    #' to preprocess.
    #'
    #' @return The \code{\link{Instance}} with the modifications that have
    #' occurred in the pipe.
    #'
    pipe = function(instance) {

      if (!"Instance" %in% class(instance)) {
        bdpar.log(message = paste0("Checking the type of the 'instance' variable: ",
                                   class(instance)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "pipe")
      }

      instance$addProperties(self$getTarget(instance$getPath()),
                            super$getPropertyName())

      if (instance$getSpecificProperty("target") %in% "unrecognizable") {

        message <- c("The file: ", instance$getPath(), " has a target unrecognizable")

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
    #' @description Gets the target from a path.
    #'
    #' @param path  A \code{\link{character}} value. The path to analyze.
    #'
    #' @return The target of the path.
    #'
    getTarget = function(path) {

      if (!"character" %in% class(path)) {
        bdpar.log(message = paste0("Checking the type of the 'path' variable: ",
                                   class(path)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "getTarget")
      }

      for (target in names(self$getTargets())) {
        selectedTarget <- self$checkTarget(target,path)

        if (selectedTarget != "") {
          return(as.character(selectedTarget))
        }
      }

      "unrecognizable"
    },
    #'
    #' @description Checks if the target is in the path.
    #'
    #' @param target A \code{\link{character}} value. The target to find in the
    #' path.
    #' @param path  A \code{\link{character}} value. The path to analize.
    #'
    #' @return if the target is found, returns target, else returns "".
    #'
    checkTarget = function(target, path) {

      if (!"character" %in% class(target)) {
        bdpar.log(message = paste0("Checking the type of the 'target' variable: ",
                                   class(target)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "checkTarget")
      }

      if (!"character" %in% class(path)) {
        bdpar.log(message = paste0("Checking the type of the 'path' variable: ",
                                   class(path)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "checkTarget")
      }

      selectedTarget <- ""

      if (stringi::stri_detect_fixed(path,target)) {
        selectedTarget <- self$getTargets()[target]
      }

      selectedTarget
    },
    #'
    #' @description Gets of targets.
    #'
    #' @return Value of targets.
    #'
    getTargets = function() {
      private$targets
    }
  ),

  private = list(
    # A (\emph{list}) value. Name of the targets property.
    targets = list()
  )
)
