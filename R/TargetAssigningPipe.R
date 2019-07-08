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

#' @title Class to get the target field of the Instance
#'
#' @description This class allows searching in the path the \strong{target} of the
#' \code{\link{Instance}}.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \preformatted{
#' TargetAssigningPipe$new(targets = list("ham","spam"),
#'                         targetsName = list("_ham_","_spam_"),
#'                         propertyName = "target",
#'                         alwaysBeforeDeps = list(),
#'                         notAfterDeps = list())
#' }
#' \itemize{
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{targets:}}{
#' (\emph{list}) name of the targets property.
#' }
#' \item{\strong{targetsName:}}{
#' (\emph{list}) the name of folders.
#' }
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
#' The targets that are searched can be controlled through the
#' constructor of the class where \emph{targetsName} will be the string that is
#' searched within the path and targets has the values that the property can
#' take.
#'
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe:}}{
#' preprocesses the \code{\link{Instance}} to obtain the target.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{pipe(instance)}
#' }
#' \item{\emph{Value:}}{
#' The \code{\link{Instance}} with the modifications that have occurred in the Pipe.
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
#' \item{\bold{getTarget}}{
#' gets the target from a path.
#' \itemize{
#' \item{\emph{Usage:}}{
#'
#' \code{getTarget(path)}
#' }
#' \item{\emph{Value:}}{
#' the target of the path.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{path:}}{
#' (\emph{character}) path to analize.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{checkTarget:}}{
#' checks if the target is in the path.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{checkTarget(target, path)}
#' }
#' \item{\emph{Value:}}{
#' if the target is found, returns target, else returns "".
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{target:}}{
#' (\emph{character}) target to find in the path.
#' }
#' \item{\strong{path:}}{
#' (\emph{character}) path to analize.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getTargets:}}{
#' gets of targets.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getTargets()}
#' }
#' \item{\emph{Value:}}{
#' value of targets.
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{targets:}}{
#'  (\emph{list}) name of the targets property.
#' }
#' }
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{ContractionPipe}},
#'          \code{\link{File2Pipe}}, \code{\link{FindEmojiPipe}},
#'          \code{\link{FindEmoticonPipe}}, \code{\link{FindHashtagPipe}},
#'          \code{\link{FindUrlPipe}}, \code{\link{FindUserNamePipe}},
#'          \code{\link{GuessDatePipe}}, \code{\link{GuessLanguagePipe}},
#'          \code{\link{Instance}}, \code{\link{InterjectionPipe}},
#'          \code{\link{MeasureLengthPipe}}, \code{\link{PipeGeneric}},
#'          \code{\link{ResourceHandler}}, \code{\link{SlangPipe}},
#'          \code{\link{StopWordPipe}}, \code{\link{StoreFileExtPipe}},
#'          \code{\link{TeeCSVPipe}}, \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import pipeR R6
#' @export TargetAssigningPipe

TargetAssigningPipe <- R6Class(

  "TargetAssigningPipe",

  inherit = PipeGeneric,

  public = list(

    initialize = function(targets = list("ham","spam"),
                            targetsName = list("_ham_","_spam_"),
                              propertyName = "target",
                                alwaysBeforeDeps = list(),
                                  notAfterDeps = list()) {

      if (!requireNamespace("stringi", quietly = TRUE)) {
        stop("[TargetAssigningPipe][initialize][Error]
                Package \"stringi\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!"list" %in% class(targets)) {
        stop("[TargetAssigningPipe][initialize][Error]
                Checking the type of the variable: targets ",
                  class(targets))
      }

      if (!"list" %in% class(targetsName)) {
        stop("[TargetAssigningPipe][initialize][Error]
                Checking the type of the variable: targetsName ",
                  class(targetsName))
      }

      if (!"character" %in% class(propertyName)) {
        stop("[TargetAssigningPipe][initialize][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[TargetAssigningPipe][initialize][Error]
                Checking the type of the variable: alwaysBeforeDeps ",
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[TargetAssigningPipe][initialize][Error]
                Checking the type of the variable: notAfterDeps ",
                  class(notAfterDeps))
      }

      private$targets <- targets
      names(private$targets) <- targetsName

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)

    },

    pipe = function(instance) {

      if (!"Instance" %in% class(instance)) {
        stop("[TargetAssigningPipe][pipe][Error]
                 Checking the type of the variable: instance ",
                   class(instance))
      }

      instance$addFlowPipes("TargetAssigningPipe")

      if (!instance$checkCompatibility("TargetAssigningPipe", self$getAlwaysBeforeDeps())) {
        stop("[TargetAssigningPipe][pipe][Error] Bad compatibility between Pipes.")
      }

      instance$addBanPipes(unlist(super$getNotAfterDeps()))

      instance$getPath() %>>%
        self$getTarget() %>>%
          {instance$addProperties(.,super$getPropertyName())}

      if (instance$getSpecificProperty("target") %in% "unrecognizable") {

        message <- c( "The file: " , instance$getPath() , " has a target unrecognizable")

        instance$addProperties(message, "reasonToInvalidate")

        cat("[TargetAssigningPipe][pipe][Warning] ", message, " \n")

        instance$invalidate()

        return(instance)
      }

      return(instance)

    },

    getTarget = function(path) {

      if (!"character" %in% class(path)) {
        stop("[TargetAssigningPipe][getTarget][Error]
                Checking the type of the variable: path ",
                  class(path))
      }

      for (target in names(self$getTargets())) {
        selectedTarget <- self$checkTarget(target,path)

        if (selectedTarget != "") {
          return(as.character(selectedTarget))
        }
      }

      return("unrecognizable")
    },

    checkTarget = function(target, path) {

      if (!"character" %in% class(target)) {
        stop("[TargetAssigningPipe][checkTarget][Error]
                Checking the type of the variable: target ",
                  class(target))
      }

      if (!"character" %in% class(path)) {
        stop("[TargetAssigningPipe][checkTarget][Error]
                Checking the type of the variable: path ",
                  class(path))
      }

      selectedTarget <- ""

      if (stringi::stri_detect_fixed(path,target)) {
        selectedTarget <- self$getTargets()[target]
      }

      return(selectedTarget)
    },

    getTargets = function() {

      return(private$targets)
    }
  ),

  private = list(
    targets = list()
  )
)
