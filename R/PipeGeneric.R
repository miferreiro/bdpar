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

#' @title Abstract super classs that handles the management of the Pipes
#'
#' @description Provides the required methods to succesfully handle each
#' \code{\link{PipeGeneric}} class.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \preformatted{
#' PipeGeneric$new(propertyName,
#'                 alwaysBeforeDeps,
#'                 notAfterDeps)
#' }
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
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe:}}{
#' abstract method to preprocess the \code{\link{Instance}}.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{pipe(instance)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{instance:}}{
#' (\emph{Instance}) \code{\link{Instance}} to preprocess.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getPropertyName:}}{
#' gets of name of property.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getPropertyName()}
#' }
#' \item{\emph{Value:}}{
#' value of name of property.
#' }
#' }
#' }
#'
#' \item{\bold{getAlwaysBeforeDeps:}}{
#' gets of the dependences always before.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getAlwaysBeforeDeps()}
#' }
#' \item{\emph{Value:}}{
#' value of dependences always before.
#' }
#' }
#' }
#'
#' \item{\bold{getNotAfterDeps:}}{
#' gets of the dependences not after.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getNotAfterDeps()}
#' }
#' \item{\emph{Value:}}{
#' value of dependences not after.
#' }
#' }
#' }
#'
#' \item{\bold{setPropertyName:}}{
#' changes the value of property's name.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{setPropertyName(propertyName)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{propertyName:}}{
#' (\emph{character}) the new value of the property's name.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{setAlwaysBeforeDeps:}}{
#' changes the value of dependencies always before.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{setAlwaysBeforeDeps(alwaysBeforeDeps)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{alwaysBeforeDeps:}}{
#' (\emph{list}) the new value of the dependencies always before.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{setNotAfterDeps:}}{
#' changes the value of dependencies not after.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{setNotAfterDeps(notAfterDeps)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{notAfterDeps:}}{
#' (\emph{list}) the new value of the dependencies not after.
#' }
#' }
#' }
#' }
#' }
#'
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{propertyName:}}{
#' (\emph{character}) the name of property.
#' }
#' \item{\bold{alwaysBeforeDeps:}}{
#' (\emph{list}) dependencies of the type alwaysBefore. These dependences indicate
#' what Pipes must be executed before the current one.
#' }
#' \item{\bold{notAfterDeps:}}{
#' (\emph{list}) dependencies of the type notAfter. These dependences indicate what
#' Pipes must not be executed after the current one.
#' }
#' }
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{ContractionPipe}},
#'          \code{\link{File2Pipe}}, \code{\link{FindEmojiPipe}},
#'          \code{\link{FindEmoticonPipe}}, \code{\link{FindHashtagPipe}},
#'          \code{\link{FindUrlPipe}}, \code{\link{FindUserNamePipe}},
#'          \code{\link{GuessDatePipe}}, \code{\link{GuessLanguagePipe}},
#'          \code{\link{Instance}}, \code{\link{InterjectionPipe}},
#'          \code{\link{MeasureLengthPipe}}, \code{\link{ResourceHandler}},
#'          \code{\link{SlangPipe}}, \code{\link{StopWordPipe}},
#'          \code{\link{StoreFileExtPipe}}, \code{\link{TargetAssigningPipe}},
#'          \code{\link{TeeCSVPipe}}, \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export PipeGeneric

PipeGeneric <- R6Class(

  "PipeGeneric",

  public = list(

    initialize = function(propertyName, alwaysBeforeDeps, notAfterDeps) {

      if (!"character" %in% class(propertyName)) {
        stop("[PipeGeneric][initialize][Error] ",
             "Checking the type of the 'propertyName' variable: ",
             class(propertyName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[PipeGeneric][initialize][Error] ",
             "Checking the type of the 'alwaysBeforeDeps' variable: ",
             class(alwaysBeforeDeps))
      }

      if (!"list" %in% class(notAfterDeps)) {
        stop("[PipeGeneric][initialize][Error] ",
             "Checking the type of the 'notAfterDeps' variable: ",
             class(notAfterDeps))
      }
      private$propertyName <- propertyName
      private$alwaysBeforeDeps <- alwaysBeforeDeps
      private$notAfterDeps <- notAfterDeps

    },

    pipe = function(instance) {

      stop("I am an abstract interface method")
    },

    getPropertyName = function() {

      return(private$propertyName)
    },

    getAlwaysBeforeDeps = function() {

      return(private$alwaysBeforeDeps)
    },

    getNotAfterDeps = function() {

      return(private$notAfterDeps)
    },

    setPropertyName = function(propertyName) {

      if (!"character" %in% class(propertyName)) {
        stop("[PipeGeneric][setPropertyName][Error] ",
             "Checking the type of the 'propertyName' variable: ",
             class(propertyName))
      }

      private$propertyName <- propertyName

      return()
    },

    setAlwaysBeforeDeps = function(alwaysBeforeDeps) {

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[PipeGeneric][setAlwaysBeforeDeps][Error] ",
             "Checking the type of the 'alwaysBeforeDeps' variable: ",
             class(alwaysBeforeDeps))
      }

      private$alwaysBeforeDeps <- alwaysBeforeDeps

      return()
    },

    setNotAfterDeps = function(notAfterDeps) {

      if (!"list" %in% class(notAfterDeps)) {
        stop("[PipeGeneric][setNotAfterDeps][Error] ",
             "Checking the type of the 'notAfterDeps' variable: ",
             class(notAfterDeps))
      }

      private$notAfterDeps <- notAfterDeps

      return()
    }
  ),

  private = list(
    propertyName = "",
    alwaysBeforeDeps = list() ,
    notAfterDeps = list()
  )
)
