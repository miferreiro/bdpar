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

#' @title Abstract super class that handles the management of the Instances
#'
#' @description Provides the required methods to succesfully handle each
#' \code{\link{Instance}} class.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \code{Instance$new(path)}
#' \itemize{
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{path:}}{
#' (\emph{character}) path of the file.
#' }
#' }
#' }
#' }
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{obtainDate:}}{
#' abstract function responsible for obtainining the date of the
#' \code{\link{Instance}}.
#' }
#' \item{\bold{obtainSource:}}{
#' abstract function in charge of determining the source of the
#' \code{\link{Instance}}.
#' }
#' \item{\bold{getDate:}}{
#' gets of date.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getDate()}
#' }
#' \item{\emph{Value:}}{
#' Value of date.
#' }
#' }
#' }
#'
#' \item{\bold{setDate:}}{
#' sets of date.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{setDate(date)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{date:}}{
#' (\emph{character}) the new value of date.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getSource:}}{
#' gets of source.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getSource()}
#' }
#' \item{\emph{Value:}}{
#' value of source.
#' }
#' }
#' }
#'
#' \item{\bold{setSource:}}{
#' modifies the source value.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{setSource(source)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{source:}}{
#' (\emph{character}) the new value of source.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getPath:}}{
#' gets of path.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getPath()}
#' }
#' \item{\emph{Value:}}{
#' value of path.
#' }
#' }
#' }
#'
#' \item{\bold{getProperties:}}{
#' gets the list of properties.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getProperties()}
#' }
#' \item{\emph{Value:}}{
#' Value of properties.
#' }
#' }
#' }
#'
#' \item{\bold{setProperties:}}{
#' modifies the list of properties.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{setProperties(properties)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{properties:}}{
#' (\emph{list}) containing the new properties.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{addProperties:}}{
#' adds a property to the list of properties.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{addProperties(propertyValue, propertyName)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{propertyValue:}}{
#' (\emph{Object}) the value of the new property.
#' }
#' \item{\strong{propertyName:}}{
#' (\emph{character}) the name of the new property.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getSpecificProperty:}}{
#' obtains a specific property.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getSpecificProperty(propertyName)}
#' }
#' \item{\emph{Value:}}{
#' the value of the specific property.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{propertyName:}}{
#' (\emph{character}) the name of the property to obtain.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{isSpecificProperty:}}{
#' checks for the existence of an specific property.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{isSpecificProperty(propertyName)}
#' }
#' \item{\emph{Value:}}{
#' A boolean results according to the existence of the specific property in the list of properties.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{propertyName:}}{
#' (\emph{character}) the name of the property to check.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{setSpecificProperty:}}{
#' modifies the value of the one property.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{setSpecificProperty(propertyName, propertyValue)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{propertyName:}}{
#' (\emph{Object}) the new value of the property.
#' }
#' \item{\strong{propertyValue:}}{
#' (\emph{character}) the name of the property.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getNamesOfProperties:}}{
#' gets of the names of all properties.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getNamesOfProperties()}
#' }
#' \item{\emph{Value:}}{
#' the names of properties.
#' }
#' }
#' }
#'
#' \item{\bold{isInstanceValid:}}{
#' checks if the \code{\link{Instance}} is valid.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{isInstanceValid()}
#' }
#' \item{\emph{Value:}}{
#' value of isValid.
#' }
#' }
#' }
#'
#' \item{\bold{invalidate:}}{
#' forces the invalidation of an specific \code{\link{Instance}}.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{invalidate()}
#' }
#' }
#' }
#'
#' \item{\bold{getFlowPipes:}}{
#' gets the list of the flow of Pipes.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getNamesOfProperties()}
#' }
#' \item{\emph{Value:}}{
#' names of the Pipes used.
#' }
#' }
#' }
#'
#' \item{\bold{addFlowPipes:}}{
#' adds a new Pipe to the flow of Pipes.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{addFlowPipes(namePipe)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{namePipe:}}{
#' (\emph{character}) name of the new Pipe to be added in the Pipe flow.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getBanPipes:}}{
#' gets an array with contaning all the Pipes.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getBanPipes()}
#' }
#' \item{\emph{Value:}}{
#' value of Pipe ban array.
#' }
#' }
#' }
#'
#' \item{\bold{addBanPipes:}}{
#' added the name of the Pipe to the array that keeps the track of Pipes having running after restrictions.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{addBanPipes(namePipe)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{namePipe:}}{
#' (\emph{character}) Pipe name to be introduced into the ban array.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{checkCompatibility:}}{
#' Check compability between Pipes.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{checkCompatibility(namePipe, alwaysBefore)}
#' }
#' \item{\emph{Value:}}{
#' boolean, depends if the compability between Pipes is correctly or not.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{namePipe:}}{
#' (\emph{character}) name of the Pipe to check the compatibility.
#' }
#' \item{\strong{alwaysBefore:}}{
#' (\emph{list}) pipes that the \code{Instance} had to go through.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{date:}}{
#'  (\emph{character}) the date on which the source was generated or sent.
#' }
#' \item{\bold{source:}}{
#'  (\emph{character}) the text of the file without modifications.
#' }
#' \item{\bold{path:}}{
#'  (\emph{character}) identifier of the \code{Instance}, in this case it will be the path of
#'  the file from which the properties are extracted.
#' }
#' \item{\bold{data:}}{
#'  (\emph{character}) the text of the file with modifications.
#' }
#' \item{\bold{properties:}}{
#'  (\emph{list}) contains a list of properties extracted from the text that is being
#'  processed.
#' }
#' \item{\bold{isValid:}}{
#'  (\emph{logical}) indicates if the \code{\link{Instance}} is valid or not.
#' }
#' \item{\bold{flowPipes:}}{
#'  (\emph{list}) the list contains the Pipes that the \code{Instance} has passed through.
#' }
#' \item{\bold{banPipes:}}{
#'  (\emph{array}) the list contains the Pipes that can not be executed from that moment.
#' }
#' }
#'
#' @keywords NULL
#'
#' @import pipeR R6 rlist
#' @export Instance

Instance <- R6Class(

  "Instance",

  public = list(

    initialize = function(path) {

      if (!"character" %in% class(path)) {
        stop("[Instance][initialize][Error]
                Checking the type of the variable: path ",
                  class(path))
      }

      private$path <- path

      self$addProperties(self$getPath(), "Initial_path")

    },

    obtainDate = function() {

      stop("[Instance][obtainDate][Error]
              I'm an abstract interface method")
    },

    obtainSource = function() {

      stop("[Instance][obtainSource][Error]
              I'm an abstract interface method")
    },

    getDate = function() {

      return(private$date)
    },

    getSource = function() {

      return(private$source)
    },

    getPath = function() {

      return(private$path)
    },

    getData = function() {

      return(private$data)
    },

    getProperties = function() {

      return(private$properties)
    },

    setSource = function(source) {

      if (!"character" %in% class(source)) {
        stop("[Instance][setSource][Error]
                Checking the type of the variable: source ",
                  class(source))
      }

      private$source <- source

      return()
    },

    setDate = function(date) {

      if (!"character" %in% class(date)) {
        stop("[Instance][setDate][Error]
                Checking the type of the variable: date ",
                  class(date))
      }

      private$date <- date

      return()
    },

    setProperties = function(properties) {

      if (!"list" %in% class(properties)) {
        stop("[Instance][setProperties][Error]
                Checking the type of the variable: properties ",
                  class(properties))
      }

      private$properties <- properties

      return()
    },

    addProperties = function(propertyValue, propertyName) {

      if (!"character" %in% class(propertyName)) {
        stop("[Instance][addProperties][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }

      private$properties <- list.append(self$getProperties(), propertyValue)

      names(private$properties)[length(self$getProperties())] <- propertyName

      return()
    },

    getSpecificProperty = function(propertyName) {

      if (!"character" %in% class(propertyName)) {
        stop("[Instance][getSpecificProperty][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }

      return(self$getProperties()[[propertyName]])
    },

    isSpecificProperty = function(propertyName) {

      return(propertyName %in% self$getNamesOfProperties())
    },

    setSpecificProperty = function(propertyName, propertyValue) {

      if (!"character" %in% class(propertyName)) {
        stop("[Instance][setSpecificProperty][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }

      private$properties[[propertyName]] <- propertyValue

      return()
    },

    getNamesOfProperties = function() {

      return(self$getProperties() %>>% names())
    },

    setData = function(data) {

      if (!"character" %in% class(data)) {
        stop("[Instance][setData][Error]
                Checking the type of the variable: data ",
                  class(data))
      }

      private$data <- data

      return()
    },

    isInstanceValid = function() {

      return(private$isValid)
    },

    invalidate = function() {

      private$isValid <- FALSE

      return()
    },

    getFlowPipes = function() {

      return(private$flowPipes)
    },

    addFlowPipes = function(namePipe) {

      if (!"character" %in% class(namePipe)) {
        stop("[Instance][addFlowPipes][Error]
                Checking the type of the variable: namePipe ",
                  class(namePipe))
      }

      private$flowPipes <- list.append(private$flowPipes, namePipe)

      return()
    },

    getBanPipes = function() {

      return(private$banPipes)
    },

    addBanPipes = function(namePipe) {

      if (!"character" %in% class(namePipe) & !is.null(namePipe)) {
        stop("[Instance][addBanPipes][Error]
                Checking the type of the variable: namePipe ",
                  class(namePipe))
      }

      if (!is.null(namePipe)) {
        private$banPipes <- c(private$banPipes, namePipe)
      }

      return()
    },

    checkCompatibility = function(namePipe, alwaysBefore) {

      if (!"character" %in% class(namePipe)) {
        stop("[Instance][checkCompatibility][Error]
                Checking the type of the variable: namePipe ",
                  class(namePipe))
      }

      if (!"list" %in% class(alwaysBefore)) {
        stop("[Instance][checkCompatibility][Error]
                Checking the type of the variable: alwaysBefore ",
                  class(alwaysBefore))
      }

      for (depsB in alwaysBefore) {

        if (!depsB %in% self$getFlowPipes()) {
          return(FALSE)
        }
      }

      if (namePipe %in% self$getBanPipes()) {
        return(FALSE)
      }

      return(TRUE)
    }
  ),

  private = list(
    date = "",
    source = "",
    path = "",
    data = "",
    properties = list(),
    isValid = TRUE,
    flowPipes = list() ,
    banPipes = c()
  )
)
