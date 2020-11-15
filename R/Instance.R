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

#' @title Abstract super class that handles the management of the Instances
#'
#' @description Provides the required methods to successfully handle each
#' \code{\link{Instance}} class.
#'
#' @seealso \code{\link{ExtractorEml}}, \code{\link{ExtractorSms}},
#' \code{\link{ExtractorTwtid}}, \code{\link{ExtractorYtbid}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export Instance

Instance <- R6Class(

  "Instance",

  public = list(
    #'
    #' @description Creates a \code{\link{Instance}} object.
    #'
    #' @param path A \code{\link{character}} value. Path of the file.
    #'
    initialize = function(path) {

      if (!"character" %in% class(path)) {
        bdpar.log(message = paste0("Checking the type of the 'path' variable: ",
                                   class(path)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "initialize")
      }

      private$path <- path
    },
    #'
    #' @description Abstract function responsible for obtaining the date of the
    #' \code{\link{Instance}}.
    #'
    obtainDate = function() {
      bdpar.log(message = "I am an abstract interface method",
                level = "FATAL",
                className = class(self)[1],
                methodName = "obtainDate")
    },
    #'
    #' @description Abstract function responsible for determining the source of
    #' the \code{\link{Instance}}.
    #'
    obtainSource = function() {
      bdpar.log(message = "I am an abstract interface method",
                level = "FATAL",
                className = class(self)[1],
                methodName = "obtainSource")
    },
    #'
    #' @description Gets the date.
    #'
    #' @return Value of date.
    #'
    getDate = function() {
      private$date
    },
    #'
    #' @description Gets the source.
    #'
    #' @return Value of source.
    #'
    getSource = function() {
      private$source
    },
    #'
    #' @description Gets the path.
    #'
    #' @return Value of path.
    #'
    getPath = function() {
      private$path
    },
    #'
    #' @description Gets the data.
    #'
    #' @return Value of data.
    #'
    getData = function() {
      private$data
    },
    #'
    #' @description Gets the properties
    #'
    #' @return Value of properties.
    #'
    getProperties = function() {
      private$properties
    },
    #'
    #' @description Modifies the source value.
    #'
    #' @param source A \code{\link{character}} value. The new value of source.
    #'
    setSource = function(source) {

      if (!"character" %in% class(source)) {
        bdpar.log(message = paste0("Checking the type of the 'source' variable: ",
                                   class(source)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "setSource")
      }

      private$source <- source
    },
    #'
    #' @description Modifies the data value.
    #'
    #' @param data A \code{\link{character}} value. The new value of data.
    #'
    setData = function(data) {

      if (!"character" %in% class(data)) {
        bdpar.log(message = paste0("Checking the type of the 'data' variable: ",
                                   class(data)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "setData")
      }

      private$data <- data
    },
    #'
    #' @description Modifies the date value.
    #'
    #' @param date A \code{\link{character}} value. The new value of date.
    #'
    setDate = function(date) {

      if (!"character" %in% class(date)) {
        bdpar.log(message = paste0("Checking the type of the 'date' variable: ",
                                   class(date)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "setDate")
      }

      private$date <- date
    },
    #'
    #' @description Modifies the properties value.
    #'
    #' @param properties A \code{\link{list}} value. The new list of properties.
    #'
    setProperties = function(properties) {

      if (!"list" %in% class(properties)) {
        bdpar.log(message = paste0("Checking the type of the 'properties' variable: ",
                                   class(properties)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "setProperties")
      }

      private$properties <- properties
    },
    #'
    #' @description Adds a property to the list of the properties.
    #'
    #' @param propertyValue A \code{Object} value. The value of the new property.
    #' @param propertyName A \code{\link{character}} value. The name of the new
    #' property.
    #'
    #' @import rlist
    #'
    addProperties = function(propertyValue, propertyName) {

      if (!"character" %in% class(propertyName)) {
        bdpar.log(message = paste0("Checking the type of the 'propertyName' variable: ",
                                   class(propertyName)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "addProperties")
      }

      private$properties <- list.append(self$getProperties(), propertyValue)

      names(private$properties)[length(self$getProperties())] <- propertyName
    },
    #'
    #' @description Obtains a specific property.
    #'
    #' @param propertyName A \code{\link{character}} value. The name of the
    #' property to obtain.
    #'
    #' @return The value of the specific property.
    #'
    getSpecificProperty = function(propertyName) {

      if (!"character" %in% class(propertyName)) {
        bdpar.log(message = paste0("Checking the type of the 'propertyName' variable: ",
                                   class(propertyName)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "getSpecificProperty")
      }

      self$getProperties()[[propertyName]]
    },
    #'
    #' @description Checks for the existence of an specific property.
    #'
    #' @param propertyName A \code{\link{character}} value. The name of the
    #' property to check.
    #'
    #' @return A logical results according to the existence of the specific
    #' property in the list of properties.
    #'
    isSpecificProperty = function(propertyName) {
      propertyName %in% self$getNamesOfProperties()
    },
    #'
    #' @description Modifies the value of the one property.
    #'
    #' @param propertyValue A \code{Object} value. The new value of the property.
    #' @param propertyName A \code{\link{character}} value. The name of the
    #' property.
    #'
    setSpecificProperty = function(propertyName, propertyValue) {

      if (!"character" %in% class(propertyName)) {
        bdpar.log(message = paste0("Checking the type of the 'propertyName' variable: ",
                                   class(propertyName)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "setSpecificProperty")
      }

      private$properties[[propertyName]] <- propertyValue
    },
    #'
    #' @description Gets of the names of all properties.
    #'
    #' @return The names of properties.
    #'
    getNamesOfProperties = function() {
       names(self$getProperties())
    },
    #'
    #' @description Checks if the \code{\link{Instance}} is valid.
    #'
    #' @return Value of isValid flag.
    #'
    isInstanceValid = function() {
      private$isValid
    },
    #'
    #' @description Forces the invalidation of an specific \code{\link{Instance}}.
    #'
    invalidate = function() {
      private$isValid <- FALSE
    },
    #'
    #' @description Gets the list of the flow of \code{\link{GenericPipe}}.
    #'
    #' @return Names of the \code{\link{GenericPipe}} used.
    #'
    getFlowPipes = function() {
      private$flowPipes
    },
    #'
    #' @description Gets the list of the flow of \code{\link{GenericPipe}}.
    #'
    #' @param namePipe A \code{\link{character}} value. Name of the new
    #' \code{\link{GenericPipe}} to be added in the \code{\link{GenericPipeline}}.
    #'
    #' @import rlist
    #'
    addFlowPipes = function(namePipe) {

      if (!"character" %in% class(namePipe)) {
        bdpar.log(message = paste0("Checking the type of the 'namePipe' variable: ",
                                   class(namePipe)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "addFlowPipes")
      }

      private$flowPipes <- list.append(private$flowPipes, namePipe)
    },
    #'
    #' @description Gets an array with containing all the ban
    #' \code{\link{GenericPipe}}.
    #'
    #' @return Value of ban \code{\link{GenericPipe}} array.
    #'
    getBanPipes = function() {
      private$banPipes
    },
    #'
    #' @description Added the name of the Pipe to the array that keeps the track
    #' of \code{\link{GenericPipe}s} having running after restrictions.
    #'
    #' @param namePipe A \code{\link{character}} value.
    #' \code{\link{GenericPipe}} name to be introduced into the ban array.
    #'
    addBanPipes = function(namePipe) {

      if (!"character" %in% class(namePipe) & !is.null(namePipe)) {
        bdpar.log(message = paste0("Checking the type of the 'namePipe' variable: ",
                                   class(namePipe)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "addBanPipes")
      }

      if (!is.null(namePipe)) {
        private$banPipes <- c(private$banPipes, namePipe)
      }
    },
    #'
    #' @description Check compatibility between \code{\link{GenericPipe}s}.
    #'
    #' @param namePipe A \code{\link{character}} value. The name of the
    #' \code{\link{GenericPipe}} name to check the compatibility.
    #' @param alwaysBefore A \code{\link{list}} value.
    #' \code{\link{GenericPipe}s} that the \code{\link{Instance}} had to go
    #' through.
    #'
    checkCompatibility = function(namePipe, alwaysBefore) {

      if (!"character" %in% class(namePipe)) {
        bdpar.log(message = paste0("Checking the type of the 'namePipe' variable: ",
                                   class(namePipe)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "checkCompatibility")
      }

      if (!"list" %in% class(alwaysBefore)) {
        bdpar.log(message = paste0("Checking the type of the 'alwaysBefore' variable: ",
                                   class(alwaysBefore)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "checkCompatibility")
      }

      for (depsB in alwaysBefore) {

        if (!depsB %in% self$getFlowPipes()) {
          return(FALSE)
        }
      }

      if (namePipe %in% self$getBanPipes()) {
        return(FALSE)
      }

      TRUE
    },
    #'
    #' @description Returns a \code{\link{character}} representing the instance
    #'
    #' @return \code{\link{Instance}} \code{\link{character}} representation
    #'
    toString = function() {
      ""
    }
  ),

  private = list(
    # A (\emph{character}) value. The date on which the source was generated or
    # sent.
    date = "",
    # A (\emph{character}) value. The text of the file without modifications.
    source = "",
    # A (\emph{character}) value. Identifier of the \code{Instance}, in this
    # case it will be the path of the file from which the properties are extracted.
    path = "",
    # A (\emph{character}) value. The text of the file with modifications.
    data = "",
    # A (\emph{list}) value. The text of the file with modifications.
    properties = list(),
    # A (\emph{logical}) value. Contains a list of properties extracted from the
    # text that is being processed.
    isValid = TRUE,
    # A (\emph{list}) value. Indicates if the \code{\link{Instance}} is valid or
    # not.
    flowPipes = list(),
    # A (\emph{array}) value. The array contains the \code{\link{GenericPipe}}
    # that can not be executed from that moment.
    banPipes = c()
  )
)
