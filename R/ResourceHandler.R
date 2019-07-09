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

#' @title Class that handles different types of resources
#'
#' @description Class that handles different types of resources.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \code{ResourceHandler$new()}
#'
#' @section Details:
#' It is a class that allows store the resources that are needed
#' in the Pipes to avoid having to repeatedly read from the file. File resources
#' of type json are read and stored in memory.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{isLoadResource:}}{
#' from the resource path, it is checked if they have already been loaded. In
#' this case, the list of the requested resource is returned. Otherwise, the
#' resource variable is added to the list of resources, and the resource list is
#' returned. In the event that the resource file does not exist, NULL is returned.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{isLoadResource(pathResource)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{pathResource:}}{
#' (\emph{character}) resource file path.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getResources:}}{
#' gets of resources variable.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getResources()}
#' }
#' \item{\emph{Value:}}{
#' value of resources variable.
#' }
#' }
#' }
#'
#' \item{\bold{setResources:}}{
#' sets of resources.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{setResources(resources)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{resources:}}{
#' (\emph{list}) the new value of resources.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getNamesResources:}}{
#' gets of names of resources.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getNamesResources()}
#' }
#' \item{\emph{Value:}}{
#' value of names of resources.
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{resources:}}{
#'  (\emph{list}) variable that stores the lists of the different types of resources.
#' }
#' }
#'
#' @keywords NULL
#'
#' @import R6 rlist
#' @export ResourceHandler

ResourceHandler <- R6Class(

  "ResourceHandler",

  public = list(

    initialize = function() {
      if (!requireNamespace("rjson", quietly = TRUE)) {
        stop("[ResourceHandler][initialize][Error]
                Package \"rjson\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }
    },

    isLoadResource = function(pathResource) {

      if (!"character" %in% class(pathResource)) {
        stop("[ResourceHandler][isLoadResource][Error]
                Checking the type of the variable: pathResource ",
                  class(pathResource));
      }

      if (pathResource %in% self$getNamesResources()) {

        return(self$getResources()[[pathResource]])

      } else {

        if (file.exists(pathResource)) {

          jsonData <- rjson::fromJSON(file = pathResource)
          self$setResources(list.append(self$getResources(), jsonData))
          names(private$resources)[length(self$getResources())] <- pathResource

          return(self$getResources()[[pathResource]])

        } else {
          return(NULL)
        }
      }
      return(listResource)
    },

    getResources = function() {

      return(private$resources)
    },

    setResources = function(resources) {

      private$resources <- resources

      return()
    },

    getNamesResources = function() {

      return(names(self$getResources()))
    }
  ),

  private = list(
    resources = list()
  )
)
