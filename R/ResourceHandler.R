#
# Bdpar provide a tool to easily build customized data flows to pre-process
# large volumes of information from different sources. To this end, bdpar allows
# to (i) easily use and create new functionalities and (ii) develop new data
# source extractors according to the user needs. Additionally, the package
# provides by default a predefined data flow to extract and preprocess the most
# relevant information (tokens, dates, ... ) from some textual sources (SMS,
# email, tweets, YouTube comments).
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

#' @title Class that handles different types of resources
#'
#' @description Class that handles different types of resources.
#'
#' @section Details:
#' It is a class that allows store the resources that are needed in the
#' \code{\link{GenericPipe}s} to avoid having to repeatedly read from
#' the file. File resources of type json are read and stored in memory.
#'
#' @keywords NULL
#'
#' @import R6
#' @export ResourceHandler

ResourceHandler <- R6Class(

  "ResourceHandler",

  public = list(
    #'
    #' @description Creates a \code{\link{ResourceHandler}} object.
    #'
    initialize = function() { },
    #'
    #' @description From the resource path, it is checked if they have already
    #' been loaded. In this case, the list of the requested resource is returned.
    #' Otherwise, the resource variable is added to the list of resources, and
    #' the resource list is returned. In the event that the resource file does
    #' not exist, NULL is returned.
    #'
    #' @param pathResource A (\emph{character}) value. The resource file path.
    #'
    #' @return The resources list is returned, if they exist.
    #'
    #' @importFrom rlist list.append
    #'
    isLoadResource = function(pathResource) {

      if (!"character" %in% class(pathResource)) {
        bdpar.log(message = paste0("Checking the type of the 'pathResource' variable: ",
                                   class(pathResource)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "isLoadResource")
      }

      if (pathResource %in% self$getNamesResources()) {
        self$getResources()[[pathResource]]
      } else {
        if (file.exists(pathResource)) {

          jsonData <- rjson::fromJSON(file = pathResource)
          self$setResources(list.append(self$getResources(), jsonData))
          names(private$resources)[length(self$getResources())] <- pathResource

          self$getResources()[[pathResource]]

        } else {
          NULL
        }
      }
    },
    #'
    #' @description Gets of resources variable.
    #'
    #' @return The value of resources variable.
    #'
    getResources = function() {
      private$resources
    },
    #'
    #' @description Sets of resources variable.
    #'
    #' @param resources The new value of resources.
    #'
    setResources = function(resources) {
      private$resources <- resources
    },
    #'
    #' @description Gets of names of resources
    #'
    #' @return Value of names of resources.
    #'
    getNamesResources = function() {
      names(self$getResources())
    }
  ),

  private = list(
    # A (\emph{list}) value. Stores the lists of the different types of resources.
    resources = list()
  )
)
