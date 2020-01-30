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

#' @title Class to find and/or remove the interjections on the data field of an Instance
#'
#' @description \code{\link{InterjectionPipe}} class is responsible for detecting
#' the existing interjections in the \strong{data} field of each \code{\link{Instance}}.
#' Identified interjections are stored inside the \strong{interjection} field of
#' \code{\link{Instance}} class. Moreover if needed, is able to perform inline
#' interjections removement.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \preformatted{
#' InterjectionPipe$new(propertyName = "interjection",
#'                      propertyLanguageName = "language",
#'                      alwaysBeforeDeps = list("GuessLanguagePipe"),
#'                      notAfterDeps = list(),
#'                      removeInterjections = TRUE)
#' }
#' \itemize{
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{propertyName:}}{
#' (\emph{character}) name of the property associated with the Pipe.
#' }
#' \item{\strong{propertyLanguageName:}}{
#' (\emph{character}) name of the language property.
#' }
#' \item{\strong{alwaysBeforeDeps:}}{
#' (\emph{list}) the dependences alwaysBefore (Pipes that must be executed before this
#' one).
#' }
#' \item{\strong{notAfterDeps:}}{
#' (\emph{list}) the dependences notAfter (Pipes that cannot be executed after this one).
#' }
#' \item{\strong{removeInterjections:}}{
#' (\emph{logical}) indicates if the interjections are removed or not.
#' }
#' }
#' }
#' }
#'
#' @section Details:
#' \code{\link{InterjectionPipe}} class requires the resource files (in json format)
#' containing the list of interjections. To this end, the language of the text
#' indicated in the \emph{propertyLanguageName} should be contained in the
#' resource file name (ie. interj.xxx.json where xxx is the value defined in the
#' \emph{propertyLanguageName} ). The location of the resources should be
#' defined in the \strong{"resources.interjections.path"} field of
#' \emph{\link{bdpar.Options}} variable.
#'
#' @section Note:
#' \code{\link{InterjectionPipe}} will automatically invalidate the
#' \code{\link{Instance}} whenever the obtained data is empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe}}{
#' Preprocesses the \code{\link{Instance}} to obtain/remove the interjections.
#' The interjections found in the Pipe are added to the list of properties of
#' the \code{\link{Instance}}.
#' \itemize{
#' \item{\emph{Usage:}}{
#'
#' \code{pipe(instance)}
#' }
#' \item{\emph{Value:}}{
#' the \code{\link{Instance}} with the modifications that have occurred in the Pipe.
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
#' \item{\bold{findInterjection:}}{
#' checks if the interjection is in the data.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{findInterjection(data, interjection)}{}
#' }
#' \item{\emph{Value:}}{
#' boolean, depending on whether the interjection is on the data.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{data:}}{
#' (\emph{character}) text where interjection will be replaced.
#' }
#' \item{\strong{interjection:}}{
#' (\emph{character}) indicate the interjection to find.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{removeInterjection:}}{
#' removes the interjection in the data.
#' \itemize{
#' \item{\emph{Usage:}}{
#'
#' \code{removeInterjection(interjection, data)}
#' }
#' \item{\emph{Value:}}{
#' the data with interjection removed.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{interjection:}}{
#' (\emph{character}) indicates the interjection to remove.
#' }
#' \item{\strong{data:}}{
#' (\emph{character}) text where interjection will be removed.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getPropertyLanguageName:}}{
#' gets of name of property language.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getPropertyLanguageName()}
#' }
#' \item{\emph{Value:}}{
#' value of name of property language.
#' }
#' }
#' }
#'
#' \item{\bold{getResourcesInterjectionsPath:}}{
#' gets of path of interjections resources.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getResourcesInterjectionsPath()}
#' }
#' \item{\emph{Value:}}{
#' value of path of interjections resources.
#' }
#' }
#' }
#'
#' \item{\bold{setResourcesInterjectionsPath:}}{
#' sets the path of interjections resources.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{setResourcesInterjectionsPath(path)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{path:}}{
#' (\emph{character}) the new value of the path of interjections resources.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{propertyLanguageName:}}{
#'  (\emph{character}) the name of property about language.
#' }
#' \item{\bold{resourcesInterjectionsPath:}}{
#'  (\emph{character}) the path where are the resources.
#' }
#' \item{\bold{removeInterjections:}}{
#'  (\emph{logical}) indicates if the interjections are removed or not.
#' }
#' }
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{bdpar.Options}},
#'          \code{\link{ContractionPipe}}, \code{\link{File2Pipe}},
#'          \code{\link{FindEmojiPipe}}, \code{\link{FindEmoticonPipe}},
#'          \code{\link{FindHashtagPipe}}, \code{\link{FindUrlPipe}},
#'          \code{\link{FindUserNamePipe}}, \code{\link{GuessDatePipe}},
#'          \code{\link{GuessLanguagePipe}}, \code{\link{Instance}},
#'          \code{\link{MeasureLengthPipe}}, \code{\link{PipeGeneric}},
#'          \code{\link{ResourceHandler}}, \code{\link{SlangPipe}},
#'          \code{\link{StopWordPipe}}, \code{\link{StoreFileExtPipe}},
#'          \code{\link{TargetAssigningPipe}}, \code{\link{TeeCSVPipe}},
#'          \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import pipeR R6 rlist
#' @export InterjectionPipe

InterjectionPipe <- R6Class(

  "InterjectionPipe",

  inherit = PipeGeneric,

  public = list(

    initialize = function(propertyName = "interjection",
                          propertyLanguageName = "language",
                          alwaysBeforeDeps = list("GuessLanguagePipe"),
                          notAfterDeps = list(),
                          removeInterjections = TRUE,
                          resourcesInterjectionsPath = NULL) {

      if (!"character" %in% class(propertyName)) {
        stop("[InterjectionPipe][initialize][Error] ",
             "Checking the type of the 'propertyName' variable: ",
             class(propertyName))
      }

      if (!"character" %in% class(propertyLanguageName)) {
        stop("[InterjectionPipe][initialize][Error] ",
             "Checking the type of the 'propertyLanguageName' variable: ",
             class(propertyLanguageName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[InterjectionPipe][initialize][Error] ",
             "Checking the type of the 'alwaysBeforeDeps' variable: ",
             class(alwaysBeforeDeps))
      }

      if (!"list" %in% class(notAfterDeps)) {
        stop("[InterjectionPipe][initialize][Error] ",
             "Checking the type of the 'notAfterDeps' variable: ",
             class(notAfterDeps))
      }

      if (!"logical" %in% class(removeInterjections)) {
        stop("[InterjectionPipe][initialize][Error] ",
             "Checking the type of the 'removeInterjections' variable: ",
             class(removeInterjections))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)

      private$propertyLanguageName <- propertyLanguageName

      if (is.null(resourcesInterjectionsPath)) {
        if (any(!bdpar.Options$isSpecificOption("resources.interjections.path"),
                is.null(bdpar.Options$get("resources.interjections.path")))) {
          stop("[InterjectionPipe][initialize][Error] Path of interjections ",
               "resources is neither defined in initialize or in bdpar.Options")
        } else {
          resourcesInterjectionsPath <- bdpar.Options$get("resources.interjections.path")
        }
      }

      if (!"character" %in% class(resourcesInterjectionsPath)) {
        stop("[InterjectionPipe][initialize][Error] ",
             "Checking the type of the 'resourcesInterjectionsPath' variable: ",
             class(resourcesInterjectionsPath))
      }

      private$resourcesInterjectionsPath <- resourcesInterjectionsPath

      private$removeInterjections <- removeInterjections
    },

    pipe = function(instance) {

      if (!"Instance" %in% class(instance)) {
        stop("[InterjectionPipe][pipe][Error] ",
             "Checking the type of the 'instance' variable: ",
             class(instance))
      }

      instance$addFlowPipes("InterjectionPipe")

      if (!instance$checkCompatibility("InterjectionPipe", self$getAlwaysBeforeDeps())) {
        stop("[InterjectionPipe][pipe][Error] Bad compatibility between Pipes")
      }

      instance$addBanPipes(unlist(super$getNotAfterDeps()))

      languageInstance <- "Unknown"

      languageInstance <- instance$getSpecificProperty(self$getPropertyLanguageName())

      #It is verified that there is a resource associated to the language of the instance
      if (is.null(languageInstance) ||
            is.na(languageInstance) ||
              "Unknown" %in% languageInstance) {

        instance$addProperties(list(), super$getPropertyName())

        message <- c( "The file: " , instance$getPath() , " has not language property")

        warning("[InterjectionPipe][pipe][Warning] ", message)

        return(instance)
      }

      JsonFile <- paste(self$getResourcesInterjectionsPath(),
                        "/interj.",
                        languageInstance,
                        ".json",
                        sep = "")

      jsonData <- Bdpar[["private_fields"]][["resourceHandler"]]$isLoadResource(JsonFile)

      if (!is.null(jsonData)) {

        #Variable which stores the interjections located in the data
        interjectionsLocated <- list()

        for (interjection in jsonData) {

          if (self$findInterjection(instance$getData(), interjection)) {
            interjectionsLocated <- list.append(interjectionsLocated, interjection)
          }

          if (private$removeInterjections &&
              interjection %in% interjectionsLocated) {

            instance$getData() %>>%
              {self$removeInterjection(interjection, .)} %>>%
                textutils::trim() %>>%
                  instance$setData()
          }
        }

        instance$addProperties(paste(interjectionsLocated), super$getPropertyName())

      } else {

        instance$addProperties(list(), super$getPropertyName())

        warning("[InterjectionPipe][pipe][Warning] ",
            "The file: " , instance$getPath() , " has not an interjectionsJsonFile ",
            "to apply to the language ->", languageInstance)

        return(instance)
      }

      if (is.na(instance$getData()) ||
          all(instance$getData() == "") ||
          is.null(instance$getData())) {

        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Interjection")

        instance$addProperties(message, "reasonToInvalidate")

        warning("[InterjectionPipe][pipe][Warning] ", message)

        instance$invalidate()

        return(instance)
      }

      return(instance)
    },

    findInterjection = function(data, interjection) {

      if (!"character" %in% class(data)) {
        stop("[InterjectionPipe][findInterjection][Error] ",
             "Checking the type of the 'data' variable: ",
             class(data))
      }

      if (!"character" %in% class(interjection)) {
        stop("[InterjectionPipe][findInterjection][Error] ",
             "Checking the type of the 'interjection' variable: ",
             class(interjection))
      }

      interjectionEscaped <- rex::escape(interjection)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><\u00A1?\u00BF!;:,.'-]|^)([\u00A1]*(",
                                 interjectionEscaped,
                                 ")[!]*)[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      return(grepl(pattern = rex::regex(regularExpresion), x = data, perl = TRUE))
    },

    removeInterjection = function(interjection, data) {

      if (!"character" %in% class(interjection)) {
        stop("[InterjectionPipe][removeInterjection][Error] ",
             "Checking the type of the 'interjection' variable: ",
             class(interjection))
      }


      if (!"character" %in% class(data)) {
        stop("[InterjectionPipe][removeInterjection][Error] ",
             "Checking the type of the 'data' variable: ",
             class(data))
      }

      interjectionEscaped <- rex::escape(interjection)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><\u00A1?\u00BF!;:,.'-]|^)([\u00A1]*(",
                                 interjectionEscaped,
                                 ")[!]*)[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      return(gsub(rex::regex(regularExpresion), "", data , perl = TRUE))

    },

    getPropertyLanguageName = function() {

      return(private$propertyLanguageName)
    },

    getResourcesInterjectionsPath = function() {

      return(private$resourcesInterjectionsPath)
    },

    setResourcesInterjectionsPath = function(path) {

      if (!"character" %in% class(path)) {
        stop("[InterjectionPipe][setResourcesInterjectionsPath][Error] ",
             "Checking the type of the 'path' variable: ",
             class(path))
      }

      private$resourcesInterjectionsPath <- path

      return()
    }
  ),

  private = list(
    propertyLanguageName = "",
    resourcesInterjectionsPath = "",
    removeInterjections = TRUE
  )
)
