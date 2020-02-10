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

#' @title Class to find and/or replace the abbreviations on the data field of an Instance
#'
#' @description \code{\link{AbbreviationPipe}} class is responsible for detecting
#' the existing abbreviations in the \strong{data} field of each \code{\link{Instance}}.
#' Identified abbreviations are stored inside the \strong{abbreviation} field of
#' \code{\link{Instance}} class. Moreover if needed, is able to perform inline
#' abbreviations replacement.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \preformatted{
#' AbbreviationPipe$new(propertyName = "abbreviation",
#'                      propertyLanguageName = "language",
#'                      alwaysBeforeDeps = list("GuessLanguagePipe"),
#'                      notAfterDeps = list(),
#'                      replaceAbbreviations = TRUE,
#'                      resourcesAbbreviationsPath = NULL)
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
#' \item{\strong{replaceAbbreviations:}}{
#' (\emph{logical}) indicates if the abbreviations are replaced or not.
#' }
#' \item{\strong{resourcesAbbreviationsPath:}}{
#' (\emph{character}) path of resource files (in json format) containing the
#' correspondence between abbreviations and meaning.
#' }
#' }
#' }
#' }
#'
#' @section Details:
#' \code{\link{AbbreviationPipe}} class requires the resource files (in json format)
#' containing the correspondence between abbreviations and meaning. To this end,
#' the language of the text indicated in the \emph{propertyLanguageName} should
#' be contained in the resource file name (ie. abbrev.xxx.json where xxx is the
#' value defined in the \emph{propertyLanguageName} ). The location of the
#' resources should be defined in the \strong{"resources.abbreviations.path"}
#' field of \emph{\link{bdpar.Options}} variable.
#'
#' @section Note:
#' \code{\link{AbbreviationPipe}} will automatically invalidate the
#' \code{\link{Instance}} whenever the obtained data is empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{GenericPipe}} and implements the
#' \code{pipe} abstract function.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe:}}{
#' preprocesses the \code{\link{Instance}} to obtain/replace the abbreviations.
#' The abbreviations found in the Pipe are added to the list of properties of
#' the \code{\link{Instance}}.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{pipe(instance)}
#' }
#' \item{\emph{Value:}}{
#' the \code{\link{Instance}} with the modifications that have occurred in the pipe.
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
#' \item{\bold{findAbbreviation:}}{
#' checks if the abbreviation is in the data.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{findAbbreviation(data, abbreviation)}
#' }
#' \item{\emph{Value:}}{
#' boolean, depending on whether the abbreviation is in the data.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{data:}}{
#' (\emph{character}) text where abbreviation will be searched.
#' }
#' \item{\strong{abbreviation:}}{
#' (\emph{character}) indicates the abbreviation to find.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{replaceAbbreviation:}}{
#' replaces the abbreviation in the data for the extendedAbbreviation.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{replaceAbbreviation(abbreviation, extendedAbbreviation, data)}
#' }
#' \item{\emph{Value:}}{
#' the data with the abbreviatons replaced.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{abbreviation:}}{
#' (\emph{character}) indicates the abbreviation to replace.
#' }
#' \item{\strong{extendedAbbreviation:}}{
#' (\emph{character}) indicates the string to replace for the abbreviations found.
#' }
#' \item{\strong{data:}}{
#' (\emph{character}) text where abbreviation will be replaced.
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
#' \item{\bold{getResourcesAbbreviationsPath:}}{
#' gets of path of abbreviations resources.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getResourcesAbbreviationsPath()}
#' }
#' \item{\emph{Value:}}{
#' value of path of abbreviations resources.
#' }
#' }
#' }
#'
#' \item{\bold{setResourcesAbbreviationsPath:}}{
#' sets the path of abbreviations resources.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{setResourcesAbbreviationsPath(path)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{path:}}{
#' (\emph{character}) the new value of the path of abbreviations resources.
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
#' \item{\bold{resourcesAbbreviationsPath:}}{
#'  (\emph{character}) path of resource files (in json format) containing the
#' correspondence between abbreviations and meaning.
#' }
#' \item{\bold{replaceAbbreviations:}}{
#'  (\emph{logical}) indicates if the abbreviations are replaced or not.
#' }
#' }
#'
#' @seealso \code{\link{bdpar.Options}}, \code{\link{ContractionPipe}},
#'          \code{\link{File2Pipe}}, \code{\link{FindEmojiPipe}},
#'          \code{\link{FindEmoticonPipe}}, \code{\link{FindHashtagPipe}},
#'          \code{\link{FindUrlPipe}}, \code{\link{FindUserNamePipe}},
#'          \code{\link{GuessDatePipe}}, \code{\link{GuessLanguagePipe}},
#'          \code{\link{Instance}}, \code{\link{InterjectionPipe}},
#'          \code{\link{MeasureLengthPipe}}, \code{\link{GenericPipe}},
#'          \code{\link{ResourceHandler}}, \code{\link{SlangPipe}},
#'          \code{\link{StopWordPipe}}, \code{\link{StoreFileExtPipe}},
#'          \code{\link{TargetAssigningPipe}}, \code{\link{TeeCSVPipe}},
#'          \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import pipeR R6 rlist
#' @export AbbreviationPipe

AbbreviationPipe <- R6Class(

  "AbbreviationPipe",

  inherit = GenericPipe,

  public = list(

    initialize = function(propertyName = "abbreviation",
                          propertyLanguageName = "language",
                          alwaysBeforeDeps = list("GuessLanguagePipe"),
                          notAfterDeps = list(),
                          replaceAbbreviations = TRUE,
                          resourcesAbbreviationsPath = NULL) {

      if (!"character" %in% class(propertyName)) {
        stop("[AbbreviationPipe][initialize][Error] ",
             "Checking the type of the 'propertyName' variable: ",
             class(propertyName))
      }

      if (!"character" %in% class(propertyLanguageName)) {
        stop("[AbbreviationPipe][initialize][Error] ",
             "Checking the type of the 'propertyLanguageName' variable: ",
             class(propertyLanguageName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[AbbreviationPipe][initialize][Error] ",
             "Checking the type of the 'alwaysBeforeDeps' variable: ",
             class(alwaysBeforeDeps))
      }

      if (!"list" %in% class(notAfterDeps)) {
        stop("[AbbreviationPipe][initialize][Error] ",
             "Checking the type of the 'notAfterDeps' variable: ",
             class(notAfterDeps))
      }

      if (!"logical" %in% class(replaceAbbreviations)) {
        stop("[AbbreviationPipe][initialize][Error] ",
             "Checking the type of the 'replaceAbbreviations' variable: ",
             class(replaceAbbreviations))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)

      private$propertyLanguageName <- propertyLanguageName

      if (is.null(resourcesAbbreviationsPath)) {
        if (any(!bdpar.Options$isSpecificOption("resources.abbreviations.path"),
                is.null(bdpar.Options$get("resources.abbreviations.path")))) {
          stop("[AbbreviationPipe][initialize][Error] Path of abbreviations ",
               "resources is neither defined in initialize or in bdpar.Options")
        } else {
          resourcesAbbreviationsPath <- bdpar.Options$get("resources.abbreviations.path")
        }
      }

      if (!"character" %in% class(resourcesAbbreviationsPath)) {
        stop("[AbbreviationPipe][initialize][Error] ",
             "Checking the type of the 'resourcesAbbreviationsPath' variable: ",
             class(resourcesAbbreviationsPath))
      }

      private$resourcesAbbreviationsPath <- resourcesAbbreviationsPath
      private$replaceAbbreviations <- replaceAbbreviations
    },

    pipe = function(instance) {

      if (!"Instance" %in% class(instance)) {
        stop("[AbbreviationPipe][pipe][Error] ",
             "Checking the type of the 'instance' variable: ",
             class(instance))
      }

      languageInstance <- "Unknown"

      languageInstance <- instance$getSpecificProperty(self$getPropertyLanguageName())

      # If the language property is not found, the instance can not be preprocessed
      if (is.null(languageInstance) ||
            is.na(languageInstance) ||
              "Unknown" %in% languageInstance) {

        instance$addProperties(list(),super$getPropertyName())

        warning("[AbbreviationPipe][pipe][Warning] ",
                "The file: " , instance$getPath() ," has not language property")

        return(instance)
      }

      JsonFile <- paste(self$getResourcesAbbreviationsPath(),
                        "/abbrev.",
                        languageInstance,
                        ".json",
                        sep = "")

      jsonData <- Bdpar[["private_fields"]][["resourceHandler"]]$isLoadResource(JsonFile)

      #It is verified that there is a resource associated to the language of the instance
      if (!is.null(jsonData)) {

        #Variable which stores the abbreviations located in the data
        abbreviationsLocated <- list()

        for (abbreviation in names(jsonData)) {

          if (self$findAbbreviation(instance$getData(), abbreviation)) {
            abbreviationsLocated <- list.append(abbreviationsLocated,
                                                  abbreviation)
          }

          if (private$replaceAbbreviations &&
              abbreviation %in% abbreviationsLocated) {

              instance$getData() %>>%
                {self$replaceAbbreviation(abbreviation,
                                            as.character(jsonData[abbreviation]),
                                              .)} %>>%
                textutils::trim() %>>%
                    instance$setData()
          }
        }

        instance$addProperties(paste(abbreviationsLocated), super$getPropertyName())

      } else {

        instance$addProperties(list(), super$getPropertyName())

        warning("[AbbreviationPipe][pipe][Warning] ",
                "The file: " , instance$getPath() , " has not an abbreviationsJsonFile ",
                "to apply to the language ->", languageInstance)

        return(instance)
      }


      if (is.na(instance$getData()) ||
          all(instance$getData() == "") ||
          is.null(instance$getData())) {
        message <- c( "The file: " , instance$getPath() ,
                      " has data empty on pipe Abbreviation")
        instance$addProperties(message, "reasonToInvalidate")

        warning("[AbbreviationPipe][pipe][Warning] ", message)

        instance$invalidate()

        return(instance)
      }

      return(instance)
    },

    findAbbreviation = function(data, abbreviation) {

      if (!"character" %in% class(data)) {
        stop("[AbbreviationPipe][findAbbreviation][Error] ",
             "Checking the type of the 'data' variable: ",
             class(data))
      }

      if (!"character" %in% class(abbreviation)) {
        stop("[AbbreviationPipe][findAbbreviation][Error] ",
             "Checking the type of the 'abbreviation' variable: ",
             class(abbreviation))
      }

      abbreviationEscaped <- rex::escape(abbreviation)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><\u00A1?\u00BF!;:,.'-]|^)(",
                                 abbreviationEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      return(grepl(pattern = rex::regex(regularExpresion), x = data, perl = TRUE))
    },

    replaceAbbreviation = function(abbreviation, extendedAbbreviation, data) {

      if (!"character" %in% class(abbreviation)) {
        stop("[AbbreviationPipe][replaceAbbreviation][Error] ",
             "Checking the type of the 'abbreviation' variable: ",
             class(abbreviation))
      }

      if (!"character" %in% class(extendedAbbreviation)) {
        stop("[AbbreviationPipe][replaceAbbreviation][Error] ",
             "Checking the type of the 'extendedAbbreviation' variable: ",
             class(extendedAbbreviation))
      }

      if (!"character" %in% class(data)) {
        stop("[AbbreviationPipe][replaceAbbreviation][Error] ",
             "Checking the type of the 'data' variable: ",
             class(data))
      }

      abbreviationEscaped <- rex::escape(abbreviation)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><\u00A1?\u00BF!;:,.'-]|^)(",
                                 abbreviationEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      return(gsub(rex::regex(regularExpresion),
                  paste(" ", extendedAbbreviation, " ", sep = ""),
                  data,
                  perl = TRUE))
    },

    getPropertyLanguageName = function() {

      return(private$propertyLanguageName)
    },

    getResourcesAbbreviationsPath = function() {

      return(private$resourcesAbbreviationsPath)
    },

    setResourcesAbbreviationsPath = function(path) {

      if (!"character" %in% class(path)) {
        stop("[AbbreviationPipe][setResourcesAbbreviationsPath][Error] ",
             "Checking the type of the 'path' variable: ",
             class(path))
      }

      private$resourcesAbbreviationsPath <- path

      return()
    }
  ),

  private = list(
    propertyLanguageName = "",
    resourcesAbbreviationsPath = "",
    replaceAbbreviations = TRUE
  )
)
