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
#'                      notAfterDeps = list())
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
#' resources should defined in the \emph{resourcesPath} section of the
#' configuration file.
#'
#' \strong{[resourcesPath]}
#'
#' resourcesAbbreviationsPath = \emph{<<resources_abbreviations_path>>}
#'
#' @section Note:
#' \code{\link{AbbreviationPipe}} will automatically invalidate the
#' \code{\link{Instance}} whenever the obtained data is empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe:}}{
#' preprocesses the \code{\link{Instance}} to obtain/replace the abbreviations.
#' The abbreviations found in the Pipe are added to the list of properties of
#' the \code{\link{Instance}}. If the \emph{replaceAbbreviations} parameter is
#' TRUE, the \code{\link{Instance}} data will be modified by replacing the
#' abbreviations found.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{pipe(instance, replaceAbbreviations = TRUE)}
#' }
#' \item{\emph{Value:}}{
#' the \code{\link{Instance}} with the modifications that have occurred in the pipe.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{instance:}}{
#' (\emph{Instance}) \code{\link{Instance}} to preproccess.
#' }
#' \item{\strong{replaceAbbreviations:}}{
#' (\emph{logical}) indicates if the abbreviations are replaced or not.
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
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{propertyLanguageName:}}{
#'  (\emph{character}) the name of property about language.
#' }
#' \item{\bold{resourcesAbbreviationsPath:}}{
#'  (\emph{character}) the path where are the resources.
#' }
#' }
#'
#' @seealso \code{\link{ContractionPipe}}, \code{\link{File2Pipe}},
#'          \code{\link{FindEmojiPipe}}, \code{\link{FindEmoticonPipe}},
#'          \code{\link{FindHashtagPipe}}, \code{\link{FindUrlPipe}},
#'          \code{\link{FindUserNamePipe}}, \code{\link{GuessDatePipe}},
#'          \code{\link{GuessLanguagePipe}}, \code{\link{Instance}},
#'          \code{\link{InterjectionPipe}}, \code{\link{MeasureLengthPipe}},
#'          \code{\link{PipeGeneric}}, \code{\link{ResourceHandler}},
#'          \code{\link{SlangPipe}}, \code{\link{StopWordPipe}},
#'          \code{\link{StoreFileExtPipe}}, \code{\link{TargetAssigningPipe}},
#'          \code{\link{TeeCSVPipe}}, \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import pipeR R6 rlist
#' @export AbbreviationPipe

AbbreviationPipe <- R6Class(

  "AbbreviationPipe",

  inherit = PipeGeneric,

  public = list(

    initialize = function(propertyName = "abbreviation",
                          propertyLanguageName = "language",
                          alwaysBeforeDeps = list("GuessLanguagePipe"),
                          notAfterDeps = list()) {

      if (!requireNamespace("rex", quietly = TRUE)) {
        stop("[AbbreviationPipe][initialize][Error]
                Package \"rex\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!requireNamespace("textutils", quietly = TRUE)) {
        stop("[AbbreviationPipe][initialize][Error]
                Package \"textutils\" needed for this class to work.
                  Please install it.",
                     call. = FALSE)
      }

      if (!"character" %in% class(propertyName)) {
        stop("[AbbreviationPipe][initialize][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }

      if (!"character" %in% class(propertyLanguageName)) {
        stop("[AbbreviationPipe][initialize][Error]
                Checking the type of the variable: propertyLanguageName ",
                  class(propertyLanguageName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[AbbreviationPipe][initialize][Error]
                Checking the type of the variable: alwaysBeforeDeps ",
                  class(alwaysBeforeDeps))
      }

      if (!"list" %in% class(notAfterDeps)) {
        stop("[AbbreviationPipe][initialize][Error]
                Checking the type of the variable: notAfterDeps ",
                  Wclass(notAfterDeps))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)

      private$propertyLanguageName <- propertyLanguageName

      private$resourcesAbbreviationsPath <- read.ini(Bdpar[["private_fields"]][["configurationFilePath"]])$resourcesPath$resourcesAbbreviationsPath

    },

    pipe = function(instance, replaceAbbreviations = TRUE) {

      if (!"Instance" %in% class(instance)) {
        stop("[AbbreviationPipe][pipe][Error]
                Checking the type of the variable: instance ",
                  class(instance))
      }

      if (!"logical" %in% class(replaceAbbreviations)) {
        stop("[AbbreviationPipe][pipe][Error]
                Checking the type of the variable: replaceAbbreviations ",
                  class(replaceAbbreviations))
      }

      instance$addFlowPipes("AbbreviationPipe")

      if (!instance$checkCompatibility("AbbreviationPipe", self$getAlwaysBeforeDeps())) {
        stop("[AbbreviationPipe][pipe][Error] Bad compatibility between Pipes.")
      }

      instance$addBanPipes(unlist(super$getNotAfterDeps()))

      languageInstance <- "Unknown"

      languageInstance <- instance$getSpecificProperty(self$getPropertyLanguageName())

      # If the language property is not found, the instance can not be preprocessed
      if (is.null(languageInstance) ||
            is.na(languageInstance) ||
              "Unknown" %in% languageInstance) {

        instance$addProperties(list(),super$getPropertyName())

        cat("[AbbreviationPipe][pipe][Warning] ",
            "The file: " , instance$getPath() ," has not language property\n")

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

          if (replaceAbbreviations && abbreviation %in% abbreviationsLocated) {

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

        cat("[AbbreviationPipe][pipe][Warning] ",
            "The file: " , instance$getPath() , " has not an abbreviationsJsonFile ",
            "to apply to the language ->", languageInstance, " \n")

        return(instance)
      }


      if (is.na(instance$getData()) ||
          all(instance$getData() == "") ||
          is.null(instance$getData())) {
        message <- c( "The file: " , instance$getPath() ,
                      " has data empty on pipe Abbreviation")
        instance$addProperties(message, "reasonToInvalidate")

        cat("[AbbreviationPipe][pipe][Warning] ", message, " \n")

        instance$invalidate()
        return(instance)
      }

      return(instance)
    },

    findAbbreviation = function(data, abbreviation) {

      if (!"character" %in% class(data)) {
        stop("[AbbreviationPipe][findAbbreviation][Error]
                Checking the type of the variable: data ",
                  class(data))
      }

      if (!"character" %in% class(abbreviation)) {
        stop("[AbbreviationPipe][findAbbreviation][Error]
                Checking the type of the variable: abbreviation ",
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
        stop("[AbbreviationPipe][replaceAbbreviation][Error]
                Checking the type of the variable: abbreviation ",
                  class(abbreviation))
      }

      if (!"character" %in% class(extendedAbbreviation)) {
        stop("[AbbreviationPipe][replaceAbbreviation][Error]
                Checking the type of the variable: extendedAbbreviation ",
                  class(extendedAbbreviation))
      }

      if (!"character" %in% class(data)) {
        stop("[AbbreviationPipe][replaceAbbreviation][Error]
                Checking the type of the variable: data ",
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
    }
  ),

  private = list(
    propertyLanguageName = "",
    resourcesAbbreviationsPath = ""
  )
)
