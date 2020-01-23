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


#' @title Class to find and/or replace the slangs on the data field of an Instance
#'
#' @description \code{\link{SlangPipe}} class is responsible for detecting
#' the existing slangs in the \strong{data} field of each \code{\link{Instance}}.
#' Identified slangs are stored inside the \strong{slang} field of
#' \code{\link{Instance}} class. Moreover if needed, is able to perform inline
#' slangs replacement.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \preformatted{
#' SlangPipe$new(propertyName = "langpropname",
#'               propertyLanguageName = "language",
#'               alwaysBeforeDeps = list("GuessLanguagePipe"),
#'               notAfterDeps = list(),
#'               replaceSlangs = TRUE)
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
#' \item{\strong{replaceSlangs:}}{
#' (\emph{logical}) indicates if the slangs are replace or not.
#' }
#' }
#' }
#' }

#' @section Details:
#' \code{\link{SlangPipe}} class requires the resource files (in json format)
#' containing the correspondence between slangs and meaning. To this end,
#' the language of the text indicated in the \emph{propertyLanguageName} should
#' be contained in the resource file name (ie. slang.xxx.json where xxx is the
#' value defined in the \emph{propertyLanguageName} ). The location of the
#' resources should defined in the \emph{resourcesPath} section of the
#' configuration file.
#'
#' \strong{[resourcesPath]}
#'
#' resourcesSlangsPath = \emph{<<resources_slangs_path>>}
#'
#' @section Note:
#' \code{\link{SlangPipe}} will automatically invalidate the
#' \code{\link{Instance}} whenever the obtained data is empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe:}}{
#' preprocesses the \code{\link{Instance}} to obtain/replace the slangs.
#' The slangs found in the Pipe are added to the list of properties of the
#' \code{\link{Instance}}.
#' \itemize{
#' \item{\emph{Usage:}}{
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
#' \item{\bold{findSlang:}}{
#' checks if the slang is in the data.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{findSlang(data, slang)}
#' }
#' \item{\emph{Value:}}{
#' boolean, depending on whether the slang is on the data.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{data:}}{
#' (\emph{character}) text where slang will be searched.
#' \item{\strong{slang:}}{
#' (\emph{character}) indicates the slang to find.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{replaceSlang:}}{
#' replaces the slang in the data for the extendedSlang.
#' \itemize{
#' \item{\emph{Usage:}}{
#'
#' \code{replaceSlang(slang, extendedSlang, data)}
#' }
#' \item{\emph{Value:}}{
#' the data with slangs replaced.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{slang:}}{
#' (\emph{character}) indicates the slang to replace.
#' }
#' \item{\strong{extendedSlang:}}{
#' (\emph{character}) indicates the string to replace for the slangs found.
#' }
#' \item{\strong{data:}}{
#' (\emph{character}) text where slang will be replaced.
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
#'
#' \code{getPropertyLanguageName()}
#' }
#' \item{\emph{Value:}}{
#' value of name of property language.
#' }
#' }
#' }
#'
#' \item{\bold{getResourcesSlangsPath:}}{
#' gets of path of slangs resources.
#' \itemize{
#' \item{\emph{Usage:}}{
#'
#' \code{getResourcesSlangsPath()}
#' }
#' \item{\emph{Value:}}{
#'
#' value of path of slangs resources.
#' }
#' }
#' }
#'
#' \item{\bold{setResourcesSlangsPath:}}{
#' sets the path of slangs resources.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{setResourcesSlangsPath(path)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{path:}}{
#' (\emph{character}) the new value of the path of slangs resources.
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
#' \item{\bold{resourcesSlangsPath:}}{
#'  (\emph{character}) the path where are the resources.
#' }
#' \item{\bold{replaceSlangs:}}{
#'  (\emph{logical}) indicates if the slangs are replace or not.
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
#'          \code{\link{ResourceHandler}}, \code{\link{StopWordPipe}},
#'          \code{\link{StoreFileExtPipe}}, \code{\link{TargetAssigningPipe}},
#'          \code{\link{TeeCSVPipe}}, \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import ini pipeR R6 rlist
#' @export SlangPipe

SlangPipe <- R6Class(

  "SlangPipe",

  inherit = PipeGeneric,

  public = list(

    initialize = function(propertyName = "langpropname",
                          propertyLanguageName = "language",
                          alwaysBeforeDeps = list("GuessLanguagePipe"),
                          notAfterDeps = list(),
                          replaceSlangs = TRUE) {

      if (!requireNamespace("rex", quietly = TRUE)) {
        stop("[SlangPipe][initialize][Error]
                Package \"rex\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!requireNamespace("textutils", quietly = TRUE)) {
        stop("[SlangPipe][initialize][Error]
                Package \"textutils\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!"character" %in% class(propertyName)) {
        stop("[SlangPipe][initialize][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }

      if (!"character" %in% class(propertyLanguageName)) {
        stop("[SlangPipe][initialize][Error]
                Checking the type of the variable: propertyLanguageName ",
                  class(propertyLanguageName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[SlangPipe][initialize][Error]
                Checking the type of the variable: alwaysBeforeDeps ",
                  class(alwaysBeforeDeps))
      }

      if (!"list" %in% class(notAfterDeps)) {
        stop("[SlangPipe][initialize][Error]
                Checking the type of the variable: notAfterDeps ",
                  class(notAfterDeps))
      }

      if (!"logical" %in% class(replaceSlangs)) {
        stop("[SlangPipe][initialize][Error]
                Checking the type of the variable: replaceSlangs ",
                  class(replaceSlangs))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)

      private$propertyLanguageName <- propertyLanguageName

      private$resourcesSlangsPath <- read.ini(Bdpar[["private_fields"]][["configurationFilePath"]])$resources$pathResourcesSlangsPath

      private$replaceSlangs <- replaceSlangs
    },

    pipe = function(instance) {

      if (!"Instance" %in% class(instance)) {
        stop("[SlangPipe][pipe][Error]
               Checking the type of the variable: instance ",
                class(instance))
      }

      instance$addFlowPipes("SlangPipe")

      if (!instance$checkCompatibility("SlangPipe", self$getAlwaysBeforeDeps())) {
        stop("[SlangPipe][pipe][Error] Bad compatibility between Pipes.")
      }

      instance$addBanPipes(unlist(super$getNotAfterDeps()))

      languageInstance <- "Unknown"

      languageInstance <- instance$getSpecificProperty(self$getPropertyLanguageName())

      #It is verified that there is a resource associated to the language of the instance
      if (is.null(languageInstance) ||
            is.na(languageInstance) ||
              "Unknown" %in% languageInstance) {

        instance$addProperties(list(), super$getPropertyName())

        message <- c( "The file: ", instance$getPath(), " has not language property")

        warning("[SlangPipe][pipe][Warning] ", message, " \n")

        return(instance)
      }

      JsonFile <- paste(self$getResourcesSlangsPath(),
                        "/slang.",
                        languageInstance,
                        ".json",
                        sep = "")

      jsonData <- Bdpar[["private_fields"]][["resourceHandler"]]$isLoadResource(JsonFile)

      if (!is.null(jsonData)) {

        #Variable which stores the Slangs located in the data
        slangsLocated <- list()

        for (slang in names(jsonData)) {

          if (self$findSlang(instance$getData(), slang)) {
            slangsLocated <- list.append(slangsLocated, slang)
          }

          if (private$replaceSlangs && slang %in% slangsLocated) {
            instance$getData() %>>%
              {self$replaceSlang(slang, as.character(jsonData[slang]), .)} %>>%
                textutils::trim() %>>%
                  instance$setData()
          }
        }

        instance$addProperties(paste(slangsLocated), super$getPropertyName())

      } else {

        instance$addProperties(list(), super$getPropertyName())

        message <- c( "The file: ", instance$getPath(), " has not an SlangsJsonFile to apply to the language-> ", languageInstance )

        warning("[SlangPipe][pipe][Warning] ", message, " \n")


        return(instance)
      }

      if (is.na(instance$getData()) ||
          all(instance$getData() == "") ||
          is.null(instance$getData())) {
        message <- c( "The file: ", instance$getPath(), " has data empty on pipe Slang")

        instance$addProperties(message, "reasonToInvalidate")

        warning("[SlangPipe][pipe][Warning] ", message, " \n")

        instance$invalidate()

        return(instance)
      }

      return(instance)
    },

    findSlang = function(data, slang) {

      if (!"character" %in% class(data)) {
        stop("[SlangPipe][findSlang][Error]
                Checking the type of the variable: data ",
                  class(data))
      }

      if (!"character" %in% class(slang)) {
        stop("[SlangPipe][findSlang][Error]
                Checking the type of the variable: slang ",
                  class(slang))
      }

      slangEscaped <- rex::escape(slang)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><\u00A1?\u00BF!;:,.']|^)(",
                                 slangEscaped,
                                 ")[;:?\"!,.'>]*(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      return(grepl(pattern = rex::regex(regularExpresion), x = data, perl = T, ignore.case = TRUE))
    },

    replaceSlang = function(slang, extendedSlang, data) {

      if (!"character" %in% class(slang)) {
        stop("[SlangPipe][replaceSlang][Error]
                Checking the type of the variable: slang ",
                  class(slang))
      }

      if (!"character" %in% class(extendedSlang)) {
        stop("[SlangPipe][replaceSlang][Error]
                Checking the type of the variable: extendedSlang ",
                  class(extendedSlang))
      }

      if (!"character" %in% class(data)) {
        stop("[SlangPipe][replaceSlang][Error]
                Checking the type of the variable: data ",
                  class(data))
      }

      slangEscaped <- rex::escape(slang)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><\u00A1?\u00BF!;:,.']|^)(",
                                 slangEscaped,
                                 ")[;:?\"!,.'>]*(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      return(gsub(rex::regex(regularExpresion),
                          paste(" ", extendedSlang, " ", sep = ""), data, perl = T, ignore.case = TRUE))

    },

    getPropertyLanguageName = function() {

      return(private$propertyLanguageName)
    },

    getResourcesSlangsPath = function() {

      return(private$resourcesSlangsPath)
    },

    setResourcesSlangsPath = function(path) {

      if (!"character" %in% class(path)) {
        stop("[SlangPipe][setResourcesSlangsPath][Error]
                Checking the type of the variable: path ",
                  class(path))
      }

      private$resourcesSlangsPath <- path

      return()
    }
  ),

  private = list(
    propertyLanguageName = "",
    resourcesSlangsPath = "",
    replaceSlangs = TRUE
  )
)
