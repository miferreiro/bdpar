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

#' @title Class to guess the language of an Instance
#'
#' @description This class allows guess the language by using language detector of library
#' cld2. Creates the \strong{language} property which indicates the idiom text.
#' Optionally, it is possible to choose the language provided by Twitter.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \preformatted{
#' GuessLanguagePipe$new(propertyName = "language",
#'                       alwaysBeforeDeps = list("StoreFileExtPipe",
#'                                               "TargetAssigningPipe"),
#'                       notAfterDeps = list(),
#'                       languageTwitter = TRUE)
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
#' \item{\strong{languageTwitter:}}{
#' (\emph{logical}) indicates whether for the Instances of type twtid the language that
#' returns the api is obtained or the detector is applied.
#' }
#' }
#' }
#' }
#'
#' @section Details:
#' To obtain the language of the tweets, it will be verified that there is a
#' json file with the information stored in memory. On the other hand, it is
#' necessary define the \strong{"cache.twitter.path"} field of
#' \emph{\link{bdpar.Options}} variable to know where the
#' information of tweets are saved.
#'
#' @section Note:
#' The Pipe will invalidate the \code{\link{Instance}} if the language of the data
#' can not be detect.
#'
#' @section Inherit:
#' This class inherits from \code{\link{GenericPipe}} and implements the
#' \code{pipe} abstract function.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe:}}{
#' preprocesses the \code{\link{Instance}} to obtain the language of the data.
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
#' \item{\bold{getLanguage:}}{
#' guesses the language of data.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getLanguage(data)}
#' }
#' \item{\emph{Value:}}{
#' the language guesser. Format: see ISO 639-3:2007.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{data:}}{
#' (\emph{character}) text to guess the language.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{languageTwitter:}}{
#' (\emph{logical}) indicates whether for the Instances of type twtid the language that
#' returns the api is obtained or the detector is applied.
#' }
#' }
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{bdpar.Options}},
#'          \code{\link{ContractionPipe}}, \code{\link{File2Pipe}},
#'          \code{\link{FindEmojiPipe}}, \code{\link{FindEmoticonPipe}},
#'          \code{\link{FindHashtagPipe}}, \code{\link{FindUrlPipe}},
#'          \code{\link{FindUserNamePipe}}, \code{\link{GuessDatePipe}},
#'          \code{\link{Instance}}, \code{\link{InterjectionPipe}},
#'          \code{\link{MeasureLengthPipe}}, \code{\link{GenericPipe}},
#'          \code{\link{SlangPipe}}, \code{\link{StopWordPipe}},
#'          \code{\link{StoreFileExtPipe}}, \code{\link{TargetAssigningPipe}},
#'          \code{\link{TeeCSVPipe}}, \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import pipeR R6
#' @export GuessLanguagePipe

GuessLanguagePipe <- R6Class(

  "GuessLanguagePipe",

  inherit = GenericPipe,

  public = list(

    initialize = function(propertyName = "language",
                          alwaysBeforeDeps = list("StoreFileExtPipe",
                                                  "TargetAssigningPipe"),
                          notAfterDeps = list(),
                          languageTwitter = TRUE) {

      if (!"character" %in% class(propertyName)) {
        stop("[GuessLanguagePipe][initialize][Error] ",
             "Checking the type of the 'propertyName' variable: ",
             class(propertyName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[GuessLanguagePipe][initialize][Error] ",
             "Checking the type of the 'alwaysBeforeDeps' variable: ",
             class(alwaysBeforeDeps))
      }

      if (!"list" %in% class(notAfterDeps)) {
        stop("[GuessLanguagePipe][initialize][Error] ",
             "Checking the type of the 'notAfterDeps' variable: ",
             class(notAfterDeps))
      }

      if (!"logical" %in% class(languageTwitter)) {
        stop("[GuessLanguagePipe][initialize][Error] ",
             "Checking the type of the 'languageTwitter' variable: ",
             class(languageTwitter))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      private$languageTwitter <- languageTwitter
    },

    pipe = function(instance) {

      if (!"Instance" %in% class(instance)) {
        stop("[GuessLanguagePipe][pipe][Error] ",
             "Checking the type of the 'instance' variable: ",
             class(instance))
      }

      if (private$languageTwitter &&
          instance$getSpecificProperty("extension") %in% "twtid") {

        cachePath <- bdpar.Options$get("cache.twitter.path")
        if (!is.null(cachePath) && file.exists(paste(cachePath, "/_",
                                                     instance$getSpecificProperty("target"),
                                                     "_/",
                                                     instance$getId(),
                                                     ".json",
                                                     sep = ""))) {

          path <- paste(cachePath,"/_",
                          instance$getSpecificProperty("target"),
                            "_/",
                              instance$getId(),
                                ".json",
                                  sep = "")

          dataFromJsonFile <- rjson::fromJSON(file = path)

          if (!is.na(dataFromJsonFile[["lang"]]) &&
                !is.null(dataFromJsonFile[["lang"]])
                  && dataFromJsonFile[["lang"]] != "") {

            langTwitter <- dataFromJsonFile[["lang"]]

            instance$addProperties(langTwitter,super$getPropertyName())

            if (is.null(instance$getSpecificProperty("language"))) {

              message <- c( "The file: " , instance$getPath() , " has a NULL twitter language")

              instance$addProperties(message, "reasonToInvalidate")

              warning("[GuessLanguagePipe][pipe][Warning] ", message)

              instance$invalidate()

              return(instance)
            }

            return(instance)
          }
        }
      }

      instance$getData() %>>%
        self$getLanguage() %>>%
          {instance$addProperties(.,super$getPropertyName())}

      if (is.na(instance$getSpecificProperty("language"))) {
        message <- c( "The file: " , instance$getPath() , " has a null language")

        instance$addProperties(message, "reasonToInvalidate")

        warning("[GuessLanguagePipe][pipe][Warning] ", message)

        instance$invalidate()

        return(instance)
      }

      return(instance)
    },

    getLanguage = function(data) {

      if (!"character" %in% class(data)) {
        stop("[GuessLanguagePipe][getLanguage][Error] ",
             "Checking the type of the 'data' variable: ",
             class(data))
      }

      langStandardize <- cld2::detect_language(data, plain_text = TRUE)

      return(langStandardize)
    }
  ),

  private = list(
    languageTwitter = FALSE
  )
)
