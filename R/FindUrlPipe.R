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

#' @title Class to find and/or remove the URLs on the data field of an Instance
#'
#' @description This class is responsible of detecting the existing URLs in the
#' \strong{data} field of each \code{\link{Instance}}. Identified URLs are
#' stored inside the \strong{URLs} field of \code{\link{Instance}} class.
#' Moreover if required, is able to perform inline URLs removement.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \preformatted{
#' FindUrlPipe$new(propertyName = "URLs",
#'                 alwaysBeforeDeps = list(),
#'                 notAfterDeps = list(),
#'                 removeUrls = TRUE,
#'                 URLPatterns = list(self$URLPattern, self$EmailPattern),
#'                 namesURLPatterns = list("UrlPattern","EmailPattern"))
#' }
#'
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
#' \item{\strong{removeUrls:}}{
#' (\emph{logical}) indicates if the URLs are removed.
#' }
#' \item{\strong{URLPatterns:}}{
#' (\emph{list}) the regex to find URLs.
#' }
#' \item{\strong{namesURLPatterns:}}{
#' (\emph{list}) the names of regex.
#' }
#' }
#' }
#' }
#'
#' @section Details:
#' The regular expressions indicated in the \code{URLPatterns}
#' variable are used to identify URLs.
#'
#' @section Note:
#' \code{\link{FindUrlPipe}} will automatically invalidate the
#' \code{\link{Instance}} whenever the obtained data is empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe:}}{
#' preprocesses the \code{\link{Instance}} to obtain/remove the users.
#' \itemize{
#' \item{\emph{Usage:}}{
#'
#' \preformatted{
#' pipe(instance)}
#' }
#' \item{\emph{Value:}}{
#'
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
#' \item{\bold{findUrl:}}{
#' finds the URLs in the data.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{findHashtag(pattern, data)}
#' }
#' \item{\emph{Value:}}{
#' list with URLs found.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{pattern:}}{
#' (\emph{character}) regex to find URLs.
#' }
#' \item{\strong{data:}}{
#' (\emph{character}) text to search the URLs.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{removeUrl:}}{
#' removes the URLs in the data.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{removeUrl(pattern, data)}
#' }
#' \item{\emph{Value:}}{
#' the data with URLs removed.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{pattern:}}{
#' (character) regex to find URLs.
#' }
#' \item{\strong{data:}}{
#' (\emph{character}) text to remove the URLs.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{putNamesURLPattern:}}{
#' sets the names to URL patterns result.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{putNamesURLPattern(resultOfURLPatterns)}
#' }
#' \item{\emph{Value:}}{
#' Value of \code{resultOfURLPatterns} variable with the names of URL pattern.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{resultOfURLPatterns:}}{
#' (\emph{list}) list with URLs found.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getURLPatterns:}}{
#' gets of URL patterns.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getURLPatterns()}
#' }
#' \item{\emph{Value:}}{
#' value of URL patterns.
#' }
#' }
#' }
#'
#' \item{\bold{getNamesURLPatterns:}}{
#' gets of name of URLs.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getNamesURLPatterns()}
#' }
#' \item{\emph{Value:}}{
#' value of name of URLs.
#' }
#' }
#' }
#'
#' \item{\bold{setNamesURLPatterns:}}{
#' sets the name of URLs.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{setNamesURLPatterns(namesURLPatterns)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{namesURLPatterns:}}{
#' (\emph{character}) the new value of the name of URLs.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @section Public fields:
#' \itemize{
#' \item{\bold{URLPattern:}}{
#'  (\emph{character}) regular expression to detect URLs.
#' }
#' \item{\bold{EmailPattern:}}{
#'  (\emph{character}) regular expression to detect emails.
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{URLPatterns:}}{
#'  (\emph{list}) regular expressions used to detect URLs.
#' }
#' \item{\bold{namesURLPatterns:}}{
#'  (\emph{list}) names of regular expressions that are used to identify URLs.
#' }
#' \item{\bold{removeUrls:}}{
#'  (\emph{logical}) indicates if the URLs are removed.
#' }
#' }
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{ContractionPipe}},
#'          \code{\link{File2Pipe}}, \code{\link{FindEmojiPipe}},
#'          \code{\link{FindEmoticonPipe}}, \code{\link{FindHashtagPipe}},
#'          \code{\link{FindUserNamePipe}}, \code{\link{GuessDatePipe}},
#'          \code{\link{GuessLanguagePipe}}, \code{\link{Instance}},
#'          \code{\link{InterjectionPipe}}, \code{\link{MeasureLengthPipe}},
#'          \code{\link{PipeGeneric}}, \code{\link{SlangPipe}},
#'          \code{\link{StopWordPipe}}, \code{\link{StoreFileExtPipe}},
#'          \code{\link{TargetAssigningPipe}}, \code{\link{TeeCSVPipe}},
#'          \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import R6 rlist pipeR
#' @export FindUrlPipe

FindUrlPipe <- R6Class(

  "FindUrlPipe",

  inherit = PipeGeneric,

  public = list(

    initialize = function(propertyName = "URLs",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list("FindUrlPipe"),
                          removeUrls = TRUE,
                          URLPatterns = list(self$URLPattern, self$EmailPattern),
                          namesURLPatterns = list("UrlPattern","EmailPattern")) {

      if (!requireNamespace("rex", quietly = TRUE)) {
        stop("[FindUrlPipe][initialize][Error]
                Package \"rex\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!requireNamespace("textutils", quietly = TRUE)) {
        stop("[FindUrlPipe][initialize][Error]
                Package \"textutils\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!requireNamespace("stringr", quietly = TRUE)) {
        stop("[FindUrlPipe][initialize][Error]
                Package \"stringr\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!"character" %in% class(propertyName)) {
        stop("[FindUrlPipe][initialize][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[FindUrlPipe][initialize][Error]
                Checking the type of the variable: alwaysBeforeDeps ",
                  class(alwaysBeforeDeps))
      }

      if (!"list" %in% class(notAfterDeps)) {
        stop("[FindUrlPipe][initialize][Error]
                Checking the type of the variable: notAfterDeps ",
                  class(notAfterDeps))
      }

      if (!"logical" %in% class(removeUrls)) {
        stop("[FindUrlPipe][initialize][Error]
                Checking the type of the variable: removeUrls ",
                  class(removeUrls))
      }

      if (!"list" %in% class(URLPatterns)) {
        stop("[FindUrlPipe][initialize][Error]
                Checking the type of the variable: URLPatterns ",
                  class(URLPatterns))
      }

      if (!"list" %in% class(namesURLPatterns)) {
        stop("[FindUrlPipe][initialize][Error]
                 Checking the type of the variable: namesURLPatterns ",
                  class(namesURLPatterns))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)

      private$removeUrls <- removeUrls
      private$URLPatterns <- URLPatterns
      private$namesURLPatterns <- namesURLPatterns
    },

    URLPattern = "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))",

    EmailPattern = "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:[\\w_.\u00E7\u00F1+-]+)(?:@|\\(at\\)|<at>)(?:(?:\\w[\\\\.:\u00F1-]?)*)[[:alnum:]\u00F1](?:\\.[a-zA-Z]{2,4}))[;:?\"!,.'>\\)]?(?=(?:\\s|$|>|\\.|,))",

    pipe = function(instance) {

      if (!"Instance" %in% class(instance)) {
        stop("[FindUrlPipe][pipe][Error]
                Checking the type of the variable: instance ",
                  class(instance))
      }

      instance$addFlowPipes("FindUrlPipe")

      if (!instance$checkCompatibility("FindEmojiInStringBufferPipe", self$getAlwaysBeforeDeps())) {
        stop("[FindUrlPipe][pipe][Error] Bad compatibility between Pipes.")
      }

      instance$addBanPipes(unlist(super$getNotAfterDeps()))

      instance$getData() %>>%
        {lapply(private$URLPatterns, self$findUrl,.)} %>>%
          self$putNamesURLPattern() %>>%
            unlist() %>>%
              {instance$addProperties(.,super$getPropertyName())}

      if (private$removeUrls) {
        for (pattern in self$getURLPatterns()) {
          instance$getData() %>>%
            {self$removeUrl(pattern,.)} %>>%
              textutils::trim() %>>%
                instance$setData()
        }
      }

      if (is.na(instance$getData()) ||
          all(instance$getData() == "") ||
          is.null(instance$getData())) {

        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Url")

        instance$addProperties(message, "reasonToInvalidate")

        warning("[FindUrlPipe][pipe][Warning] ", message, " \n")

        instance$invalidate()

        return(instance)
      }

      return(instance)
    },

    findUrl = function(pattern, data) {

      if (!"character" %in% class(pattern)) {
        stop("[FindUrlPipe][findUrl][Error]
                Checking the type of the variable: pattern ",
                  class(pattern))
      }

      if (!"character" %in% class(data)) {
        stop("[FindUrlPipe][findUrl][Error]
                Checking the type of the variable: data ",
                  class(data))
      }

      return(stringr::str_match_all(data,
                           rex::regex(pattern,
                                 ignore_case = TRUE,
                                 multiline = TRUE))[[1]][,2] %>>% unique() %>>% unlist() )
    },

    removeUrl = function(pattern, data) {

      if (!"character" %in% class(pattern)) {
        stop("[FindUrlPipe][removeUrl][Error]
                Checking the type of the variable: pattern ",
                  class(pattern))
      }

      if (!"character" %in% class(data)) {
        stop("[FindUrlPipe][removeUrl][Error]
                Checking the type of the variable: data ",
                  class(data))
      }

      return(stringr::str_replace_all(data,
                              rex::regex(pattern,
                                    ignore_case = TRUE,
                                    multiline = TRUE), " "))

    },

    putNamesURLPattern = function(resultOfURLPatterns) {

      if (!"list" %in% class(resultOfURLPatterns)) {
        stop("[FindUrlPipe][putNamesURLPattern][Error]
                Checking the type of the variable: resultOfURLPatterns ",
                  class(resultOfURLPatterns))
      }

      names(resultOfURLPatterns) <- self$getNamesURLPatterns()

      return(resultOfURLPatterns)
    },

    getURLPatterns = function() {

      return(private$URLPatterns)
    },

    setURLPatterns = function(URLPatterns) {

      if (!"list" %in% class(URLPatterns)) {
        stop("[FindUrlPipe][setURLPatterns][Error]
                Checking the type of the variable: URLPatterns ",
                  class(URLPatterns))
      }

      private$URLPatterns <- URLPatterns

      return()
    },

    getNamesURLPatterns = function() {

      return(private$namesURLPatterns)
    },

    setNamesURLPatterns = function(namesURLPatterns) {

      if (!"list" %in% class(namesURLPatterns)) {
        stop("[FindUrlPipe][setNamesURLPatterns][Error]
                Checking the type of the variable: namesURLPatterns ",
                  class(namesURLPatterns))
      }

      private$namesURLPatterns <- namesURLPatterns

      return()
    }
  ),

  private = list(
    URLPatterns = list(),
    namesURLPatterns = list(),
    removeUrls = TRUE
  )
)
