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

#' @title Class to find and/or remove the URLs on the data field of an Instance
#'
#' @description This class is responsible of detecting the existing URLs in the
#' \strong{data} field of each \code{\link{Instance}}. Identified URLs are
#' stored inside the \strong{URLs} field of \code{\link{Instance}} class.
#' Moreover if required, is able to perform inline URLs removement.
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
#' This class inherits from \code{\link{GenericPipe}} and implements the
#' \code{pipe} abstract function.
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{ContractionPipe}},
#'          \code{\link{File2Pipe}}, \code{\link{FindEmojiPipe}},
#'          \code{\link{FindEmoticonPipe}}, \code{\link{FindHashtagPipe}},
#'          \code{\link{FindUserNamePipe}}, \code{\link{GuessDatePipe}},
#'          \code{\link{GuessLanguagePipe}}, \code{\link{Instance}},
#'          \code{\link{InterjectionPipe}}, \code{\link{MeasureLengthPipe}},
#'          \code{\link{GenericPipe}}, \code{\link{SlangPipe}},
#'          \code{\link{StopWordPipe}}, \code{\link{StoreFileExtPipe}},
#'          \code{\link{TargetAssigningPipe}}, \code{\link{TeeCSVPipe}},
#'          \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export FindUrlPipe

FindUrlPipe <- R6Class(

  "FindUrlPipe",

  inherit = GenericPipe,

  public = list(
    #'
    #' @description Creates a \code{\link{FindUrlPipe}} object.
    #'
    #' @param propertyName A \code{\link{character}} value. Name of the property
    #' associated with the \code{\link{GenericPipe}}.
    #' @param propertyLanguageName A \code{\link{character}} value. Name of the
    #' language property.
    #' @param alwaysBeforeDeps A \code{\link{list}} value. The dependencies
    #' alwaysBefore (\code{\link{GenericPipe}s} that must be executed before
    #' this one).
    #' @param notAfterDeps A \code{\link{list}} value. The dependencies
    #' notAfter (\code{\link{GenericPipe}s} that cannot be executed after
    #' this one).
    #' @param removeUrls A \code{\link{logical}} value. Indicates if the
    #' URLs are removed.
    #' @param URLPatterns A \code{\link{list}} value. The regex to find URLs.
    #' @param namesURLPatterns A \code{\link{list}} value. The names of regex.
    #'
    initialize = function(propertyName = "URLs",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list("FindUrlPipe"),
                          removeUrls = TRUE,
                          URLPatterns = list(self$URLPattern, self$EmailPattern),
                          namesURLPatterns = list("UrlPattern","EmailPattern")) {

      if (!"character" %in% class(propertyName)) {
        stop("[", class(self)[1], "][initialize][Error] ",
             "Checking the type of the 'propertyName' variable: ",
             class(propertyName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[", class(self)[1], "][initialize][Error] ",
             "Checking the type of the 'alwaysBeforeDeps' variable: ",
             class(alwaysBeforeDeps))
      }

      if (!"list" %in% class(notAfterDeps)) {
        stop("[", class(self)[1], "][initialize][Error] ",
             "Checking the type of the 'notAfterDeps' variable: ",
             class(notAfterDeps))
      }

      if (!"logical" %in% class(removeUrls)) {
        stop("[", class(self)[1], "][initialize][Error] ",
             "Checking the type of the 'removeUrls' variable: ",
             class(removeUrls))
      }

      if (!"list" %in% class(URLPatterns)) {
        stop("[", class(self)[1], "][initialize][Error] ",
             "Checking the type of the 'URLPatterns' variable: ",
             class(URLPatterns))
      }

      if (!"list" %in% class(namesURLPatterns)) {
        stop("[", class(self)[1], "][initialize][Error] ",
             "Checking the type of the 'namesURLPatterns' variable: ",
             class(namesURLPatterns))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)

      private$removeUrls <- removeUrls
      private$URLPatterns <- URLPatterns
      private$namesURLPatterns <- namesURLPatterns
    },
    #' @field URLPattern  A \code{\link{character}} value. The regular
    #' expression to detect URLs.
    URLPattern = "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))",
    #' @field EmailPattern  A \code{\link{character}} value. The regular expression to detect emails.
    EmailPattern = "(?:\\s|[\"><\u00A1\u00BF?!;:,.'\\(]|^)((?:[\\w_.\u00E7\u00F1+-]+)(?:@|\\(at\\)|<at>)(?:(?:\\w[\\\\.:\u00F1-]?)*)[[:alnum:]\u00F1](?:\\.[a-zA-Z]{2,4}))[;:?\"!,.'>\\)]?(?=(?:\\s|$|>|\\.|,))",
    #'
    #' @description Preprocesses the \code{\link{Instance}} to obtain/remove
    #' the URLs. The URLs found in the data are added to the
    #' list of properties of the \code{\link{Instance}}.
    #'
    #' @param instance A \code{\link{Instance}} value. The \code{\link{Instance}}
    #' to preprocess.
    #'
    #' @return The \code{\link{Instance}} with the modifications that have
    #' occurred in the pipe.
    #'
    #' @import pipeR
    #'
    pipe = function(instance) {

      if (!"Instance" %in% class(instance)) {
        stop("[", class(self)[1], "][pipe][Error] ",
             "Checking the type of the 'instance' variable: ",
             class(instance))
      }

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

        message <- c("The file: ", instance$getPath(), " has data empty on pipe Url")

        instance$addProperties(message, "reasonToInvalidate")

        warning("[", class(self)[1], "][pipe][Warning] ", message)

        instance$invalidate()

        return(instance)
      }

      instance
    },
    #'
    #' @description Finds the \emph{URLs} in the data.
    #'
    #' @param pattern A \code{\link{character}} value. The regex to find URLs.
    #' @param data A \code{\link{character}} value. The text to find the URLs.
    #'
    #' @return The \code{\link{list}} with URLs found.
    #'
    findUrl = function(pattern, data) {

      if (!"character" %in% class(pattern)) {
        stop("[", class(self)[1], "][findUrl][Error] ",
             "Checking the type of the 'pattern' variable: ",
             class(pattern))
      }

      if (!"character" %in% class(data)) {
        stop("[", class(self)[1], "][findUrl][Error] ",
             "Checking the type of the 'data' variable: ",
             class(data))
      }

      unlist(
        unique(
          stringr::str_match_all(data,
                                 rex::regex(pattern,
                                            ignore_case = TRUE,
                                            multiline = TRUE))[[1]][,2]))


    },
    #'
    #' @description Removes \emph{the URL} in the data.
    #'
    #' @param pattern A \code{\link{character}} value. The regex to find URLs.
    #' @param data A \code{\link{character}} value. The text to remove the URLs.
    #'
    #' @return The data with URLs removed.
    #'
    removeUrl = function(pattern, data) {

      if (!"character" %in% class(pattern)) {
        stop("[", class(self)[1], "][removeUrl][Error] ",
             "Checking the type of the 'pattern' variable: ",
             class(pattern))
      }

      if (!"character" %in% class(data)) {
        stop("[", class(self)[1], "][removeUrl][Error] ",
             "Checking the type of the 'data' variable: ",
             class(data))
      }

      stringr::str_replace_all(data,
                               rex::regex(pattern,
                                          ignore_case = TRUE,
                                          multiline = TRUE),
                               " ")

    },
    #'
    #' @description Sets the names to \emph{URL patterns} result.
    #'
    #' @param resultOfURLPatterns A \code{\link{list}} value. The list with
    #' URLs found.
    #'
    #' @return The URLs found with the names of URL pattern.
    #'
    putNamesURLPattern = function(resultOfURLPatterns) {

      if (!"list" %in% class(resultOfURLPatterns)) {
        stop("[", class(self)[1], "][putNamesURLPattern][Error] ",
             "Checking the type of the 'resultOfURLPatterns' variable: ",
             class(resultOfURLPatterns))
      }

      names(resultOfURLPatterns) <- self$getNamesURLPatterns()

      resultOfURLPatterns
    },
    #'
    #' @description Gets \emph{the URL patterns}.
    #'
    #' @return Value of \emph{URL patterns}.
    #'
    getURLPatterns = function() {
      private$URLPatterns
    },
    #'
    #' @description Sets the \emph{URL patterns}.
    #'
    #' @param URLPatterns A \code{\link{list}} value. The new value of
    #' the URL patterns.
    #'
    setURLPatterns = function(URLPatterns) {

      if (!"list" %in% class(URLPatterns)) {
        stop("[", class(self)[1], "][setURLPatterns][Error] ",
             "Checking the type of the 'URLPatterns' variable: ",
             class(URLPatterns))
      }

      private$URLPatterns <- URLPatterns
    },
    #'
    #' @description Gets the \emph{names of URLs}.
    #'
    #' @return Value of names of URLs.
    #'
    getNamesURLPatterns = function() {
      private$namesURLPatterns
    },
    #'
    #' @description Sets the \emph{names of URLs}.
    #'
    #' @param namesURLPatterns A \code{\link{list}} value. The new value of
    #' the names of URLs.
    #'
    setNamesURLPatterns = function(namesURLPatterns) {

      if (!"list" %in% class(namesURLPatterns)) {
        stop("[", class(self)[1], "][setNamesURLPatterns][Error] ",
             "Checking the type of the 'namesURLPatterns' variable: ",
             class(namesURLPatterns))
      }

      private$namesURLPatterns <- namesURLPatterns
    }
  ),

  private = list(
    # A (\emph{list}) value. Regular expressions used to detect URLs.
    URLPatterns = list(),
    # A (\emph{list}) value. Names of regular expressions that are used to
    # identify URLs.
    namesURLPatterns = list(),
    # A (\emph{logical}) value. Indicates if the URLS are removed or
    # not.
    removeUrls = TRUE
  )
)
