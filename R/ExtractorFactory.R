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

#' @title Class to handle the creation of Instance types
#'
#' @description \code{\link{ExtractorFactory}} class builds the appropriate
#' \code{\link{Instance}} object according to the file extension.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \code{ExtractorFactory$new()}
#'
#' @section Methods:
#' \itemize{
#' #' \item{\bold{registerExtractor:}}{
#' adds an extractor to the list of extensions
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{registerExtractor(extension, extractor)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{extension:}}{
#' (\emph{character}) the name of the extension option.
#' }
#' \item{\strong{extractor:}}{
#' (\emph{Object}) the extractor of the new extension.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{setExtractor:}}{
#' modifies the extractor of the one extension.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{setExtractor(extension, extractor)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{extension:}}{
#' (\emph{character}) the name of the new extension.
#' }
#' \item{\strong{extractor:}}{
#' (\emph{Instance}) the value of the new extractor.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{removeExtractor:}}{
#' removes a specific extractor throught the extension.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{removeExtractor(extension)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{extension:}}{
#' (\emph{character}) the name of the extension to remove.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getAllExtractors:}}{
#' gets the list of extractors.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getAllExtractors()}
#' }
#' \item{\emph{Value:}}{
#' Value of extractors.
#' }
#' }
#' }
#'
#' \item{\bold{createInstance:}}{
#' builds the \code{\link{Instance}} object according to the file extension.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{createInstance(path)}
#' }
#' \item{\emph{Value:}}{
#' the \code{\link{Instance}} corresponding object according to the file extension.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{path:}}{
#' (\emph{character}) path of the file to create an \code{\link{Instance}}.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{ExtractorEml}}, \code{\link{ExtractorSms}},
#' \code{\link{ExtractorTwtid}}, \code{\link{ExtractorYtbid}},
#' \code{\link{Instance}}
#'
#' @keywords NULL
#'
#' @import R6 tools
#' @export ExtractorFactory

ExtractorFactory <- R6Class(

  "ExtractorFactory",

  public = list(

    initialize = function() {
      private$extractors <- list("eml" = ExtractorEml,
                                 "tsms" = ExtractorSms,
                                 "twtid" = ExtractorTwtid,
                                 "ytbid" = ExtractorYtbid)
    },

    registerExtractor = function(extension, extractor) {
      if (!"character" %in% class(extension)) {
        stop("[ExtractorFactory][registerExtractor][Error] Checking the type of the 'extension' variable: ",
             class(extension))
      }

      if (self$isSpecificExtractor(extension)) {
        stop("[ExtractorFactory][registerExtractor][Error] '", extension, "' extension is already ",
             "added")
      } else {
        if (!"R6ClassGenerator" %in% class(extractor) || extractor$inherit != "Instance") {
          stop("[ExtractorFactory][registerExtractor][Error] Checking the type of the 'extractor' variable: ",
               class(extractor))
        }
        private$extractors <- list.append(private$extractors, extractor)
        names(private$extractors)[length(private$extractors)] <- extension
      }
    },

    setExtractor = function(extension, extractor) {
      if (!"character" %in% class(extension)) {
        stop("[ExtractorFactory][setExtractor][Error] Checking the type of the 'extension' variable: ",
             class(extension))
      }

      if (!self$isSpecificExtractor(extension)) {
        stop("[ExtractorFactory][setExtractor][Error] '", extension, "' extension is not configured")
      } else {
        if (!"R6ClassGenerator" %in% class(extractor) || extractor$inherit != "Instance") {
          stop("[ExtractorFactory][setExtractor][Error] Checking the type of the 'extractor' variable: ",
               class(extractor))
        }
        private$extractors[[extension]] <- extractor
      }
    },

    removeExtractor = function(extension) {
      if (!"character" %in% class(extension)) {
        stop("[ExtractorFactory][removeExtractor][Error] Checking the type of the 'extension' variable: ",
             class(extension))
      }

      if (!self$isSpecificExtractor(extension)) {
        stop("[ExtractorFactory][removeExtractor][Error] '", extension, "' extension is not configured")
      } else {
        private$extractors <- list.remove(private$extractors, extension)
      }
    },

    getAllExtractors = function() {
      private$extractors
    },

    isSpecificExtractor = function(extension) {
      extension %in% names(private$extractors)
    },

    createInstance = function(path) {

      if (!"character" %in% class(path)) {
        stop("[ExtractorFactory][createInstance][Error] ",
             "Checking the type of the 'path' variable: ",
             class(path))
      }

      if (!tools::file_ext(path) %in% names(private$extractors)) {
        message("[ExtractorFactory][createInstance][Warning] ",
                "The extension '", tools::file_ext(path), "' is not registered")
      } else {
        extractor <- private$extractors[[tools::file_ext(path)]]
        extractor <- extractor$new(path)
        return(extractor)
      }

      return()
    },

    reset = function() {
      private$extractors <- list("eml" = ExtractorEml,
                                 "tsms" = ExtractorSms,
                                 "twtid" = ExtractorTwtid,
                                 "ytbid" = ExtractorYtbid)
    },

    print = function(...) {
      print(self$getAllExtractors())
    }
  ),

  private = list(
    extractors = list()
  )
)
