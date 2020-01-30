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

#' @title Class to handle email files with eml extension
#'
#' @description This class inherits from the \code{\link{Instance}} class and
#' implements the functions of extracting the text and the date from an eml type file.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \code{ExtractorEml$new(path)}
#' \itemize{
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{path:}}{
#' (\emph{character}) path of the eml type file.
#' }
#' \item{\strong{PartSelectedOnMPAlternative:}}{
#' (\emph{character}) configuration to read the eml files. If it is NULL, checks
#' if is defined in the \strong{"eml.PartSelectedOnMPAlternative"} field of
#' \emph{\link{bdpar.Options}} variable.
#' }
#' }
#' }
#' }
#'
#' @section Details:
#' The way to indicate which part to choose in the email, when is a multipart email,
#' is through the \strong{"eml.PartSelectedOnMPAlternative"}
#' field of \emph{\link{bdpar.Options}} variable.
#'
#' @section Note:
#' To be able to use this class it is necessary to have Python installed.
#'
#' @section Inherit:
#' This class inherits from \code{\link{Instance}} and implements the
#' \code{obtainSource} and \code{obtainDate} abstracts functions.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{obtainDate:}}{
#' obtains the date of the eml file. Calls the function \emph{read_emails}
#' and obtains the date of the file indicated in the path and then transforms it
#' into the generic date format, that is "\%a \%b \%d \%H:\%M:\%S \%Z \%Y"
#' (Example: "Thu May 02 06:52:36 UTC 2013").
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{obtainDate()}
#' }
#' }
#' }
#'
#' \item{\bold{obtainSource:}}{
#' obtains the source of the eml file. Calls the function \emph{read_emails}
#' and obtains the source of the file indicated in the path. In addition, it
#' initializes the data with the initial source.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{obtainSource()}
#' }
#' }
#' }
#'
#' \item{\bold{getPartSelectedOnMPAlternative:}}{
#' gets of \code{PartSelectedOnMPAlternative} variable.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getPartSelectedOnMPAlternative()}
#' }
#' \item{\emph{Value:}}{
#' value of \code{PartSelectedOnMPAlternative} variable.
#' }
#' }
#' }
#'
#' \item{\bold{setPartSelectedOnMPAlternative:}}{
#' sets of \code{PartSelectedOnMPAlternative} variable.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{setPartSelectedOnMPAlternative(PartSelectedOnMPAlternative)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{PartSelectedOnMPAlternative}}{
#' (\emph{character}) the new value of \code{PartSelectedOnMPAlternative} variable.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{PartSelectedOnMPAlternative:}}{
#'  (\emph{character}) configuration to read the eml files. Indicates whether the
#'  text/plain part or the text/html part is read in multipart emails.
#' }
#' }
#'
#' @seealso \code{\link{bdpar.Options}}, \code{\link{ExtractorSms}},
#' \code{\link{ExtractorTwtid}}, \code{\link{ExtractorYtbid}},
#' \code{\link{Instance}}
#'
#' @keywords NULL
#'
#' @import pipeR R6
#' @export ExtractorEml

ExtractorEml <- R6Class(

  classname = "ExtractorEml",

  inherit = Instance,

  public = list(

    initialize = function(path,
                          PartSelectedOnMPAlternative = NULL) {

      if (!"character" %in% class(path)) {
        stop("[ExtractorEml][initialize][Error] ",
             "Checking the type of the 'path' variable: ",
             class(path))
      }

      path %>>%
        super$initialize()


      if (is.null(PartSelectedOnMPAlternative)) {
        if (any(!bdpar.Options$isSpecificOption("eml.PartSelectedOnMPAlternative"),
                is.null(bdpar.Options$get("eml.PartSelectedOnMPAlternative")))) {
          stop("[ExtractorEml][initialize][Error] Part of select on .eml files ",
               "is neither defined in initialize or in bdpar.Options")
        } else {
          PartSelectedOnMPAlternative <- bdpar.Options$get("eml.PartSelectedOnMPAlternative")
        }
      }

      if (!"character" %in% class(PartSelectedOnMPAlternative)) {
        stop("[ExtractorEml][initialize][Error] ",
             "Checking the type of the 'PartSelectedOnMPAlternative' variable: ",
             class(PartSelectedOnMPAlternative))
      }

      PartSelectedOnMPAlternative %>>%
        self$setPartSelectedOnMPAlternative()
    },

    obtainDate = function() {

      dateEml <- tryCatch(

        read_emails(super$getPath(),self$getPartSelectedOnMPAlternative())["date"],

        error = function(e) {
          message("[ExtractorEml][obtainDate][Error] Date eml error ",
                   super$getPath()," ", paste(e))
        }
      )

      tryCatch({
        formatDateEml <- "%a, %d %b %Y %H:%M:%S %z"
        StandardizedDate <- as.POSIXct(dateEml[[1]], format = formatDateEml)
        formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
        format(StandardizedDate, formatDateGeneric) %>>%
          super$setDate()
        },
        error = function(e) {
          message("[ExtractorEml][obtainDate][Error] Date eml error in
                   standardized proccess", super$getPath(), " ", paste(e))
        }
      )

      return()
    },

    obtainSource = function() {

      private$source <- tryCatch(

        paste(read_emails(super$getPath(),
                          self$getPartSelectedOnMPAlternative())["message"],
              collapse = " "),

        error = function(e) {
          message("[ExtractorEml][obtainSource][Error] Source eml error ",
                   super$getPath()," ", paste(e))
        }
      )

      super$getSource() %>>%
        super$setData()

      return()
    },

    getPartSelectedOnMPAlternative = function() {

      return(private$PartSelectedOnMPAlternative)
    },

    setPartSelectedOnMPAlternative = function(PartSelectedOnMPAlternative) {

      if (!"character" %in% class(PartSelectedOnMPAlternative)) {
        stop("[ExtractorEml][setPartSelectedOnMPAlternative][Error] ",
             "Checking the type of the 'PartSelectedOnMPAlternative' variable: ",
             class(PartSelectedOnMPAlternative))
      }

      private$PartSelectedOnMPAlternative <- PartSelectedOnMPAlternative

      return()
    }

  ),

  private = list(
    PartSelectedOnMPAlternative = "text/plain"
  )
)
