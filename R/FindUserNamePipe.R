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

#' @title Class to find and/or remove the users on the data field of an Instance
#'
#' @description This class is responsible of detecting the existing use names in the
#' \strong{data} field of each \code{\link{Instance}}. Identified user names are
#' stored inside the \strong{userName} field of \code{\link{Instance}} class.
#' Moreover if required, is able to perform inline user nanme removement.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Constructor:
#' \preformatted{
#' FindUserNamePipe$new(propertyName = "userName",
#'                      alwaysBeforeDeps = list(),
#'                      notAfterDeps = list())
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
#' }
#' }
#' }
#
#' @section Details:
#' The regular expressions indicated in the \code{userPattern}
#' variable are used to identify user names.
#'
#' @section Note:
#' \code{\link{FindUserNamePipe}} will automatically invalidate the
#' \code{\link{Instance}} whenever the obtained data is empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe:}}{
#' preprocesses the \code{\link{Instance}} to obtain/remove the name users.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{pipe(instance, removeUser = TRUE)}
#' }
#' \item{\emph{Value:}}{
#' the \code{\link{Instance}} with the modifications that have occurred in the Pipe.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{instance:}}{
#' (\emph{Instance}) \code{\link{Instance}} to preproccess.
#' }
#' \item{\strong{removeUser:}}{
#' (\emph{logical}) indicates if the users are removed.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{findUserName:}}{
#' finds the name users in the data.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{findHashtag(data)}
#' }
#' \item{\emph{Value:}}{
#' list with users names found.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{data:}}{
#' (\emph{character}) text to search the user names.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{removeUserName:}}{
#' removes the users in the data.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{removeUserName(data)}
#' }
#' \item{\emph{Value:}}{
#' the data with name users removed.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{data:}}{
#' (\emph{character}) text to remove the user names.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @section Public fields:
#' \itemize{
#' \item{\bold{userPattern:}}{
#'  (\emph{character}) regular expression to detect users.
#' }
#' }
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{ContractionPipe}},
#'          \code{\link{File2Pipe}}, \code{\link{FindEmojiPipe}},
#'          \code{\link{FindEmoticonPipe}}, \code{\link{FindHashtagPipe}},
#'          \code{\link{FindUrlPipe}}, \code{\link{GuessDatePipe}},
#'          \code{\link{GuessLanguagePipe}}, \code{\link{Instance}},
#'          \code{\link{InterjectionPipe}}, \code{\link{MeasureLengthPipe}},
#'          \code{\link{PipeGeneric}}, \code{\link{SlangPipe}},
#'          \code{\link{StopWordPipe}}, \code{\link{StoreFileExtPipe}},
#'          \code{\link{TargetAssigningPipe}}, \code{\link{TeeCSVPipe}},
#'          \code{\link{ToLowerCasePipe}}
#'
#' @keywords NULL
#'
#' @import pipeR R6 rlist
#' @export FindUserNamePipe

FindUserNamePipe <- R6Class(

  "FindUserNamePipe",

  inherit = PipeGeneric,

  public = list(

    initialize = function(propertyName = "userName",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list()) {

      if (!requireNamespace("rex", quietly = TRUE)) {
        stop("[FindUserNamePipe][initialize][Error]
                Package \"rex\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!requireNamespace("textutils", quietly = TRUE)) {
        stop("[FindUserNamePipe][initialize][Error]
                Package \"textutils\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!requireNamespace("stringr", quietly = TRUE)) {
        stop("[FindUserNamePipe][initialize][Error]
                Package \"stringr\" needed for this class to work.
                  Please install it.",
                    call. = FALSE)
      }

      if (!"character" %in% class(propertyName)) {
        stop("[FindUserNamePipe][initialize][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }

      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[FindUserNamePipe][initialize][Error]
                Checking the type of the variable: alwaysBeforeDeps ",
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[FindUserNamePipe][initialize][Error]
                Checking the type of the variable: notAfterDeps ",
                  class(notAfterDeps))
      }

      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },

    userPattern = "(?:\\s|^|[\"><\u00A1\u00BF?!;:,.'-])(@[^[:cntrl:][:space:]!\"#$%&'()*+\\\\,\\/:;<=>?@\\[\\]^`{|}~]+)[;:?\"!,.'>-]?(?=(?:\\s|$|>))",

    pipe = function(instance, removeUser = TRUE){

      if (!"Instance" %in% class(instance)) {
        stop("[FindUserNamePipe][pipe][Error]
                Checking the type of the variable: instance ",
                  class(instance))
      }

      if (!"logical" %in% class(removeUser)) {
          stop("[FindUserNamePipe][pipe][Error]
                  Checking the type of the variable: removeUser ",
                    class(removeUser))
      }

      instance$addFlowPipes("FindUserNamePipe")

      if (!instance$checkCompatibility("FindUserNamePipe", self$getAlwaysBeforeDeps())) {
        stop("[FindUserNamePipe][pipe][Error] Bad compatibility between Pipes.")
      }

      instance$addBanPipes(unlist(super$getNotAfterDeps()))

      instance$getData() %>>%
        self$findUserName() %>>%
          unique() %>>%
            unlist() %>>%
              {instance$addProperties(.,super$getPropertyName())}

      if (removeUser) {
        instance$getData()  %>>%
          self$removeUserName() %>>%
            textutils::trim() %>>%
              instance$setData()
      }

      if (is.na(instance$getData()) ||
          all(instance$getData() == "") ||
          is.null(instance$getData())) {
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe UserName")

        instance$addProperties(message, "reasonToInvalidate")

        warning("[FindUserNamePipe][pipe][Warning] ", message, " \n")

        instance$invalidate()

        return(instance)
      }

      return(instance)
    },

    findUserName = function(data) {

      if (!"character" %in% class(data)) {
        stop("[FindUserNamePipe][findUserName][Error]
                Checking the type of the variable: data ",
                  class(data))
      }

      return(stringr::str_match_all(data,
                           rex::regex(self$userPattern,
                                 ignore_case = TRUE,
                                 multiline = TRUE))[[1]][,2])
    },

    removeUserName = function(data) {

      if (!"character" %in% class(data)) {
        stop("[FindUserNamePipe][removeUserName][Error]
                Checking the type of the variable: data ",
                  class(data))
      }

      return(stringr::str_replace_all(data,
                             rex::regex(self$userPattern,
                                   ignore_case = TRUE,
                                   multiline = TRUE), " "))
    }
  )
)
