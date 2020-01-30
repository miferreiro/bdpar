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

#' @title Object to handle the keys/attributes/options common to all pipeline flow
#'
#' @description This class provides the necessary methods to manage a list of
#' keys or options used along the pipe flow, both those provided by the default
#' library and those implemented by the user.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Details:
#' By default, the application initializes the object named \code{bdpar.Options}
#' of type \code{BdparOptions} which is in charge of initializing the
#' options used in the defined pipes.
#'
#' The default fields on \emph{\link{bdpar.Options}} are initialized, if needed,
#' as shown bellow:
#'
#' \strong{[eml]}
#'
#' - \code{bdpar.Options$set("eml.PartSelectedOnMPAlternative", <<PartSelectedOnMPAlternative>>)}
#'
#' \strong{[resources]}
#'
#' - \code{bdpar.Options$set("resources.abbreviations.path", <<abbreviation.path>>)}
#'
#' - \code{bdpar.Options$set("resources.contractions.path", <<contractions.path>>)}
#'
#' - \code{bdpar.Options$set("resources.interjections.path", <<interjections.path>>)}
#'
#' - \code{bdpar.Options$set("resources.slangs.path", <<slangs.path>>)}
#'
#' - \code{bdpar.Options$set("resources.stopwords.path", <<stopwords.path>>)}
#'
#' \strong{[twitter]}
#'
#' - \code{bdpar.Options$set("twitter.consumer.key", <<consumer_key>>)}
#'
#' - \code{bdpar.Options$set("twitter.consumer.secret", <<consumer_secret>>)}
#'
#' - \code{bdpar.Options$set("twitter.access.token", <<access_token>>)}
#'
#' - \code{bdpar.Options$set("twitter.access.token.secret", <<access_token_secret>>)}
#'
#' - \code{bdpar.Options$set("twitter.cache.path", <<cache.path>>)}
#'
#' \strong{[teeCSVPipe]}
#'
#' - \code{bdpar.Options$set("teeCSVPipe.output.path", <<outputh.path>>)}
#'
#' \strong{[youtube]}
#'
#' - \code{bdpar.Options$set("youtube.app.id", <<app_id>>)}
#'
#' - \code{bdpar.Options$set("youtube.app.password", <<app_password>>)}
#'
#' - \code{bdpar.Options$set("youtube.cache.path", <<cache.path>>)}

#' @section Methods:
#' \itemize{
#' \item{\bold{get:}}{
#' obtains a specific option.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{get(key)}
#' }
#' \item{\emph{Value:}}{
#' the value of the specific option.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{key:}}{
#' (\emph{character}) the name of the option to obtain.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{add:}}{
#' adds a option to the list of options
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{add(key, value)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{key:}}{
#' (\emph{character}) the name of the new option.
#' }
#' \item{\strong{propertyName:}}{
#' (\emph{Object}) the value of the new option.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{set:}}{
#' modifies the value of the one option.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{set(key, value)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{key:}}{
#' (\emph{character}) the name of the new option.
#' }
#' \item{\strong{propertyName:}}{
#' (\emph{Object}) the value of the new option.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{remove:}}{
#' removes a specific option.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{get(key)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{key:}}{
#' (\emph{character}) the name of the option to remove.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getAll:}}{
#' gets the list of options.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getAll()}
#' }
#' \item{\emph{Value:}}{
#' Value of options.
#' }
#' }
#' }
#'
#' \item{\bold{remove:}}{
#' resets the option list to the initial state.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{reset()}
#' }
#' }
#' }
#'
#' \item{\bold{isSpecificOption:}}{
#' checks for the existence of an specific option.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{isSpecificProperty(key)}
#' }
#' \item{\emph{Value:}}{
#' A boolean results according to the existence of the specific option in the list of options
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{key:}}{
#' (\emph{character}) the key of the option to check.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#'
#' @seealso \code{\link{AbbreviationPipe}}, \code{\link{Connections}},
#'          \code{\link{ContractionPipe}}, \code{\link{ExtractorEml}},
#'          \code{\link{ExtractorTwtid}}, \code{\link{ExtractorYtbid}},
#'          \code{\link{GuessLanguagePipe}}, \code{\link{SlangPipe}},
#'          \code{\link{StopWordPipe}}, \code{\link{TeeCSVPipe}}
#'
#' @keywords NULL
#'
#' @export bdpar.Options

bdpar.Options <- BdparOptions$new()
