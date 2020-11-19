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

#'
#' @title Write messages to the log at a given priority level using
#' the custom bdpar log
#'
#' @description \code{bdpar.log} is responsible for managing the messages to
#' show on the log.
#'
#' @param message A string to be printed to the log with the corresponding
#' priority level.
#' @param level The desired priority level (DEBUG,INFO,WARN,ERROR and FATAL).
#' In the case of the FATAL level will be call to the stop function. Also, if
#' the level is WARN, the message will be a warning.
#' @param className A string to indicated in which class is called to the log.
#' If the value is NULL, this field is not shown in the log.
#' @param methodName A string to indicated in which method is called to the log.
#' If the value is NULL, this field is not shown in the log.
#'
#' @section Details:
#' The format output is as following:
#'
#' [currentTime][className][methodName][level] message
#'
#' The type of message changes according to the level indicated:
#'
#' - The \strong{DEBUG},\strong{INFO} and \strong{ERROR} levels return a text
#' using the \code{\link{message}} function.
#'
#' - The \strong{WARN} level returns a text using the \code{\link{warning}} function.
#'
#' - The \strong{FATAL} level returns a text using the \code{\link{stop}} function.
#'
#' @note In the case of multithreading, the log will only be by file.
#'
#' @examples
#' \dontrun{
#'
#' # First step, configure the behavior of log
#'
#' bdpar.options$configureLog(console = TRUE, threshold = "DEBUG", file = NULL)
#'
#' message <- "Message example"
#'
#' className <- "Class name example"
#'
#' methodName <- "Method name example"
#'
#' bdpar.log(message = message, level = "DEBUG", className = NULL, methodName = NULL)
#'
#' bdpar.log(message = message, level = "INFO", className = className, methodName = methodName)
#'
#' bdpar.log(message = message, level = "WARN", className = className, methodName = NULL)
#'
#' bdpar.log(message = message, level = "ERROR", className = NULL, methodName = NULL)
#'
#' bdpar.log(message = message, level = "FATAL", className = NULL, methodName = methodName)
#' }
#'
#' @keywords NULL
#'
#' @export bdpar.log
#' @seealso \code{\link{bdpar.Options}}
#' @include wrapper.R

bdpar.log <- function(message, level = "INFO", className = NULL, methodName = NULL) {

  if (is.null(level) ||
      !is.character(level) ||
      !any(c("FATAL", "ERROR", "WARN", "INFO", "DEBUG") %in% level)) {
    stop("[", format(Sys.time()), "]","[bdpar.log][FATAL] The 'level' variable ",
         "must be between these values: FATAL, ERROR, WARN, INFO or DEBUG")
  }

  settings <- .getLoggerSettings()

  if (is.null(settings$loggers)) {
    stop("[", format(Sys.time()), "]","[bdpar.log][FATAL] Logger is not ",
         "configured. Use bdpar.options$configureLog to configure its behavior")
  }

  for (logger in settings$loggers) {
    if (.levelToInt(level) >= .levelToInt(logger$threshold)) {
      logger$.logFunction(this = logger, level = level, message = list(className, methodName, message))
    }
  }
}
