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

# Wrappers to adapt functions of ParallelLogger package to bdpar package.

#'
#'@include bdpar.log.R
#'
.clearLoggers <- function() {
  settings <- .getLoggerSettings()
  settings$loggers <- list()
  .setLoggerSettings(settings)
}

.getLoggerSettings <- function() {
  settings <- getOption("loggerSettings")

  if (is.null(getOption("warning.expression"))) {
    .registerDefaultHandlers()
  }
  settings
}

.setLoggerSettings <- function(settings) {
  options(loggerSettings = settings)
}
#'
#' @importFrom methods is
#'
.conditionHandler <- function(condition) {
  if (is(condition, "error")) {
    bdpar.log(message = condition$message, level = "FATAL",
              className = NULL, methodName = ".conditionHandler")
  } else if (is(condition, "warning")) {
    bdpar.log(message = condition$message, level = "WARN",
              className = NULL, methodName = ".conditionHandler")
  } else if (is(condition, "message")) {
    bdpar.log(message = condition$message, level = "INFO",
              className = NULL, methodName = ".conditionHandler")
  }
}

.registerDefaultHandlers <- function() {
  logBaseError <- function() {
    bdpar.log(message = gsub("\n", " ", geterrmessage()), level = "FATAL",
              className = NULL, methodName = ".conditionHandler")
  }
  options(error = logBaseError)

  options(warning.expression = quote({
    evaluate <- function(message, frameIndex) {
      if (frameIndex < -20) {
        return(as.character(force(message)))
      } else {
        text <- tryCatch(as.character(eval(message, envir = sys.frame(frameIndex))), error = function(x) "error")
        if (text == "error") {
          return(evaluate(message, frameIndex - 2))
        } else {
          return(text)
        }
      }
    }

    for (i in 1:sys.nframe()) {
      frame <- sys.call(-i)
      if (!is.null(frame) && length(frame) > 1) {
        name <- as.character(frame[[1]])
        if (length(name) == 1) {
          if (is.language(frame[[1]]) && name == "warning") {
            bdpar.log(message = evaluate(frame[[2]], -i - 1), level = "WARN",
                      className = NULL, methodName = ".conditionHandler")
            break
          } else if (name == ".signalSimpleWarning") {
            bdpar.log(message = frame[[2]], level = "WARN",
                      className = NULL, methodName = ".conditionHandler")
            break
          } else if (name == ".Deprecated") {
            bdpar.log(message = paste0("This function is deprecated. Use '", frame[[2]], "' instead."),
                      level = "WARN", className = NULL, methodName = ".conditionHandler")
            break
          }
        }
      }
    }})
  )
}

#'
#' @importFrom methods is
#'
.registerLogger <- function(logger) {
  settings <- .getLoggerSettings()
  settings$loggers[[length(settings$loggers) + 1]] <- logger
  .setLoggerSettings(settings)
  invisible(NULL)
}

.levelToInt <- function(level) {
  if (level == "DEBUG")
    return(2)
  if (level == "INFO")
    return(3)
  if (level == "WARN")
    return(4)
  if (level == "ERROR")
    return(5)
  if (level == "FATAL")
    return(6)
}
