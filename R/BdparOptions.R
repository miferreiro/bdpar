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

#' @import R6 rlist
#' @include wrapper.R

BdparOptions <- R6Class(

  "BdparOptions",

  public = list(

    initialize = function() {

      private$bdpar.options <- list(extractorEML.mpaPartSelected = "text/plain",
                                    resources.abbreviations.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                             "bdpar",
                                                                             "resources",
                                                                             "abbreviations-json"),
                                    resources.contractions.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                            "bdpar",
                                                                            "resources",
                                                                            "contractions-json"),
                                    resources.interjections.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                             "bdpar",
                                                                             "resources",
                                                                             "interjections-json"),
                                    resources.slangs.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                      "bdpar",
                                                                      "resources",
                                                                      "slangs-json"),
                                    resources.stopwords.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                         "bdpar",
                                                                         "resources",
                                                                         "stopwords-json"),
                                    twitter.consumer.key = NULL,
                                    twitter.consumer.secret = NULL,
                                    twitter.access.token = NULL,
                                    twitter.access.token.secret = NULL,
                                    cache.twitter.path = NULL,
                                    teeCSVPipe.output.path = "teeCSVPipe.output.csv",
                                    youtube.app.id = NULL,
                                    youtube.app.password = NULL,
                                    cache.youtube.path = NULL,
                                    cache = TRUE,
                                    cache.folder = ".cache",
                                    numCores = 1,
                                    verbose = FALSE)

      private$bdpar.log.layout.console <- function(level, ...) {

        if (!bdpar.Options$isSpecificOption("numCores") ||
            is.null(bdpar.Options$get("numCores")) ||
            bdpar.Options$get("numCores") <= 1 ||
            is.null(getOption("threadNumber"))) {

          dots <- list(...)
          className <- ""
          methodName <- ""

          if (!is.null(dots[[1]][[1]])) {
            className <- paste0("[", dots[[1]][[1]], "]")
          }
          if (!is.null(dots[[1]][[2]])) {
            methodName <- paste0("[", dots[[1]][[2]], "]")
          }

          message <- dots[[1]][[3]]

          if (identical(level, "DEBUG") ||
              identical(level, "INFO") ||
              identical(level, "ERROR")) {
            message("[", format(Sys.time()), "]", className, methodName,
                    "[", level, "] ", message)
          } else {
            if (identical(level, "WARN")) {
              warning("[", format(Sys.time()), "]", className, methodName,
                      "[", level, "] ", message, call. = FALSE)
            } else {
              if (identical(level, "FATAL")) {
               stop("[", format(Sys.time()), "]", className, methodName,
                    "[", level, "] ", message, call. = FALSE)
              }
            }
          }
        }
      }

      private$bdpar.log.layout.file <- function(level, ...) {

        dots <- list(...)
        className <- ""
        methodName <- ""

        if (!is.null(dots[[1]][[1]])) {
          className <- paste0("[", dots[[1]][[1]], "]")
        }
        if (!is.null(dots[[1]][[2]])) {
          methodName <- paste0("[", dots[[1]][[2]], "]")
        }
        message <- dots[[1]][[3]]

        if (!bdpar.Options$isSpecificOption("numCores") ||
            is.null(bdpar.Options$get("numCores")) ||
            bdpar.Options$get("numCores") <= 1) {
          paste0("[", format(Sys.time()), "]", className, methodName,
                 "[", level, "] ", message, collapse = "")
        } else {

          threadNumber <- getOption("threadNumber")
          if (is.null(threadNumber)) {
            threadLabel <- "[Main thread]"
          } else {
            threadLabel <- paste0("[Thread", threadNumber, "]")
          }

          paste0("[", format(Sys.time()), "]", threadLabel, className, methodName,
                 "[", level, "] ", message, collapse = "")
        }
      }

      private$threshold <- "INFO"
      appenders <- list(private$console_appender_bdpar(layout = private$bdpar.log.layout.console))

      .clearLoggers()
      logger <- private$createLoggerCustom(name = "SIMPLE",
                                           threshold = private$threshold,
                                           appenders = appenders)
      .registerLogger(logger)
      private$bdpar.log.console <-  TRUE
    },

    get = function(key) {

      if (!"character" %in% class(key)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[get][FATAL] Checking the type of the 'key' variable: ",
             class(key))
      }

      if (!key %in% names(private$bdpar.options)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[get][FATAL] '", key, "' option is not configured")
      }
      private$bdpar.options[[key]]
    },

    add = function(key, value) {

      if (!"character" %in% class(key)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[add][FATAL] Checking the type of the 'key' variable: ",
             class(key))
      }

      if (self$isSpecificOption(key)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[add][FATAL] '", key, "' option is already ",
             "configured with the value: ", self$get(key))
      } else {
        private$bdpar.options <- list.append(private$bdpar.options, value)
        names(private$bdpar.options)[length(private$bdpar.options)] <- key
      }
    },

    set = function(key, value) {

      if (!"character" %in% class(key)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[set][FATAL] Checking the type of the 'key' variable: ",
             class(key))
      }

      if (!self$isSpecificOption(key)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[set][FATAL] '", key, "' option is not configured")
      } else {
        if (is.null(value)) {
          private$bdpar.options[key] <- list(value)
        } else {
          private$bdpar.options[[key]] <- value
        }
      }
    },

    remove = function(key) {
      if (!"character" %in% class(key)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[remove][FATAL] Checking the type of the 'key' variable: ",
             class(key))
      }

      if (!self$isSpecificOption(key)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[remove][FATAL] '", key, "' option is not configured")
      } else {
        private$bdpar.options <- list.remove(private$bdpar.options, key)
      }
    },

    getAll = function() {
      private$bdpar.options
    },

    reset = function() {
      private$bdpar.options <- list(extractorEML.mpaPartSelected = "text/plain",
                                    resources.abbreviations.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                             "bdpar",
                                                                             "resources",
                                                                             "abbreviations-json"),
                                    resources.contractions.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                            "bdpar",
                                                                            "resources",
                                                                            "contractions-json"),
                                    resources.interjections.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                             "bdpar",
                                                                             "resources",
                                                                             "interjections-json"),
                                    resources.slangs.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                      "bdpar",
                                                                      "resources",
                                                                      "slangs-json"),
                                    resources.stopwords.path = file.path(Sys.getenv("R_LIBS_USER"),
                                                                         "bdpar",
                                                                         "resources",
                                                                         "stopwords-json"),
                                    twitter.consumer.key = NULL,
                                    twitter.consumer.secret = NULL,
                                    twitter.access.token = NULL,
                                    twitter.access.token.secret = NULL,
                                    cache.twitter.path = NULL,
                                    teeCSVPipe.output.path = "teeCSVPipe.output.csv",
                                    youtube.app.id = NULL,
                                    youtube.app.password = NULL,
                                    cache.youtube.path = NULL,
                                    cache = TRUE,
                                    cache.folder = ".cache",
                                    numCores = 1,
                                    verbose = FALSE)
    },

    isSpecificOption = function(key) {
      key %in% names(private$bdpar.options)
    },

    cleanCache = function() {
      if (!self$isSpecificOption("cache.folder") ||
          is.null(self$get("cache.folder"))) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[cleanCache][Error] Cache folder ",
             "is not defined in bdpar.Options")
      }

      unlink(self$get("cache.folder"),
             recursive = T)

      if (!dir.exists(self$get("cache.folder"))) {
        message("[", format(Sys.time()), "][", class(self)[1], "]",
                "[cleanCache][Info] The cache folder \"",
                self$get("cache.folder"),
                "\" has been deleted successfully!")
      } else {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[cleanCache][Error] The cache folder \"",
             self$get("cache.folder"),
             "\" could not deleted correctly!")
      }
    },

    configureLog = function(console = TRUE, threshold = "INFO", file = NULL) {

      if (is.null(threshold) ||
          !is.character(threshold) ||
          !any(c("FATAL", "ERROR", "WARN", "INFO", "DEBUG") %in% threshold)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[configureLog][FATAL] The 'threshold' parameter ",
             "must be between these values: FATAL, ERROR, WARN, INFO or DEBUG")
      }

      if (!"logical" %in% class(console)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[configureLog][FATAL] ",
             "Checking the type of the 'console' variable: ",
             class(console))
      }

      private$threshold <- threshold
      .clearLoggers()

      if (!is.null(file) &&
          is.character(file)) {
        appenders <- list(private$file_appender_bdpar(layout  = private$bdpar.log.layout.file,
                                                      fileName = file))

        logger <- private$createLoggerCustom(name = "file_logger",
                                             threshold = threshold,
                                             appenders = appenders)
        .registerLogger(logger)

        private$bdpar.log.file <- file
      } else {
        private$bdpar.log.file <- FALSE
      }

      if (console) {
        appenders <- list(private$console_appender_bdpar(layout = private$bdpar.log.layout.console))

        logger <- private$createLoggerCustom(name = "console_logger",
                                             threshold = threshold,
                                             appenders = appenders)
        .registerLogger(logger)

        private$bdpar.log.console <- TRUE
      } else {
        private$bdpar.log.console <- FALSE
      }

      if (all(isFALSE(private$bdpar.log.file),
              isFALSE(private$bdpar.log.console))) {
        .clearLoggers()
      }
    },

    disableLog = function() {
      .clearLoggers()
      private$bdpar.log.console <- FALSE
      private$bdpar.log.file <- FALSE
      private$threshold <- "INFO"
    },

    getLogConfiguration = function() {
      message("[", format(Sys.time()), "][", class(self)[1], "]",
              "[getLogConfiguration][INFO] Log configuration:\n",
              "\t- Threshold: ",
              ifelse(all(!private$bdpar.log.console,
                         is.null(private$bdpar.log.file)),
                     "Not configured",
                     as.character(private$threshold)), "\n",
              "\t- Console log status: ",
              ifelse(private$bdpar.log.console,
                     "Actived",
                     "Disabled"), "\n",
              "\t- File log status: ",
              ifelse(!isFALSE(private$bdpar.log.file),
                     paste0("Actived. File asociated: ", private$bdpar.log.file),
                     "Disabled"))
    },

    print = function(...) {
      print(self$getAll())
    }
  ),

  private = list(
    bdpar.options = list(),
    threshold = "",
    bdpar.log.layout.console = NULL,
    bdpar.log.layout.file = NULL,
    bdpar.log.console = FALSE,
    bdpar.log.file = FALSE,
    console_appender_bdpar = function(layout = private$bdpar.log.layout.console) {
      .appendFunction <- function(this, level, message) { }
      appender <- list(.appendFunction = .appendFunction,
                       layout = layout)
      class(appender) <- "Appender"
      appender
    },

    file_appender_bdpar = function(layout = private$bdpar.log.layout.file, fileName) {
      .appendFunction <- function(this, level, message) {
        con <- file(fileName, open = "at", blocking = FALSE)
        writeLines(text = message, con = con)
        flush(con)
        close(con)
      }
      appender <- list(.appendFunction = .appendFunction,
                       layout = layout,
                       fileName = fileName)
      class(appender) <- "Appender"
      appender
    },

    createLoggerCustom = function(name = "SIMPLE",
                                   threshold = "INFO",
                                   appenders) {

      .logFunction <- function(this, level, message) {
        for (appender in this$appenders) {
          formatted <- appender$layout(level, message)
          appender$.appendFunction(appender, level, formatted)
        }
      }

      logger <- list(name = name,
                     .logFunction = .logFunction,
                     threshold = threshold,
                     appenders = appenders)
      class(logger) <- "Logger"
      logger
    }
  )
)
