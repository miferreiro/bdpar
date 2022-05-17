#
# Bdpar provide a tool to easily build customized data flows to pre-process
# large volumes of information from different sources. To this end, bdpar allows
# to (i) easily use and create new functionalities and (ii) develop new data
# source extractors according to the user needs. Additionally, the package
# provides by default a predefined data flow to extract and preprocess the most
# relevant information (tokens, dates, ... ) from some textual sources (SMS,
# email, tweets, YouTube comments).
#
# Copyright (C) 2020-2022 Sing Group (University of Vigo)
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

#' @title Class implementing a default pipelining process.
#'
#' @description This \code{\link{DefaultPipeline}} class inherits from the
#' \code{\link{GenericPipeline}} class. Includes the \strong{execute} method which
#' provides a default pipelining implementation.
#'
#' @section Details:
#' The default flow is:
#'
#' \preformatted{
#' instance \%>|\%
#'
#'   TargetAssigningPipe$new() \%>|\%
#'
#'   StoreFileExtPipe$new() \%>|\%
#'
#'   GuessDatePipe$new() \%>|\%
#'
#'   File2Pipe$new() \%>|\%
#'
#'   MeasureLengthPipe$new(propertyName = "length_before_cleaning_text") \%>|\%
#'
#'   FindUserNamePipe$new() \%>|\%
#'
#'   FindHashtagPipe$new() \%>|\%
#'
#'   FindUrlPipe$new() \%>|\%
#'
#'   FindEmoticonPipe$new() \%>|\%
#'
#'   FindEmojiPipe$new() \%>|\%
#'
#'   GuessLanguagePipe$new() \%>|\%
#'
#'   ContractionPipe$new() \%>|\%
#'
#'   AbbreviationPipe$new() \%>|\%
#'
#'   SlangPipe$new() \%>|\%
#'
#'   ToLowerCasePipe$new() \%>|\%
#'
#'   InterjectionPipe$new() \%>|\%
#'
#'   StopWordPipe$new() \%>|\%
#'
#'   MeasureLengthPipe$new(propertyName = "length_after_cleaning_text") \%>|\%
#'
#'   TeeCSVPipe$new()
#' }
#'
#' @section Inherit:
#' This class inherits from \code{\link{GenericPipeline}} and implements the
#' \code{execute} abstract function.
#'
#' @seealso \code{\link{bdpar.log}}, \code{\link{Instance}},
#'          \code{\link{DynamicPipeline}}, \code{\link{GenericPipeline}},
#'          \code{\link{GenericPipe}}, \code{\link{\%>|\%}}
#'
#' @keywords NULL
#'
#' @import R6
#' @export DefaultPipeline

DefaultPipeline <- R6Class(

  "DefaultPipeline",

  inherit = GenericPipeline,

  public = list(
    #'
    #' @description Creates a \code{\link{DefaultPipeline}} object.
    #'
    initialize = function() { },
    #'
    #' @description Function where is implemented the flow of the
    #' \code{\link{GenericPipe}s}.
    #'
    #' @param instance A \code{\link{Instance}} value. The \code{\link{Instance}}
    #' that is going to be processed.
    #'
    #' @return The preprocessed \code{\link{Instance}}.
    #'
    execute = function(instance) {

      if (!"Instance" %in% class(instance)) {
        bdpar.log(message = paste0("Checking the type of the 'instance' variable: ",
                                   class(instance)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "execute")
      }

      bdpar.log(message = instance$getPath(),
                level = "INFO",
                className = class(self)[1],
                methodName = "execute")

      tryCatch(
        instance %>|%
          TargetAssigningPipe$new() %>|%
          StoreFileExtPipe$new() %>|%
          GuessDatePipe$new() %>|%
          File2Pipe$new() %>|%
          MeasureLengthPipe$new(propertyName = "length_before_cleaning_text") %>|%
          FindUserNamePipe$new() %>|%
          FindHashtagPipe$new() %>|%
          FindUrlPipe$new() %>|%
          FindEmoticonPipe$new() %>|%
          FindEmojiPipe$new() %>|%
          GuessLanguagePipe$new() %>|%
          ContractionPipe$new() %>|%
          AbbreviationPipe$new() %>|%
          SlangPipe$new() %>|%
          ToLowerCasePipe$new() %>|%
          InterjectionPipe$new() %>|%
          StopWordPipe$new() %>|%
          MeasureLengthPipe$new(propertyName = "length_after_cleaning_text") %>|%
          TeeCSVPipe$new()
        ,
        error = function(e) {
          bdpar.log(message = paste0(instance$getPath()," :", paste(e)),
                    level = "ERROR",
                    className = class(self)[1],
                    methodName = "execute")

          instance$invalidate()
        }
      )
      instance
    },
    #'
    #' @description Gets a list with containing the set of
    #' \code{link{GenericPipe}s} of the pipeline,
    #'
    #' @return The set of \code{\link{GenericPipe}s} containing the pipeline.
    #'
    get = function() {
      list(TargetAssigningPipe$new(),
           StoreFileExtPipe$new(),
           GuessDatePipe$new(),
           File2Pipe$new(),
           MeasureLengthPipe$new(propertyName = "length_before_cleaning_text"),
           FindUserNamePipe$new(),
           FindHashtagPipe$new(),
           FindUrlPipe$new(),
           FindEmoticonPipe$new(),
           FindEmojiPipe$new(),
           GuessLanguagePipe$new(),
           ContractionPipe$new(),
           AbbreviationPipe$new(),
           SlangPipe$new(),
           ToLowerCasePipe$new(),
           InterjectionPipe$new(),
           StopWordPipe$new(),
           MeasureLengthPipe$new(propertyName = "length_after_cleaning_text"),
           TeeCSVPipe$new())
    },
    #'
    #' @description Prints pipeline representation. (Override print function)
    #'
    #' @param ... Further arguments passed to or from other methods.
    #'
    print = function(...) {
      cat("instance %>|%
          TargetAssigningPipe$new() %>|%
          StoreFileExtPipe$new() %>|%
          GuessDatePipe$new() %>|%
          File2Pipe$new() %>|%
          MeasureLengthPipe$new(propertyName = \"length_before_cleaning_text\") %>|%
          FindUserNamePipe$new() %>|%
          FindHashtagPipe$new() %>|%
          FindUrlPipe$new() %>|%
          FindEmoticonPipe$new() %>|%
          FindEmojiPipe$new() %>|%
          GuessLanguagePipe$new() %>|%
          ContractionPipe$new() %>|%
          AbbreviationPipe$new() %>|%
          SlangPipe$new() %>|%
          ToLowerCasePipe$new() %>|%
          InterjectionPipe$new() %>|%
          StopWordPipe$new() %>|%
          MeasureLengthPipe$new(propertyName = \"length_after_cleaning_text\") %>|%
          TeeCSVPipe$new()\n")
    },
    #'
    #' @description Returns a \code{\link{character}} representing the pipeline
    #'
    #' @return \code{\link{DefaultPipeline}} \code{\link{character}} representation
    #'
    toString = function() {
      toRet <- paste0("instance %>|%",
          "\n\tTargetAssigningPipe$new() %>|%",
          "\n\tStoreFileExtPipe$new() %>|%",
          "\n\tGuessDatePipe$new() %>|%",
          "\n\tFile2Pipe$new() %>|%",
          "\n\tMeasureLengthPipe$new(propertyName = \"length_before_cleaning_text\") %>|%",
          "\n\tFindUserNamePipe$new() %>|%",
          "\n\tFindHashtagPipe$new() %>|%",
          "\n\tFindUrlPipe$new() %>|%",
          "\n\tFindEmoticonPipe$new() %>|%",
          "\n\tFindEmojiPipe$new() %>|%",
          "\n\tGuessLanguagePipe$new() %>|%",
          "\n\tContractionPipe$new() %>|%",
          "\n\tAbbreviationPipe$new() %>|%",
          "\n\tSlangPipe$new() %>|%",
          "\n\tToLowerCasePipe$new() %>|%",
          "\n\tInterjectionPipe$new() %>|%",
          "\n\tStopWordPipe$new() %>|%",
          "\n\tMeasureLengthPipe$new(propertyName = \"length_after_cleaning_text\") %>|%",
          "\n\tTeeCSVPipe$new()")
      toRet
    }
  )
)
