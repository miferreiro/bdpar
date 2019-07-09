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

# defining a class Email which can be used to parse emails.
read_emails <- function(email_file, PartSelectedOnMPAlternative){

  if (class(email_file) != "character") {
    stop("email_files must be a character vector containing file paths to email txt files..." )
  }

  emails <- data.frame(message = "",
                       date = "",
                       filename = "",
                       stringsAsFactors = FALSE)
  email <- getElement(email_file, PartSelectedOnMPAlternative)

  return(email)
}



getElement = function(filename, PartSelectedOnMPAlternative) {

  path <- paste("exec", "parse.py", sep = "/")

  command <- paste("python", path, filename, "date", PartSelectedOnMPAlternative, sep = " ")
  try(suppressWarnings(response <- system(command,
                                          intern = TRUE,
                                          ignore.stderr = TRUE)), silent = TRUE)

  if (!is.null(attr(response,"status"))) {
      if (attr(response,"status") == 1) {
          response <- ""
      }
  }
  object <- c()
  object["date"] <- response


  command <- paste("python", path, filename, "message", PartSelectedOnMPAlternative, sep = " ")
  try(suppressWarnings(response <- system(command,
                                          intern = TRUE,
                                          ignore.stderr = TRUE)), silent = TRUE)

  if (!is.null(attr(response,"status"))) {
      if (attr(response,"status") == 1) {
          response <- ""
      }
  }
  object["message"] <- response

  return(object)
}


