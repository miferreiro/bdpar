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

#' Example of the content of the files to be preprocessed.
#'
#' A manually collected data set containing e-mails and SMS messages from the
#' nutritional and health domain classified as spam and non-spam (with a ratio
#' of 50\%). In addition the dataset contains two variables: (i) path which
#' indicates the location of the target file and, (ii) source which contains
#' the raw text comprising each file.
#'
#' @usage data(bdparData)
#'
#' @format A data frame with 20 rows and 2 variables:
#' \describe{
#'   \item{path}{File path.}
#'   \item{source}{File content.}
#' }
"bdparData"
