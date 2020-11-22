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

#' @title Class to manage the preprocess of the files throughout the flow of pipes
#'
#' @description \code{\link{Bdpar}} class provides the static variables required
#' to perform the whole data flow process. To this end \code{\link{Bdpar}} is
#' in charge of (i) initialize the objects of handle the connections to APIs
#' (\code{\link{Connections}}) and handles json resources (\code{\link{ResourceHandler}})
#' and (ii) executing the flow of pipes (inherited from \code{\link{GenericPipeline}} class)
#' passed as argument.
#'
#' @section Details:
#' In the case that some pipe, defined on the workflow, needs some type of
#' configuration, it can be defined through \emph{\link{bdpar.Options}} variable
#' which have different methods to support the functionality of different pipes.
#'
#' @section Static variables:
#' \itemize{
#' \item{\bold{connections}:}{
#' (\emph{Connections}) object that handles the connections with YouTube and
#' Twitter.
#' }
#' \item{\bold{resourceHandler}:}{
#' (\emph{ResourceHandler}) object that handles the json resources files.
#' }
#' }
#'
#' @seealso \code{\link{bdpar.Options}}, \code{\link{Connections}},
#'          \code{\link{DefaultPipeline}}, \code{\link{DynamicPipeline}},
#'          \code{\link{GenericPipeline}}, \code{\link{Instance}},
#'          \code{\link{ExtractorFactory}}, \code{\link{ResourceHandler}},
#'          \code{\link{runPipeline}}
#'
#' @examples
#' \dontrun{
#'
#' #If it is necessary to indicate any configuration, do it through:
#' #bdpar.Options$set(key, value)
#' #If the key is not initialized, do it through:
#' #bdpar.Options$add(key, value)
#'
#' #If it is necessary parallelize, do it through:
#' #bdpar.Options$set("numCores", numCores)
#'
#' #If it is necessary to change the behavior of the log, do it through:
#' #bdpar.Options$configureLog(console = TRUE, threshold = "INFO", file = NULL)
#'
#' #Folder with the files to preprocess
#' path <- system.file(file.path("example"),
#'                     package = "bdpar")
#'
#' #Object which decides how creates the instances
#' extractors <- ExtractorFactory$new()
#'
#' #Object which indicates the pipes' flow
#' pipeline <- DefaultPipeline$new()
#'
#' objectBdpar <- Bdpar$new()
#'
#' #Starting file preprocessing...
#' objectBdpar$execute(path = path,
#'                     extractors = extractors,
#'                     pipeline = pipeline,
#'                     summary = TRUE)
#' }
#' @keywords NULL
#'
#' @import R6
#' @export Bdpar
#' @include wrapper.R

Bdpar <- R6Class(

  "Bdpar",

  public = list(
    #'
    #' @description Creates a \link{Bdpar} object. Initializes the static
    #' variables: \emph{connections} and \emph{resourceHandler}.
    #'
    initialize = function() {
      Bdpar[["private_methods"]][["connections"]] <- function() { Connections$new() }
      Bdpar[["private_methods"]][["resourceHandler"]] <- function() { ResourceHandler$new() }
    },
    #'
    #' @description Preprocess files through the indicated flow of pipes.
    #'
    #' @details In case of wanting to parallelize, it is necessary to indicate
    #' the number of cores to be used through bdpar.Options$set("numCores", numCores)
    #'
    #' @param path A \code{\link{character}} value. The path where the files to
    #' be processed are located.
    #' @param extractors A \code{\link{ExtractorFactory}} value. Class which
    #' implements the \code{createInstance} method to choose which type of
    #' \code{\link{Instance}} is created.
    #' @param pipeline A \code{\link{GenericPipeline}} value. Subclass of
    #' \code{\link{GenericPipeline}}, which implements the \code{execute} method.
    #' By default, it is the \code{\link{DefaultPipeline}} pipeline.
    #' @param summary (\emph{logical}) flag indicating if a summary of the
    #' pipeline execution is provided or not.
    #'
    #' @return The list of \code{Instances} that have been preprocessed.
    #'
    #' @importFrom parallel detectCores
    #'
    execute = function(path,
                       extractors = ExtractorFactory$new(),
                       pipeline = DefaultPipeline$new(),
                       summary = FALSE) {

      if (!"character" %in% class(path)) {
        bdpar.log(message = paste0("Checking the type of the 'path' variable: ",
                                   class(path)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "execute")
      }

      if (!"ExtractorFactory" %in% class(extractors)) {
        bdpar.log(message = paste0("Checking the type of the 'extractors' variable: ",
                                   class(extractors)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "execute")
      }

      if (!inherits(pipeline, c("GenericPipeline"))) {
        bdpar.log(message = paste0("Checking the type of the 'pipeline' variable: ",
                                   class(pipeline)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "execute")
      }

      if (!"logical" %in% class(summary)) {
        bdpar.log(message = paste0("Checking the type of the 'summary' variable: ",
                                   class(summary)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "execute")
      }

      if (all(sapply(path, function(p) file.exists(p) || dir.exists(p)))) {
        files <- unlist(lapply(path, function(p) {
                                       ifelse(dir.exists(p),
                                              return(list.files(path = p,
                                                                recursive = TRUE,
                                                                full.names = TRUE,
                                                                all.files = TRUE)),
                                              return(p))
          }))
      } else {
        bdpar.log(message = paste0("Path parameter must be an existing file ",
                                   "or directory"),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "execute")
      }

      # Create the list of instances, which will contain the date, source, path,
      # data and a list of properties of the file that is in the indicated path.
      InstancesList <- sapply(files, extractors$createInstance)

      bdpar.log(message = paste0("Has been created: ", length(InstancesList),
                                 " instances."),
                level = "INFO",
                className = class(self)[1],
                methodName = "execute")

      if (!bdpar.Options$isSpecificOption("numCores") ||
          is.null(bdpar.Options$get("numCores")) ||
          bdpar.Options$get("numCores") < 1) {
        bdpar.log(message = "The number of cores to be used is incorrectly set (min: 1)",
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "execute")
      } else {
        if (bdpar.Options$get("numCores") == 1) {
          listInstances <- sapply(InstancesList, pipeline$execute)
        } else {

          numCores <- bdpar.Options$get("numCores")

          if (parallel::detectCores() - 2 <  numCores) {
            bdpar.log(message = paste0("The number of cores to be used is incorrectly set (max: ", parallel::detectCores() - 2 , ")") ,
                      level = "FATAL",
                      className = class(self)[1],
                      methodName = "execute")
          } else {

            bdpar.log(message = paste0("Executing the pipeline in parallel mode with ",
                                       numCores, " cores."),
                      level = "INFO",
                      className = class(self)[1],
                      methodName = "execute")

            cl <- private$makeCluster(numberOfThreads = numCores)

            listInstances <- private$clusterApply(cl,
                                                  InstancesList,
                                                  private$executeWrapper,
                                                  pipeline,
                                                  bdpar.Options)
            private$stopCluster(cl)
          }
        }
      }

      bdpar.log(message = "The pipeline execution has been finished!",
                level = "INFO",
                className = class(self)[1],
                methodName = "execute")

      if (summary) {
        private$summary(pipeline = pipeline,
                        listInstances = listInstances)
      }

      listInstances
    }
  ),

  private = list(
    # Initialize the object that handles the different types of connections with
    # Youtube and Twitter
    connections = function() { Connections$new() },
    # Object that handles the json resources files.
    resourceHandler = function() { ResourceHandler$new() },

    summary = function(pipeline, listInstances) {
      if (!inherits(pipeline, c("GenericPipeline"))) {
        bdpar.log(message = paste("Checking the type of the 'pipeline' variable:",
                                   class(pipeline)),
                  level = "FATAL",
                  className = class(self)[1],
                  methodName = "summary")
      }

      if (is.null(listInstances) || !is.list(listInstances) ||
          !all(sapply(listInstances, function(instance) {
            inherits(instance, "Instance")
          }))) {
        bdpar.log(message = paste("List of intances parameter must be a",
                                  "list comprised of 'Instance' objects.",
                                  "Aborting..."),
                  level = "FATAL", className = class(self)[1],
                  methodName = "summary")
      }

      contValid <- sum(sapply(listInstances, function(instance){
        ifelse(instance$isInstanceValid(), 1, 0)
      }))

      contInvalid <- sum(sapply(listInstances, function(instance){
        ifelse(instance$isInstanceValid(), 0, 1)
      }))

      properties <- unique(unlist(lapply(listInstances, function(instance) {
        as.vector(instance$getNamesOfProperties())
      })))

      output <- "Summary after bdpar execution"

      pipelineOutput <- gsub("\\\t", "\\\t\\\t\\\t", pipeline$toString())

      output <- paste0(output, "\n\tPipeline executed: ",
                       "\n\t\t", pipelineOutput)

      output <- paste0(output,
                       "\n\tValid instances: ", contValid,
                       "\n\tInvalid instances: ", contInvalid)

      if (length(listInstances) > 0 && contInvalid > 0) {
        invalidInfo <- ""
        for(i in 1:length(listInstances)) {
          instance <- listInstances[[i]]
          if (!instance$isInstanceValid()) {
            if (instance$isSpecificProperty("reasonToInvalidate")) {
              invalidInfo <- paste0(invalidInfo, "\n\t\t- ",
                                    instance$getPath(), " : ",
                                    instance$getSpecificProperty("reasonToInvalidate"))
            } else {
              invalidInfo <- paste0(invalidInfo, "\n\t\t- ", instance$getPath(),
                                    " : ", "Reason Unknow")
            }
          }
        }

        output <- paste0(output, invalidInfo)
      }

      output <- paste0(output,
                       "\n\tAll the possible properties obtained in the ",
                       "different instances: ", length(properties))

      if (length(properties) > 0) {
        for (i in 1:length(properties)) {
          output <- paste0(output, "\n\t\t- ", properties[i])
        }
      }

      bdpar.log(message = output,
                level = "INFO",
                className = class(self)[1],
                methodName = "summary")
    },
    #'
    #' @importFrom parallel makeCluster
    #'
    makeCluster = function (numberOfThreads) {

      bdpar.log(message = paste0("Initiating cluster with ",
                                 numberOfThreads, " threads"),
                level = "DEBUG",
                className = class(self)[1],
                methodName = "makeCluster")

      cluster <- parallel::makeCluster(numberOfThreads, type = "SOCK")
      logThreadStart <- function(loggers, threadNumber) {
        .clearLoggers()
        for (logger in loggers) {
          .registerLogger(logger)
        }

        options(threadNumber = threadNumber)

        bdpar.log(message = paste0("Thread ", threadNumber, " initiated"),
                  level = "DEBUG",
                  className = class(self)[1],
                  methodName = "makeCluster")

        finalize <- function(env) {
          bdpar.log(message = paste0("Thread ", threadNumber, " terminated"),
                    level = "DEBUG",
                    className = class(self)[1],
                    methodName = "makeCluster")
        }
        reg.finalizer(globalenv(), finalize, onexit = TRUE)
        return(NULL)
      }
      loggers <- .getLoggerSettings()$loggers
      for (i in 1:length(cluster)) {
        parallel:::sendCall(cluster[[i]], logThreadStart, list(loggers = loggers,
                                                               threadNumber = i))
      }
      for (i in 1:length(cluster)) {
        parallel:::recvOneResult(cluster)
      }

      cluster
    },
    #'
    #' @importFrom parallel stopCluster
    #'
    stopCluster = function(cluster) {
      parallel::stopCluster(cluster)
      bdpar.log(message = "Stopping cluster",
                level = "INFO",
                className = class(self)[1],
                methodName = "stopCluster")
    },

    clusterApply = function (cluster, x, fun, ...) {

      n <- length(x)
      p <- length(cluster)
      if (n > 0 && p > 0) {
        for (i in 1:min(n, p)) {
          parallel:::sendCall(cluster[[i]], private$functionWrapper,
                              c(list(x[[i]]), list(...), list(fun = fun)),
                              tag = i)
        }
        val <- vector("list", n)
        hasError <- FALSE
        formatError <- function(threadNumber, error, args) {
          sprintf("Thread %s returns error: \"%s\" when using argument(s): %s",
                  threadNumber, gsub("\n", "\\n", gsub("\t", "\\t", error)),
                  gsub("\n", "\\n", gsub("\t","\\t", paste(args, collapse = ","))))
        }
        for (i in 1:n) {
          d <- parallel:::recvOneResult(cluster)
          if (inherits(d$value, "try-error")) {
            val[d$tag] <- NULL
            errorMessage <- formatError(d$node, d$value,
                                        c(list(x[[d$tag]])))

            bdpar.log(message = errorMessage, level = "ERROR",
                      className = class(self)[1], methodName = "clusterApply")

            hasError <- TRUE

          }
          j <- i + min(n, p)
          if (j <= n) {
            parallel:::sendCall(cluster[[d$node]], fun, c(list(x[[j]]),
                                                          list(...)), tag = j)
          }
          val[d$tag] <- list(d$value)
        }
        if (hasError) {
          bdpar.log(message = paste0("Error(s) when calling function '",
                                     substitute(fun, parent.frame(1)), "', see earlier messages for details"),
                    level = "ERROR", className = class(self)[1], methodName = "clusterApply")
        }
        return(val)
      }
    },

    functionWrapper = function (..., fun = fun) {
      handler <- function(e) {
        stop(conditionMessage(e))
      }
      withCallingHandlers(do.call("fun", lapply(list(...), enquote)), error = handler)
    },

    executeWrapper = function (InstancesList, pipeline, bdpar.Options) {
      assignInNamespace("bdpar.Options", bdpar.Options, "bdpar")
      pipeline$execute(InstancesList)
    }
  )
)
