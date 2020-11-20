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

#' @title bdpar customized forward-pipe operator
#'
#' @description Defines a customized forward pipe operator extending the
#' features of classical \%>\%. Concretely \%>|\% is able to stop the pipelining
#' process whenever an \code{\link{Instance}} has been invalidated. This issue,
#' avoids executing the whole pipelining process for the invalidated
#' \code{\link{Instance}} and therefore reduce the time and resources used to
#' complete the whole process.
#'
#' @docType methods
#'
#' @format NULL
#'
#' @section Details:
#' This is the \%>\% operator of the modified magrittr library to both
#' (i) to stop the flow when the \code{\link{Instance}} is invalid and (ii)
#' automatically call the \code{pipe} function of the R6 objects passing
#' through it (iii) to check the dependencies of the \code{\link{Instance}} and
#' (iv) to manage the pipeline cache.
#'
#' The usage structure would be as shown below:
#'
#' \preformatted{
#' instance \%>|\%
#'
#' pipeObject$new() \%>|\%
#'
#' pipeObject$new(<<argument1>>, <<argument2>, ...) \%>|\%
#'
#' pipeObject$new()
#' }
#'
#' @section Note:
#' Pipelining process is automatically stopped if the \code{\link{Instance}}
#' is invalid.
#'
#' @param lhs an \code{\link{Instance}} object.
#' @param rhs a function call using the bdpar semantics.
#'
#' @usage lhs \%>|\% rhs
#'
#' @return The \code{\link{Instance}} modified by the methods it has traversed.
#'
#' @seealso \code{\link{bdpar.Options}}, \code{\link{Instance}},
#' \code{\link{GenericPipe}}
#'
#' @keywords NULL
#'
#' @export
#' @name operator-pipe

`%>|%` <- function(lhs, rhs) {

  parent <- parent.frame()
  env <- new.env(parent = parent)
  chain_parts <- split_chain(match.call(), env = env)

  pipes <- chain_parts[["pipes"]]
  rhss <- chain_parts[["rhss"]]
  lhs <- chain_parts[["lhs"]]

  rhss <- lapply(rhss, function(call) {
    parse(text = paste0(deparse(call), "$pipe(instance)"))
  })

  env[["_function_list"]] <- lapply(1:length(rhss), function(i) wrap_function(rhss[[i]],
                                                                               pipes[[i]], parent))

  env[["_fseq"]] <- `class<-`(eval(quote(function(value) {freduce(value,
                                                                 `_function_list`)}), env, env), c("fseq", "function"))

  env[["freduce"]] <- freduce


  env[["_lhs"]] <- eval(lhs, parent, parent)

  result <- withVisible(eval(expr = quote(`_fseq`(`_lhs`)), envir = env, enclos = env))

  result[["value"]]
}
#
# @title Apply to the list of functions sequentially and control if the
#        instance is invalid
# @description This function applies the first function to value, then the next
#              function to the result of the previous function call, etc.
#              Execute the functions and allow interruption if the instance is
#              invalid.
# @details This is the freduce method of the modified magrittr library in order
#          to (i) stop the flow when the instance is invalid, (ii) invoke automatically
#          pipe function of object received and (iii) check the dependencies of the Instances.
# @docType methods
# @param value Initial value.
# @param function_list A list of functions.
#
# @return The result after applying each function in turn.
#
# @seealso \code{\link{Instance}}
#' @importFrom digest digest
# @export freduce
#

freduce = function(instance, function_list) {

  cache <- FALSE
  cache.valid <- FALSE

  if (any(!bdpar.Options$isSpecificOption("cache"),
          is.null(bdpar.Options$get("cache")))) {
    bdpar.log(message = "Cache status is not defined in bdpar.Options",
              level = "FATAL", className = NULL, methodName = NULL)
  } else {
    cache <- bdpar.Options$get("cache")
    cache.valid <- cache
  }

  numPipes <- length(function_list)
  cont <- 1
  pipes.executed <- ""
  algo <- "md5"

  while (instance$isInstanceValid() && cont <= numPipes) {

    pipe.name <- parse(text = paste0("class(",
                                    substr(deparse(function_list[[cont]])[2],
                                           12,
                                           nchar(deparse(function_list[[cont]])[2]) - 16),
                                    ")[1]"))

    pipe.id <- substr(eval(parse(text = paste0(substr(deparse(function_list[[cont]])[2],
                                                      12,
                                                      nchar(deparse(function_list[[cont]])[2]) - 16),
                                               "$hash(algo = \"", algo,"\")")),
                           parent.frame()),
                      0,
                      12)

    pipes.executed <- paste(pipes.executed, pipe.id)

    if (cache && cache.valid) {

      if (any(!bdpar.Options$isSpecificOption("cache.folder"),
              is.null(bdpar.Options$get("cache.folder")))) {
        bdpar.log(message = "Cache folder is not defined in bdpar.Options",
                  level = "FATAL", className = NULL, methodName = NULL)
      } else {
        cache.folder <- bdpar.Options$get("cache.folder")
      }

      cache.path <- file.path(cache.folder,
                              substr(digest(object = readLines(con = instance$getPath(),
                                                               warn = FALSE),
                                            algo = algo),
                                     0,
                                     12),
                              paste0(cont,
                                     "-",
                                     substr(digest(object = pipes.executed,
                                                   algo = algo),
                                            0,
                                            12),
                                     "-",
                                     pipe.id,
                                     ".z"))

      if (file.exists(cache.path)) {
        load(file = cache.path)
        instance <- unserialize(srlz)
      } else {
        cache.valid <- FALSE
      }
    }

    exprAlwBefDeps <- parse(text = paste0(substr(deparse(function_list[[cont]])[2],
                                                 12,
                                                 nchar(deparse(function_list[[cont]])[2]) - 16),
                                          "$getAlwaysBeforeDeps()"))

    exprNotAftDeps <- parse(text = paste0(substr(deparse(function_list[[cont]])[2],
                                                 12,
                                                 nchar(deparse(function_list[[cont]])[2]) - 16),
                                          "$getNotAfterDeps()"))

    instance$addFlowPipes(eval(pipe.name,
                               parent.frame()))

    if (!instance$checkCompatibility(eval(pipe.name,
                                          parent.frame()),
                                     eval(exprAlwBefDeps,
                                          parent.frame()))) {

      bdpar.log(message = paste0("Bad compatibility between Pipes on ",
                                 eval(pipe.name,
                                      parent.frame())),
                level = "ERROR", className = "pipeOperator", methodName = NULL)

      break
    }

    instance$addBanPipes(unlist(eval(exprNotAftDeps,
                                     parent.frame())))

    instance <- eval(function_list[[cont]](instance),
                     parent.frame())

    # To avoid building the status message of an instance
    # when the log level is debug
    if (bdpar.Options$.__enclos_env__$private$threshold == "DEBUG") {

      bdpar.log(message = paste0("Instance_ID:", cont,
                                 " (Last pipe: ", eval(pipe.name,
                                                       parent.frame()), ")\n",
                                 instance$toString()),
                level = "DEBUG", className = NULL, methodName = NULL)
    }

    if (cache) {

      if (any(!bdpar.Options$isSpecificOption("cache.folder"),
             is.null(bdpar.Options$get("cache.folder")))) {
        bdpar.log(message = "Cache folder is not defined in bdpar.Options",
                  level = "FATAL", className = NULL, methodName = NULL)
      } else {
        cache.folder <- bdpar.Options$get("cache.folder")
      }

      cache.instance.folder <- file.path(cache.folder,
                                         substr(digest(object = readLines(con = instance$getPath(),
                                                                          warn = FALSE),
                                                        algo = algo),
                                                0,
                                                12))

      new.cache.instance.path <- file.path(cache.instance.folder,
                                           paste0(cont,
                                                  "-",
                                                  substr(digest(object = pipes.executed,
                                                                algo = algo),
                                                         0,
                                                         12),
                                                  "-",
                                                  pipe.id,
                                                  ".z"))

      if (cache && !file.exists(new.cache.instance.path)) {

        if (!dir.exists(cache.instance.folder)) {
          dir.create(cache.instance.folder,
                     recursive = TRUE)
          if (!dir.exists(cache.instance.folder)) {
            bdpar.log(message = paste0("Cannot create directory '",
                                       cache.instance.folder, "'"),
                      level = "FATAL", className = NULL, methodName = NULL)
          }
        }

        srlz <- serialize(object = instance,
                          connection = NULL)
        save(srlz,
             file = new.cache.instance.path)
      }
    }

    if (!instance$isInstanceValid()) {
      bdpar.log(message = paste0("The instance ", instance$getPath(),
                                 " is invalid and will not continue through the flow of pipes"),
                level = "INFO", className = "pipeOperator", methodName = NULL)
      break
    }

    cont <- cont + 1
  }

  instance <- withVisible(instance)
  instance[["value"]]
}

split_chain = function(expr, env) {
  rhss <- list()
  pipes <- list()
  i <- 1L

  while (is.call(expr) && is_pipe(expr[[1L]])) {
    pipes[[i]] <- expr[[1L]]
    rhs <- expr[[3L]]

    rhss[[i]]  <- rhs

    expr <- expr[[2L]]
    i <- i + 1L
  }

  list(rhss = rev(rhss), pipes = rev(pipes), lhs = expr)
}

is_pipe = function (pipe) {
  identical(pipe, quote(`%>|%`))
}

wrap_function = function (body, pipe, env) {
  eval(call("function", as.pairlist(alist(. = )), body), env, env)
}
