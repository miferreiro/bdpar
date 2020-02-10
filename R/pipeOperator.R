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

#' @title bdpar customized fordward-pipe operator
#'
#' @description Defines a customized fordward pipe operator extending the
#' features of classical \%>\%. Concretely \%>I\% is able to stop the pipelining
#' process whenever an \code{\link{Instance}} has been invalidated. This issue,
#' avoids executing the whole pipelining proccess for the invalidated
#' \code{\link{Instance}} and therefore reduce the time and resources used to
#' complete the whole proccess.
#'
#' @docType methods
#'
#' @format NULL
#'
#' @section Details:
#' This is the \%>\% operator of the modified magrittr library to both
#' (i) to stop the flow when the \code{\link{Instance}} is invalid and (ii)
#' automatically call the \code{pipe} function of the R6 objects passing
#' through it and (iii) check the dependencies of the \code{\link{Instance}}.
#'
#' The usage structure would be as shown below:
#'
#' \preformatted{
#' instance \%>I\%
#'
#' pipeObject$new() \%>I\%
#'
#' pipeObject$new(<<argument1>>, <<argument2>, ...) \%>I\%
#'
#' pipeObject$new()
#' }
#'
#' @section Note:
#' Pipelining proccess is automatically stopped if the \code{\link{Instance}}
#' is invalid.
#'
#' @param lhs an \code{\link{Instance}} object.
#' @param rhs a function call using the bdpar semantics.
#'
#' @usage lhs \%>I\% rhs
#'
#' @return The \code{\link{Instance}} modified by the methods it has traversed.
#'
#' @seealso \code{\link{Instance}}, \code{\link{PipeGeneric}}
#'
#' @keywords NULL
#'
#' @import magrittr
#' @importFrom purrr is_function
#' @export %>I%

`%>I%` <- function(lhs, rhs) {

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

  if (is_placeholder(lhs)) {
    env[["_fseq"]]
  } else {

    env[["_lhs"]] <- eval(lhs, parent, parent)

    result <- withVisible(eval(expr = quote(`_fseq`(`_lhs`)),envir = env,
                               enclos = env))


      result[["value"]]
  }
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
#' @import magrittr
# @export freduce
#

freduce = function(instance, function_list)
{
  k <- length(function_list)

  if (k > 1) {
    for (i in 1:(k - 1L)) {

      pipeName <- parse(text = paste0("class(",
                                      substr(deparse(function_list[[i]])[2],
                                             12,
                                             nchar(deparse(function_list[[i]])[2]) - 16),
                                      ")[1]"))

      exprAlwBefDeps <- parse(text = paste0(substr(deparse(function_list[[i]])[2],
                                                   12,
                                                   nchar(deparse(function_list[[i]])[2]) - 16),
                                            "$getAlwaysBeforeDeps()"))

      exprNotAftDeps <- parse(text = paste0(substr(deparse(function_list[[i]])[2],
                                                   12,
                                                   nchar(deparse(function_list[[i]])[2]) - 16),
                                            "$getNotAfterDeps()"))

      instance$addFlowPipes(eval(pipeName, parent.frame()))

      if (!instance$checkCompatibility(eval(pipeName, parent.frame()), eval(exprAlwBefDeps, parent.frame()))) {
        message("[pipeOperator][freduce][Error] Bad compatibility between Pipes on ", eval(pipeName, parent.frame()))
        break
      }

      instance$addBanPipes(unlist(eval(exprNotAftDeps, parent.frame())))

      instance <- eval(function_list[[i]](instance), parent.frame())
      if (!instance$isInstanceValid()) {
        message("[pipeOperator][freduce][Info] The instance ", instance$getPath(),
                " is invalid and will not continue through the flow of pipes")
        break
      }
    }
  }

  if (instance$isInstanceValid()) {

    pipeName <- parse(text = paste0("class(",
                                    substr(deparse(function_list[[k]])[2],
                                           12,
                                           nchar(deparse(function_list[[k]])[2]) - 16),
                                    ")[1]"))

    exprAlwBefDeps <- parse(text = paste0(substr(deparse(function_list[[k]])[2],
                                                 12,
                                                 nchar(deparse(function_list[[k]])[2]) - 16),
                                          "$getAlwaysBeforeDeps()"))

    exprNotAftDeps <- parse(text = paste0(substr(deparse(function_list[[k]])[2],
                                                 12,
                                                 nchar(deparse(function_list[[k]])[2]) - 16),
                                          "$getNotAfterDeps()"))

    instance$addFlowPipes(eval(pipeName, parent.frame()))

    if (!instance$checkCompatibility(eval(pipeName, parent.frame()), eval(exprAlwBefDeps, parent.frame()))) {
      message("[pipeOperator][freduce][Error] Bad compatibility between Pipes on ", eval(pipeName, parent.frame()))
    } else {

      instance$addBanPipes(unlist(eval(exprNotAftDeps, parent.frame())))

      instance <- eval(function_list[[k]](instance), parent.frame())

      if (!instance$isInstanceValid()) {
        message("[pipeOperator][freduce][Info] The instance ", instance$getPath(),
                " is invalid and will not continue through the flow of pipes")
      }

      instance <- withVisible(instance)
    }
  } else {
    instance <- withVisible(instance)
  }

  instance[["value"]]
}

split_chain = function(expr, env)
{
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

is_pipe = function (pipe)
{
  identical(pipe, quote(`%>I%`))
}

wrap_function = function (body, pipe, env)
{
  eval(call("function", as.pairlist(alist(. = )), body), env, env)
}

is_placeholder = function (symbol)
{
  identical(symbol, quote(.))
}
