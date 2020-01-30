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
#' through it.
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
    call <- call[-2]
    parse(text = paste0(deparse(call), "$pipe(instance)"))
  })

  env[["_function_list"]] <- lapply(1:length(rhss), function(i) wrap_function(rhss[[i]],
                                                                               pipes[[i]], parent))


  env[["_fseq"]] <- `class<-`(eval(quote(function(value) {freduce(value,
                                                                 `_function_list`)}), env, env), c("fseq", "function"))

  env[["freduce"]] <- freduce

  if (is_placeholder(lhs)) {
    env[["_fseq"]]
  }
  else {

    env[["_lhs"]] <- eval(lhs, parent, parent)

    result <- withVisible(eval(expr = quote(`_fseq`(`_lhs`)),envir = env,
                               enclos = env))
    if (is_compound_pipe(pipes[[1L]])) {
      eval(call("<-", lhs, result[["value"]]), parent,
           parent)
    }
    else {
      if (result[["visible"]])
        result[["value"]]
      else invisible(result[["value"]])
    }
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
#          to stop the flow when the instance is invalid.
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
      instance <- eval(function_list[[i]](instance))
      if (!instance$isInstanceValid()) {
        message("[pipeOperator][freduce][Info] The instance ", instance$getPath(),
            " is invalid and will not continue through the flow of pipes")
        break
      }
    }
  }

  if (instance$isInstanceValid()) {
    instance <- withVisible(eval(function_list[[k]](instance)))
  } else {
    instance <- withVisible(instance)
  }


  if (instance[["visible"]])
    instance[["value"]]
  else invisible(instance[["value"]])
}

split_chain = function(expr, env)
{

  rhss <- list()
  pipes <- list()
  i <- 1L


  while (is.call(expr) && is_pipe(expr[[1L]])) {
    pipes[[i]] <- expr[[1L]]
    rhs <- expr[[3L]]


    if (is_parenthesized(rhs)) {

      rhs <- eval(rhs, env, env)
    }


    rhss[[i]] <- if (is_dollar(pipes[[i]]) || is_funexpr(rhs))
      rhs
    else if (purrr::is_function(rhs))
      prepare_function(rhs)
    else if (is_first(rhs))
      prepare_first(rhs)
    else rhs



    if (is.call(rhss[[i]]) && identical(rhss[[i]][[1L]],
                                        quote(`function`)))
      stop("Anonymous functions must be parenthesized",
           call. = FALSE)
    expr <- expr[[2L]]
    i <- i + 1L
  }


  list(rhss = rev(rhss), pipes = rev(pipes), lhs = expr)
}


is_pipe = function (pipe)
{
  identical(pipe, quote(`%>%`)) || identical(pipe, quote(`%>I%`)) || identical(pipe, quote(`%T>%`)) ||
    identical(pipe, quote(`%<>%`)) || identical(pipe, quote(`%$%`))
}

wrap_function = function (body, pipe, env)
{

  if (is_tee(pipe)) {
    body <- call("{", body, quote(.))
  }
  else if (is_dollar(pipe)) {
    body <- substitute(with(., b), list(b = body))
  }

  eval(call("function", as.pairlist(alist(. = )), body), env,
       env)
}

is_tee = function (pipe)
{
  identical(pipe, quote(`%T>%`))
}

is_parenthesized = function (expr)
{
  is.call(expr) && identical(expr[[1]], quote(`(`))
}

is_dollar = function (pipe)
{
  identical(pipe, quote(`%$%`))
}

is_funexpr = function (expr)
{
  is.call(expr) && identical(expr[[1]], quote(`{`))
}

is_first = function (expr)
{
  !any(vapply(expr[-1], identical, logical(1), quote(.)))
}

prepare_first = function (expr)
{
  as.call(c(expr[[1L]], quote(.), as.list(expr[-1L])))
}

prepare_function = function(f)
{
  as.call(list(f, quote(.)))
}

is_placeholder = function (symbol)
{
  identical(symbol, quote(.))
}

is_compound_pipe = function (pipe)
{
  identical(pipe, quote(`%<>%`))
}

