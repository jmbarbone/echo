# TODO implement echo levels:
# 4 = EXP : expressions run
# 3 = OUT : expression output
# 2 = MSG : messages
# 1 = WRN : warnings
# 0 = ERR : errors

#' Echo
#'
#' Echo expression or a file
#'
#' @details Levels of output can be controlled with `level`:
#'
#' \describe{
#'   \item{`4`}{`EXP`: logs expressions that were evaluated}
#'   \item{`3`}{`OUT`: logs outputs from expressions}
#'   \item{`2`}{`MSG`: logs messages}
#'   \item{`1`}{`WRN`: logs warnings}
#'   \item{`0`}{`ERR`: logs errors}
#' }
#'
#' When set, all outputs at the `level` or below are run. Errors are always
#' logged as they will interrupt and stop the program.
#'
#' @param expr Expressions to evaluate
#' @param to A connection or file name for outputs
#' @param msg Logical, if `FALSE` does not output a message
#' @param level Sets the echo level (see details)
#' @returns returns the last evaluated value, invisibly
#' @examples
#' try(echo({
#'   1 + 1
#'   Sys.sleep(2)
#'   head(mtcars)
#'   message(1)
#'   warning(2)
#'   stop(3)
#' }))
#' @export
echo <- function(
    exprs,
    log = echo_get_log(),  # stdout()
    msg = echo_get_msg(),  # TRUE
    level = echo_get_level()
) {
  op <- options(
    echo.msg = msg,
    echo.log = log,
    echo.level = level,
    width = max(getOption("width") - 37, 30)
  )
  on.exit(options(op), add = TRUE)

  env <- environment()
  # TODO add feature to read file
  exprs <- as.list(substitute(exprs))[-1]

  # TODO add functions for other controls
  for (expr in exprs) {
    res <- evaluate(expr, env = env)
  }

  invisible(res)
}

evaluate <- function(expr, env = env) {
  dep <- deparse1(expr)
  echo_exp(dep)

  res <- tryCatch(
    eval(as.expression(expr), envir = env),
    message = function(e) {
      echo_msg(e)
      tryInvokeRestart("muffleMessage")
    },
    warning = function(e) {
      echo_wrn(conditionMessage(e))
      tryInvokeRestart("muffleWarning")
    },
    error = function(e) {
      echo_err(conditionMessage(e))
      stop("Error in ", dep, "\n  ", conditionMessage(e), call. = FALSE)
    }
  )

  if (is.null(res)) {
    utils::flush.console()
  } else {
    echo_out(utils::capture.output(res))
  }
}
