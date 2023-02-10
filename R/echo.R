#' Echo
#'
#' Echo expression or a file
#'
#' @details Levels of output can be controlled with `level`:
#'
#' \describe{
#'   \item{`0`}{`EXP`: logs expressions that were evaluated}
#'   \item{`1`}{`OUT`: logs outputs from expressions}
#'   \item{`2`}{`MSG`: logs messages}
#'   \item{`3`}{`WRN`: logs warnings}
#'   \item{`4`}{`ERR`: logs errors}
#' }
#'
#'   When set, all outputs at the `level` or below are run. Errors are always
#'   logged as they will interrupt and stop the program.
#'
#' @param exprs Expressions to evaluate.
#' @param to A connection or file name for outputs
#' @param msg Logical, if `FALSE` does not output a message
#' @param level Sets the echo level (see details)
#' @param file File path to evaluate (like [base::source()]).  If `file` is not
#'   `NULL`, then `exprs` must be missing.
#' @returns Nothing, called for side-effects
#' @examples
#' try(echo({
#'   1 + 1
#'   Sys.sleep(2)
#'   head(mtcars)
#'   message(1)
#'   warning(2)
#'   stop(3)
#' }))
#'
#' try(echo(file = system.file("example-script.R", package = "echo")))
#' @export
echo <- function(
    exprs,
    log = echo_get_log(),  # stdout()
    msg = echo_get_msg(),  # TRUE
    level = echo_get_level(),
    file = NULL
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
  if (!is.null(file)) {
    if (!missing(exprs)) {
      stop("If 'file' is not NULL, 'expr' must be missing", call. = FALSE)
    }
    exprs <- parse(file)
  } else {
    exprs <- as.list(substitute(exprs))[-1]
  }

  # TODO add functions for other controls
  for (expr in exprs) {
    evaluate(expr, env = env)
  }

  invisible()
}

evaluate <- function(expr, env = env) {
  dep <- deparse1(expr)
  echo_exp(dep)

  output <- utils::capture.output(value <- tryCatch(
    eval(as.expression(expr), envir = env),
    message = function(e) {
      echo_msg(conditionMessage(e))
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
  ))

  if (is.null(value)) {
    utils::flush.console()
  } else {
    echo_out(output)
  }

  invisible(value)
}
