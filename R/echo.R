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
#'   Timestamps are printed in UTC by default.  To control this, set the option
#'   value, such as `options(echo.timezone = "EST")`.
#'
#' @param expr Expression to evaluate; should be written with curly braces (see
#'   examples)
#' @param log A connection or file name for outputs; defaults to `stdout()`
#' @param msg Logical, if `FALSE` does not output a message; defaults to `TRUE`
#' @param level Sets the echo level (see details); defaults to `0L`
#' @param file File path to evaluate (like [base::source()]).  If `file` is not
#'   `NULL`, then `expr` must be missing`
#' @returns Nothing, called for side-effects
#' @examples
#' # make sure to use braces for expr
#' echo(letters, level = 0)   # bad
#' echo({letters}, level = 0) # good
#'
#' try(echo(
#'   expr = {
#'     print(1 + 1)
#'     Sys.sleep(2)
#'     head(mtcars)
#'     message(1)
#'     warning(2)
#'     stop(3)
#'   },
#'   level = 0
#' ))
#'
#'
#' # Parse lines in a file instead
#' try(echo(file = system.file("example-script.R", package = "echo")))
#'
#' # Note that
#' x <- c("example for", "writing lines")
#' echo({
#'   x
#'   print(x)
#'   writeLines(x)
#' }, level = 0)
#' @export
echo <- function(
    expr,
    log = echo_get_log(),  #> stdout()
    msg = echo_get_msg(),  #> TRUE
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
    if (!missing(expr)) {
      stop("If 'file' is not NULL, 'expr' must be missing", call. = FALSE)
    }
    expr <- parse(file)
  } else {
    expr <- as.list(substitute(expr))[-1]
  }

  # TODO add functions for other controls
  for (ex in expr) {
    evaluate(ex, env = env)
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

  if (is.null(value) && identical(output, character())) {
    utils::flush.console()
  } else {
    echo_out(output)
  }

  invisible(value)
}
