
#' Echo progress env
#'
#' Environments for handling the progress bars
#'
#' @details Two progress bar environments are created.  `echo_progress` is the
#'   standard environment which produces a progress bar similar to
#'   [utils::txtProgressBar()] with `style = 3`.  `echo_null` doesn't produce
#'   any outputs but has the same methods as `echo_progress` so that it can be
#'   used in place of `echo_progress` when `progress = FALSE`.
#'
#' @section: fields
#' - `self`: The environment itself
#' - `at`: The current progress (`int`)
#' - `total`: The total value the progress bar goes to (`int`)
#'
#' @section: methods Methods which begin with `.` are conceptually _private_
#' methods and only used internally for _public_ methods to work.  The _private_
#' methods are not available in `echo_null`.
#'
#' - `reset()`: Reset the progress bar
#' - `set(x)`: Sets the progress bar to `x`
#' - `add(x)`: Increment the progress bar by `x`
#' - `show()`: Show the current progress (print)
#' - `flush()`: Flush the progress bar (removes it)
#' - `.width()`: Get the width of the progress bar
#' - `.percent()`: Get the percentage of progress completed
#' - `.progress()`: Get the current character length of progress bar (for width)
#' - `.remaining()`: Get the remaining character length of the progress bar
#'
#' @examples
#' # shorter alias
#' pb <- echo_progress
#' # run within braces to execute together
#' {
#'   # starts
#'   pb$reset()
#'   pb$show()
#'   pb$add(10)
#'
#'   # processing
#'   Sys.sleep(2)
#'
#'   pb$flush()
#'   print(letters)
#'   pb$add(15)
#'   pb$show()
#'
#'   # processing
#'   Sys.sleep(2)
#'
#'   pb$flush()
#'   print(state.abb)
#'   pb$add(20)
#'   pb$show()
#'
#'   # processing
#'   Sys.sleep(2)
#'
#'   pb$flush()
#'   pb$set(100)
#'   pb$show()
#'
#'   pb$reset()
#'   cat("Done!")
#' }
#' @keywords internal
#' @name echo_progress_env
#' @noRd
NULL

#' @rdname echo_progress_env
#' @noRd
echo_progress <- new.env(hash = FALSE)
local(envir = echo_progress, {
  self <- echo_progress
  at <- 0L
  total <- 100L

  reset <- function() {
    self$at <- 0L
    self$.width()
    invisible(self)
  }

  set <- function(x) {
    self$at <- x
    invisible(self)
  }

  add <- function(x) {
    x <- as.integer(x)
    if (is.na(x)) {
      return()
    }

    self$at <- self$at + x
    invisible(self)
  }

  show <- function() {
    if (self$at == 0L) {
      return(invisible())
    }

    cat(
      "  |",
      strrep("=",  self$.progress()),
      strrep(" ", self$.remaining()),
      "|",
      format(floor(self$.percent() * 100L), width = 4L),
      "%",
      strrep(" ", getOption("width") - self$.width()),
      if (self$at == self$total) "\n",
      sep = ""
    )
    flush.console()
  }

  flush <- function() {
    if (self$at == 0L) {
      return(invisible())
    }

    w <- getOption("width")
    bs <- strrep("\b", w)
    cat(bs)
    cat(strrep(" ", w))
    cat(bs)
    cat("\r")
  }

  # 'internal' methods

  .width <- function(x = getOption("width")) {
    max(20L, as.integer(x) - 10L)
  }

  .percent <- function() {
    self$at / self$total
  }

  .progress <- function() {
    floor(self$.percent() * self$.width())
  }

  .remaining <- function() {
    self$.width() - self$.progress()
  }
})
lockBinding("self", echo_progress)
lockBinding("reset", echo_progress)
lockBinding("set", echo_progress)
lockBinding("add", echo_progress)
lockBinding("show", echo_progress)
lockBinding("flush", echo_progress)
lockBinding(".width", echo_progress)
lockBinding(".percent", echo_progress)
lockBinding(".progress", echo_progress)
lockBinding(".remaining", echo_progress)
lockEnvironment(echo_progress)

#' @rdname echo_progress_env
#' @noRd
echo_null <- new.env(hash = FALSE)
local(envir = echo_null, {
  self <- parent.frame()
  total <- 0L
  reset <- function(...) invisible()
  set <- function(...) invisible()
  add <- function(...) invisible()
  show <- function(...) invisible()
  flush <- function(...) invisible()
})

lockBinding("self", echo_null)
lockBinding("reset", echo_null)
lockBinding("set", echo_null)
lockBinding("add", echo_null)
lockBinding("show", echo_null)
lockBinding("flush", echo_null)
lockEnvironment(echo_null)
