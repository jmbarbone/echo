
echo_echo <- function(x, level = "NUL", p = NULL) {
  if (echo_get_level() > level) {
    return(invisible())
  }

  if (inherits(p, "echo_progress")) {
    p$flush()
    on.exit(p$show(), add = TRUE)
  }

  op <- options(width = getOption("echo.width", getOption("width", 80L)))
  on.exit(options(op), add = TRUE)

  print(x)
}

#' @export
print.echo_exp <- function(x, ...) {
  catln(time(), "[EXP] ", x)
}

#' @export
print.echo_out <- function(x, ...) {
  if (!length(x) || isTRUE(!nzchar(x))) {
    return(invisible())
  }

  catln(paste0(time(), "[OUT] #> ", x, collapse = "\n"))
}

#' @export
print.echo_msg <- function(x, ...) {
  cat0(paste0(time(), "[MSG] #> ", x))
}

#' @export
print.echo_wrn <- function(x, ...) {
  catln(paste0(time(), "[WRN] #> ", x))
}

#' @export
print.echo_err <- function(x, ...) {
  catln(paste0(time(), "[ERR] #> ", x))
}

echo_exp <- function(x, p = NULL) {
  echo_class("exp", x, p = p)
}

echo_out <- function(x, p = NULL) {
  echo_class("out", x, p = p)
}

echo_msg <- function(x, p = NULL) {
  echo_class("msg", x, p = p)
}

echo_wrn <- function(x, p = NULL) {
  echo_class("wrn", x, p = p)
}

echo_err <- function(x, p = NULL) {
  echo_class("err", x, p = p)
}

echo_class <- function(class, text, p = NULL) {
  stopifnot(toupper(class) %in% level_levels())
  x <- structure(
    text,
    class = c("echo_echo", paste0("echo_", class), "list")
  )

  echo_echo(x, level = toupper(class), p = p)
}
