
echo_echo <- function(x, level = "NUL") {
  if (echo_get_level() > level) {
    return(invisible())
  }

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

echo_exp <- function(x) {
  echo_class("exp", x)
}

echo_out <- function(x) {
  echo_class("out", x)
}

echo_msg <- function(x) {
  echo_class("msg", x)
}

echo_wrn <- function(x) {
  echo_class("wrn", x)
}

echo_err <- function(x) {
  echo_class("err", x)
}

echo_class <- function(class, text) {
  stopifnot(toupper(class) %in% level_levels())
  x <- structure(
    text,
    class = c("echo_echo", paste0("echo_", class), "list")
  )

  echo_echo(x, level = toupper(class))
}
