
echo_echo <- function(x, level = "NUL") {
  if (echo_get_level() > level) {
    return(invisible())
  }

  print(x)
}

print.echo_exp <- function(x) {
  cat0(time(), "[EXP] ")
  # dep <- deparse1(expr)
  dep <- deparse1(x)
  catln(dep)
}

print.echo_out <- function(x) {
  catln(paste0(time(), "[OUT] #> ", x, collapse = "\n"))
}

print.echo_msg <- function(x) {
  cat0(paste0(time(), "[MSG] #> ", x))
}

print.echo_wrn <- function(x) {
  catln(paste0(time(), "[WRN] #> ", x))
}

print.echo_err <- function(x) {
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
