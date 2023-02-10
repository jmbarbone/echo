cat0 <- function(..., sep = "") {
  cat(..., sep = sep, file = echo_get_log())
}

catln <- function(...) {
  cat0(..., "\n")
}

time <- function() {
  paste0("[", format(Sys.time(), tz = "UTC"), "] ")
}

echo_get_log <- function() {
  echo_var("log", stdout())
}

echo_get_msg <- function() {
  var <- echo_var("to", TRUE)

  if (tolower(var) %in% c("yes", "true")) {
    var <- TRUE
  }

  var
}

echo_get_level <- function(default = 1L) {
  var <- echo_var("level", default)

  if (is.numeric(var)) {
    var <- as.integer(var)
    if (!var %in% 0:4) {
      stop("If echo.level is a number it must be 0, 1, 2, 3, 4", call. = FALSE)
    }
    var <- level_levels()[var + 1L]
  }

  var <- match.arg(as.character(var), level_levels())
  ordered(var, level_levels())
}

level_levels <- function() {
  c("EXP", "OUT", "MSG", "WRN", "ERR", "NUL")
}

echo_var <- function(x, default = NULL) {
  getOption(
    paste0("echo.", tolower(x)),
    sys_get_env(paste0("ECHO_", toupper(x)), default)
  )
}

sys_get_env <- function(x, default = NULL) {
  x <- Sys.getenv(x)

  if (x == "") {
    default
  } else {
    x
  }
}
