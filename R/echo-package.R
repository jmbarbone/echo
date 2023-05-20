#' @aliases echo_package
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

op.echo <- list( # nolint: object_name_linter.
  echo.timezone = "UTC"
)

.onAttach <- function(libname, pkgname) {
  options(op.echo[!names(op.echo) %in% names(options())])
}
