
# echo (development version)

* `echo()` gains new `progress` param to print a progress bar, similar to `utils::txtProgressBar(style = 3)` [#2](https://github.com/jmbarbone/echo/issues/2)
* `echo()` now handles single expressions better (e.g., `echo({ letters })` and `echo(letters)` should produce the same result)
* `echo()` gains new `expr` param to evaluate an `expression` object instead of `expr` or `file`

# echo 0.1.0

* Added a `NEWS.md` file to track changes to the package.
