#' Print method for ssize_result
#' @param x Object of class \code{ssize_result}
#' @param ... Additional arguments (ignored)
#' @export
print.ssize_result <- function(x, ...) {
  cat("=== Sample Size / Power Result ===\n")
  print.default(x)
  invisible(x)
}
