# --- One-sample continuous ---
#' Sample size or power for one-sample continuous outcome
#'
#' Computes either the required sample size or the achieved power
#' for a one-sample continuous outcome test.
#'
#' @param delta Numeric. Expected mean difference from the null hypothesis.
#' @param sd Numeric. Standard deviation of the outcome.
#' @param n Numeric. Sample size if computing achieved power.
#' @param power Numeric. Desired power if computing required sample size.
#' @param sig.level Numeric. Type I error (default 0.05).
#' @param alternative Character. "two.sided" or "one.sided".
#'
#' @return List of class \code{ssize_result} with either:
#' \itemize{
#'   \item \code{n} - required sample size (if \code{power} was provided)
#'   \item \code{achieved_power} - achieved power (if \code{n} was provided)
#' }
#' @export
#'
#' @examples
#' # Compute required sample size
#' ps_continuous_onesample(delta = 2, sd = 5, power = 0.8)
#'
#' # Compute achieved power for given sample size
#' ps_continuous_onesample(delta = 2, sd = 5, n = 50)
ps_continuous_onesample <- function(delta, sd, n = NULL, power = NULL,
                                    sig.level = 0.05,
                                    alternative = c("two.sided","one.sided")) {

  alternative <- match.arg(alternative)
  if (!xor(is.null(n), is.null(power))) stop("Provide exactly one of 'n' or 'power'.")

  alpha <- ifelse(alternative == "two.sided", sig.level / 2, sig.level)
  z_alpha <- qnorm(1 - alpha)

  if (!is.null(n)) {
    # Compute achieved power
    z <- delta / (sd / sqrt(n))
    power_value <- if (alternative == "two.sided") {
      pnorm(z - z_alpha) + pnorm(-z - z_alpha)
    } else {
      pnorm(z - z_alpha)
    }
    power_value <- max(0, min(1, power_value))
    out <- list(achieved_power = power_value, n = n)
  } else {
    # Compute required sample size
    z_beta <- qnorm(power)
    n_req <- ((z_alpha + z_beta) * sd / delta)^2
    out <- list(n = ceiling(n_req))
  }

  class(out) <- "ssize_result"
  out
}
