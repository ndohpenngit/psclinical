# --- Paired binary (McNemar approximation with correlation) ---
#' Sample size or power for paired binary outcome (McNemar test approximation)
#'
#' Computes either the required sample size or the achieved power
#' for a paired binary design using McNemar's test or correlated proportions.
#'
#' @param p1 Numeric. Event probability at baseline.
#' @param p2 Numeric. Event probability at follow-up.
#' @param rho Numeric. Correlation between paired responses (default 0.5).
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
#' # Required sample size for McNemar test
#' ps_binary_paired(p1 = 0.3, p2 = 0.5, power = 0.8)
#'
#' # Achieved power for given sample size
#' ps_binary_paired(p1 = 0.3, p2 = 0.5, n = 60)
ps_binary_paired <- function(p1, p2, rho = 0.5,
                             n = NULL, power = NULL,
                             sig.level = 0.05,
                             alternative = c("two.sided", "one.sided")) {

  alternative <- match.arg(alternative)
  if (!xor(is.null(n), is.null(power)))
    stop("Provide exactly one of 'n' or 'power'.")

  alpha <- ifelse(alternative == "two.sided", sig.level / 2, sig.level)
  z_alpha <- qnorm(1 - alpha)

  # --- Variance under correlation ---
  # See Fleiss et al. (2013), "Statistical Methods for Rates and Proportions"
  delta <- p2 - p1
  var_d <- p1 * (1 - p1) + p2 * (1 - p2) - 2 * rho * sqrt(p1 * (1 - p1) * p2 * (1 - p2))

  if (var_d <= 0)
    stop("Invalid combination of p1, p2, and rho (variance ≤ 0).")

  if (!is.null(n)) {
    # --- Compute achieved power ---
    z <- abs(delta) * sqrt(n) / sqrt(var_d)
    power_value <- if (alternative == "two.sided") {
      1 - pnorm(z_alpha - z) + pnorm(-z_alpha - z)
    } else {
      1 - pnorm(z_alpha - z)
    }
    power_value <- max(0, min(1, power_value))
    out <- list(achieved_power = power_value, n = n)
  } else {
    # --- Compute required sample size ---
    z_beta <- qnorm(power)
    n_req <- ((z_alpha + z_beta)^2 * var_d) / (delta^2)
    out <- list(n = ceiling(n_req))
  }

  class(out) <- "ssize_result"
  out
}
