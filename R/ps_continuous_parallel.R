#' Continuous outcome: two-sample parallel design
#'
#' Computes sample size or power for a two-sample continuous outcome trial.
#'
#' @param delta Expected mean difference between treatment groups.
#' @param sd Common standard deviation.
#' @param type Type of test: "superiority", "noninferiority", or "equivalence".
#' @param margin Noninferiority or equivalence margin (required for NI/equivalence).
#' @param power Desired power (0-1). Provide either `power` or `n1/n2`.
#' @param n1,n2 Sample sizes per group. Provide either `power` or `n1/n2`.
#' @param sig.level Type I error (default 0.05).
#' @param ratio Allocation ratio n1/n2 (default 1).
#' @param alternative "two.sided" or "one.sided" (default "two.sided").
#'
#' @return A list of class \code{ssize_result} with computed sample sizes, total, and/or achieved power.
#' @export
#'
#' @examples
#' # Sample size determination for superiority design
#' ps_continuous_parallel(delta = 2, sd = 5, power = 0.8)
#'
#' # Power estimation for given sample sizes
#' ps_continuous_parallel(delta = 2, sd = 5, n1 = 50, n2 = 50)
#'
#' # Noninferiority design: sample size determination
#' ps_continuous_parallel(delta = 1, sd = 2, type = "noninferiority",
#'                        margin = 0.5, power = 0.9)
#'
#' # Equivalence design: power estimation
#' ps_continuous_parallel(delta = 1, sd = 2, type = "equivalence",
#'                        margin = 0.5, n1 = 60, n2 = 60)
ps_continuous_parallel <- function(
    delta,
    sd,
    type = c("superiority","noninferiority","equivalence"),
    margin = NULL,
    power = NULL,
    n1 = NULL, n2 = NULL,
    sig.level = 0.05,
    ratio = 1,
    alternative = c("two.sided","one.sided")
) {
  type <- match.arg(type)
  alternative <- match.arg(alternative)
  r <- ratio

  # Adjust alpha for test type and sidedness
  alpha <- ifelse(type == "superiority" & alternative == "two.sided",
                  sig.level / 2, sig.level)
  z_alpha <- qnorm(1 - alpha)

  # --- internal helpers ---
  compute_effective_for_sample <- function(type, delta, margin) {
    if (type == "superiority") {
      list(delta_eff = abs(delta))
    } else if (type == "noninferiority") {
      if (is.null(margin)) stop("Margin must be provided for noninferiority.")
      list(delta_eff = delta + margin)
    } else if (type == "equivalence") {
      if (is.null(margin)) stop("Margin must be provided for equivalence.")
      list(delta_eff = margin - abs(delta))
    }
  }

  compute_n_for_power <- function(power) {
    if (is.null(power) || power <= 0 || power >= 1)
      stop("Power must be between 0 and 1.")
    z_beta <- qnorm(power)
    eff <- compute_effective_for_sample(type, delta, margin)
    n2 <- ceiling(((z_alpha + z_beta)^2 * sd^2 * (1 + 1/r)) / (eff$delta_eff^2))
    n1 <- ceiling(r * n2)
    list(n1 = n1, n2 = n2, total = n1 + n2)
  }

  compute_power_for_n <- function(n1, n2) {
    if (is.null(n1) || is.null(n2) || n1 <= 0 || n2 <= 0)
      stop("n1 and n2 must be positive.")
    se <- sd * sqrt(1/n1 + 1/n2)
    if (type == "superiority") {
      z_beta <- abs(delta)/se - z_alpha
      pnorm(z_beta)
    } else if (type == "noninferiority") {
      z_beta <- (delta + margin)/se - z_alpha
      pnorm(z_beta)
    } else if (type == "equivalence") {
      upper <- (margin - z_alpha * se - delta)/se
      lower <- (-margin + z_alpha * se - delta)/se
      pnorm(upper) - pnorm(lower)
    }
  }

  # --- main logic ---
  if (!is.null(power) & is.null(n1) & is.null(n2)) {
    # Sample size determination
    out <- compute_n_for_power(power)
  } else if (is.null(power) & !is.null(n1) & !is.null(n2)) {
    # Power estimation
    pw <- compute_power_for_n(n1, n2)
    out <- list(power = pw, n1 = n1, n2 = n2, total = n1 + n2)
  } else if (!is.null(power) & !is.null(n1) & !is.null(n2)) {
    # Both provided: return both results
    nreq <- compute_n_for_power(power)
    pw <- compute_power_for_n(n1, n2)
    out <- c(nreq, list(achieved_power = pw))
  } else {
    stop("Provide either power OR n1/n2, or both for full computation.")
  }

  class(out) <- "ssize_result"
  return(out)
}
