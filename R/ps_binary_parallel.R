#' Binary outcome: two-sample parallel design
#'
#' Computes sample size or power for a two-sample binary outcome trial.
#'
#' @param p1 Event probability in control.
#' @param p2 Event probability in treatment.
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
#' # Sample size determination (superiority)
#' ps_binary_parallel(p1 = 0.3, p2 = 0.2, power = 0.8)
#'
#' # Power estimation for given sample sizes
#' ps_binary_parallel(p1 = 0.3, p2 = 0.2, n1 = 50, n2 = 50)
#'
#' # Noninferiority design: sample size determination
#' ps_binary_parallel(p1 = 0.3, p2 = 0.25, type = "noninferiority",
#'                    margin = 0.05, power = 0.9)
#'
#' # Equivalence design: power estimation
#' ps_binary_parallel(p1 = 0.3, p2 = 0.25, type = "equivalence",
#'                    margin = 0.05, n1 = 60, n2 = 60)
ps_binary_parallel <- function(
    p1,
    p2,
    type = c("superiority","noninferiority","equivalence"),
    margin = NULL,
    power = NULL,
    n1 = NULL,
    n2 = NULL,
    sig.level = 0.05,
    ratio = 1,
    alternative = c("two.sided","one.sided")
) {
  type <- match.arg(type)
  alternative <- match.arg(alternative)
  r <- ratio
  var1 <- p1 * (1 - p1)
  var2 <- p2 * (1 - p2)
  alpha <- ifelse(type == "superiority" & alternative == "two.sided", sig.level / 2, sig.level)
  z_alpha <- qnorm(1 - alpha)

  compute_effective_for_sample <- function(type, p1, p2, margin) {
    diff <- p1 - p2
    if (type == "superiority") return(list(diff_eff = abs(diff)))
    if (type == "noninferiority") {
      if (is.null(margin)) stop("Margin must be provided for noninferiority.")
      return(list(diff_eff = diff + margin))
    }
    if (type == "equivalence") {
      if (is.null(margin)) stop("Margin must be provided for equivalence.")
      return(list(diff_eff = margin - abs(diff)))
    }
  }

  compute_n_for_power <- function(power) {
    if (is.null(power) || power <= 0 || power >= 1) stop("Power must be between 0 and 1.")
    z_beta <- qnorm(power)
    eff <- compute_effective_for_sample(type, p1, p2, margin)
    if (eff$diff_eff == 0) stop("Effective difference is zero; cannot compute sample size.")
    n2 <- ceiling(((z_alpha + z_beta)^2 * (var1 + var2 / r)) / (eff$diff_eff^2))
    n1 <- ceiling(r * n2)
    list(n1 = n1, n2 = n2, total = n1 + n2)
  }

  compute_power_for_n <- function(n1, n2) {
    if (is.null(n1) || is.null(n2) || n1 <= 0 || n2 <= 0) stop("n1 and n2 must be positive.")
    se <- sqrt(var1 / n1 + var2 / n2)
    diff <- p1 - p2
    if (type == "superiority") {
      z_beta <- abs(diff) / se - z_alpha
      return(pnorm(z_beta))
    }
    if (type == "noninferiority") {
      z_beta <- (diff + margin) / se - z_alpha
      return(pnorm(z_beta))
    }
    if (type == "equivalence") {
      upper <- (margin - z_alpha * se - diff) / se
      lower <- (-margin + z_alpha * se - diff) / se
      return(max(0, min(1, pnorm(upper) - pnorm(lower))))  # bounded
    }
  }

  if (!is.null(power) & is.null(n1) & is.null(n2)) {
    out <- compute_n_for_power(power)
  } else if (is.null(power) & !is.null(n1) & !is.null(n2)) {
    pw <- compute_power_for_n(n1, n2)
    out <- list(power = pw, n1 = n1, n2 = n2, total = n1 + n2)
  } else if (!is.null(power) & !is.null(n1) & !is.null(n2)) {
    nreq <- compute_n_for_power(power)
    pw <- compute_power_for_n(n1, n2)
    out <- c(nreq, list(achieved_power = pw))
  } else {
    stop("Provide either power OR n1/n2, or both for full computation.")
  }

  class(out) <- "ssize_result"
  return(out)
}
