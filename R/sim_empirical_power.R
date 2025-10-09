#' Simulate empirical power
#'
#' Simulates continuous, binary, or survival outcomes to estimate empirical power.
#' Optionally replicates the simulation multiple times and returns both the
#' vector of empirical power estimates and their mean. Supports multi-core
#' parallelization for faster performance.
#'
#' @param design Character. One of "continuous", "binary", or "survival".
#' @param nsim Integer. Number of simulated trials per replication (default 1000).
#' @param n Integer. Sample size per arm (default 50).
#' @param delta Numeric. Mean difference (required for continuous outcomes).
#' @param sd Numeric. Standard deviation (required for continuous outcomes).
#' @param p1,p2 Numerics. Event probabilities for control and treatment (required for binary outcomes).
#' @param HR Numeric. Hazard ratio (required for survival outcomes).
#' @param lambdaC Numeric. Baseline hazard for control group (required for survival outcomes).
#' @param accrual Numeric. Accrual period (currently not used, placeholder for extensions).
#' @param followup Numeric. Follow-up period (currently not used, placeholder for extensions).
#' @param dropout Numeric. Dropout rate between 0 and 1 (default 0, used for survival outcomes).
#' @param alpha Numeric. Significance level (default 0.05).
#' @param alternative Character. "two.sided" or "one.sided" (default "two.sided").
#' @param nrep Integer. Number of replicated runs for distribution of powers (default 100).
#' @param plot Logical. If TRUE, plots histogram and density of replicated powers (default TRUE).
#' @param seed Integer. Optional random seed for reproducibility.
#' @param ncores Integer. Number of CPU cores for parallel execution
#'  (default: max cores - 1). That is, all but one core detected on the system).
#'
#' @return A list with:
#' \describe{
#'   \item{mean_power}{Mean empirical power across replications.}
#'   \item{rep_powers}{Vector of empirical power estimates (length = nrep).}
#' }
#'
#' @details
#' The function supports three study designs:
#' \itemize{
#'   \item \strong{Continuous:} Simulated via two-sample t-tests.
#'   \item \strong{Binary:} Simulated via two-sample tests for proportions.
#'   \item \strong{Survival:} Simulated via Cox proportional hazards models.
#' }
#'
#' Parallelization is implemented with \code{parallel::parLapply}, which can
#' significantly reduce computation time when \code{nrep} is large.
#'
#' @examples
#' # --- Continuous outcome example ---
#' res1 <- sim_empirical_power(
#'   design = "continuous",
#'   nsim = 1000, n = 50,
#'   delta = 1, sd = 2,
#'   nrep = 50, plot = TRUE
#' )
#' res1$mean_power
#' hist(res1$rep_powers)
#'
#' # --- Binary outcome example ---
#' res2 <- sim_empirical_power(
#'   design = "binary",
#'   nsim = 1000, n = 60,
#'   p1 = 0.3, p2 = 0.5,
#'   nrep = 50, plot = TRUE
#' )
#' res2$mean_power
#'
#' # --- Survival outcome example ---
#' res3 <- sim_empirical_power(
#'   design = "survival",
#'   nsim = 500, n = 40,
#'   HR = 0.7, lambdaC = 0.1,
#'   dropout = 0.05,
#'   nrep = 20, plot = TRUE
#' )
#' res3$mean_power
#'
#' @export
sim_empirical_power <- function(
    design = c("continuous", "binary", "survival"),
    nsim = 1000,
    n = 50,
    delta = NULL, sd = NULL,
    p1 = NULL, p2 = NULL,
    HR = NULL, lambdaC = NULL,
    accrual = NULL, followup = NULL, dropout = 0,
    alpha = 0.05,
    alternative = c("two.sided", "one.sided"),
    nrep = 100,
    plot = TRUE,
    seed = NULL,
    ncores = max(parallel::detectCores() - 1, 1)
) {
  design <- match.arg(design)
  alternative <- match.arg(alternative)
  if (!is.null(seed)) set.seed(seed)

  # --- Input validation ---
  stopifnot(nsim > 0, n > 1, alpha > 0, alpha < 1)
  if (design == "continuous" && (is.null(delta) || is.null(sd)))
    stop("For continuous design, specify both 'delta' and 'sd'.")
  if (design == "binary" && (is.null(p1) || is.null(p2)))
    stop("For binary design, specify both 'p1' and 'p2'.")
  if (design == "survival" && (is.null(HR) || is.null(lambdaC)))
    stop("For survival design, specify both 'HR' and 'lambdaC'.")

  # --- Helper for one simulation batch ---
  run_sim <- function() {
    if (design == "continuous") {
      g1 <- matrix(rnorm(nsim * n, mean = 0, sd = sd), nrow = nsim)
      g2 <- matrix(rnorm(nsim * n, mean = delta, sd = sd), nrow = nsim)
      mean_diff <- rowMeans(g2) - rowMeans(g1)
      se <- sqrt(2 * sd^2 / n)
      tstat <- mean_diff / se
      df <- 2 * n - 2
      pvals <- if (alternative == "two.sided") 2 * pt(-abs(tstat), df)
      else pt(tstat, df, lower.tail = (delta > 0))
      mean(pvals < alpha)

    } else if (design == "binary") {
      x1 <- rbinom(nsim, n, p1)
      x2 <- rbinom(nsim, n, p2)
      p_pool <- (x1 + x2) / (2 * n)
      se <- sqrt(2 * p_pool * (1 - p_pool) / n)
      z <- (x2 / n - x1 / n) / se
      pvals <- if (alternative == "two.sided") 2 * pnorm(-abs(z))
      else pnorm(z, lower.tail = (p2 > p1))
      mean(pvals < alpha)

    } else if (design == "survival") {
      n_total <- 2 * n
      group <- rep(c(0, 1), each = n)
      lambdaT <- lambdaC * HR
      time <- rexp(n_total, rate = ifelse(group == 0, lambdaC, lambdaT))
      status <- rbinom(n_total, 1, 1 - dropout)
      df <- data.frame(time = time, status = status, group = factor(group))
      fit <- survival::coxph(survival::Surv(time, status) ~ group, data = df)
      pval <- summary(fit)$coefficients[, "Pr(>|z|)"]
      if (alternative == "one.sided") pval <- pval / 2
      as.numeric(pval < alpha)
    }
  }

  # --- Parallel execution ---
  cl <- parallel::makeCluster(ncores)
  on.exit(parallel::stopCluster(cl))
  parallel::clusterExport(cl, varlist = ls(), envir = environment())
  rep_powers <- unlist(parallel::parLapply(cl, seq_len(nrep), function(i) run_sim()))

  # --- Optional plot ---
  if (plot) {
    if (length(unique(rep_powers)) > 1) {
      hist(rep_powers, breaks = 20, freq = FALSE,
           col = "skyblue", border = "white",
           main = "Distribution of Empirical Power Estimates",
           xlab = "Estimated Power")
      lines(density(rep_powers), col = "blue", lwd = 2)
      abline(v = mean(rep_powers), col = "darkred", lwd = 2, lty = 2)
    } else {
      hist(rep_powers, breaks = 5, freq = FALSE,
           col = "skyblue", border = "white",
           main = "Empirical Power (single value)",
           xlab = "Estimated Power")
      abline(v = mean(rep_powers), col = "darkred", lwd = 2, lty = 2)
    }
  }

  # --- Return both mean and vector ---
  list(
    mean_power = mean(rep_powers),
    rep_powers = rep_powers
  )
}
