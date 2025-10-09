#' Simulate empirical power for continuous, binary, or survival outcomes
#'
#' Supports two-arm parallel, one-sample, and paired designs.
#'
#' @param design Character. One of "continuous", "binary", "survival"
#' @param nsim Integer. Number of simulated trials per replication
#'  (default 1000)
#' @param n Numeric. Sample size per arm or per group (default 50)
#' @param delta Numeric. Mean difference (required for continuous outcomes)
#' @param sd Numeric. Standard deviation (required for continuous outcomes except paired)
#' @param sd_diff Numeric. Standard deviation of within-subject differences (for paired continuous design)
#' @param p1,p2 Numeric. Event probabilities for control and treatment
#'  (required for binary outcomes)
#' @param paired Logical. If TRUE, simulate paired design (default FALSE)
#' @param one_sample Logical. If TRUE, simulate one-sample test (default FALSE)
#' @param HR Numeric. Hazard ratio (required for survival outcomes)
#' @param lambdaC Numeric. Baseline hazard for control group
#'  (required for survival outcomes)
#' @param accrual,followup Numeric. Placeholder for time-to-event parameters
#' @param dropout Numeric. Dropout rate between 0 and 1 (default 0)
#' @param alpha Numeric. Significance level (default 0.05)
#' @param alternative Character. "two.sided" or "one.sided" (default "two.sided")
#' @param nrep Integer. Number of replicated runs (default 100)
#' @param plot Logical. If TRUE, plot distribution of simulated powers (default TRUE)
#' @param seed Integer. Optional random seed
#' @param ncores Integer. Number of CPU cores for parallel execution
#'
#' @return List with:
#'   \item{mean_power}{Mean empirical power across replications}
#'   \item{rep_powers}{Vector of empirical power estimates (length = nrep)}
#'   \item{plot_powers}{Plot object if plot=TRUE}
#' @export
#'
#' @examples
#' # One-sample continuous
#' res1 <- sim_empirical_power(design="continuous", n=50, delta=2, sd=5,
#'                             one_sample=TRUE, nrep=50)
#' res1$mean_power
#'
#' # Paired binary
#' res2 <- sim_empirical_power(design="binary", n=60, p1=0.3, p2=0.5,
#'                             paired=TRUE, nrep=50)
#' res2$mean_power
#'
#' # Two-arm parallel continuous
#' res3 <- sim_empirical_power(design="continuous", n=50, delta=2, sd=5,
#'                             nrep=50)  # default: parallel
#' res3$mean_power
#'
#' # Two-arm parallel binary
#' res4 <- sim_empirical_power(design="binary", n=60, p1=0.3, p2=0.5,
#'                             nrep=50)
#' res4$mean_power
#'
#' # Two-arm survival
#' res5 <- sim_empirical_power(design="survival", n=40, HR=0.7, lambdaC=0.1,
#'                             dropout=0.05, nrep=20)
#' res5$mean_power
sim_empirical_power <- function(
    design = c("continuous", "binary", "survival"),
    nsim = 1000,
    n = 50,
    delta = NULL, sd = NULL, sd_diff = NULL,
    p1 = NULL, p2 = NULL,
    paired = FALSE,
    one_sample = FALSE,
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
  stopifnot(nsim > 0, n > 1, alpha > 0, alpha < 1)

  # --- Input validation ---
  if (design == "continuous") {
    if (paired) {
      if (is.null(delta) || is.null(sd_diff))
        stop("For paired continuous design, specify both 'delta' and 'sd_diff'.")
    } else if (one_sample) {
      if (is.null(delta) || is.null(sd))
        stop("For one-sample continuous design, specify both 'delta' and 'sd'.")
    } else {
      if (is.null(delta) || is.null(sd))
        stop("For two-sample continuous design, specify both 'delta' and 'sd'.")
    }
  }

  if (design == "binary" && one_sample && is.null(p1))
    stop("For one-sample binary, specify 'p1'.")
  if (design == "binary" && !one_sample && (is.null(p1) || is.null(p2)))
    stop("For binary design, specify both 'p1' and 'p2'.")
  if (design == "survival" && (is.null(HR) || is.null(lambdaC)))
    stop("For survival design, specify both 'HR' and 'lambdaC'.")

  # --- One simulation batch ---
  run_sim <- function() {
    if (design == "continuous") {
      if (one_sample) {
        x <- matrix(rnorm(nsim * n, mean = delta, sd = sd), nrow = nsim)
        se <- sd / sqrt(n)
        tstat <- rowMeans(x) / se
        df <- n - 1

      } else if (paired) {
        # Paired continuous: simulate difference directly
        diff_mat <- matrix(rnorm(nsim * n, mean = delta, sd = sd_diff), nrow = nsim)
        se <- sd_diff / sqrt(n)
        tstat <- rowMeans(diff_mat) / se
        df <- n - 1

      } else {
        # Two-sample parallel continuous
        g1 <- matrix(rnorm(nsim * n, mean = 0, sd = sd), nrow = nsim)
        g2 <- matrix(rnorm(nsim * n, mean = delta, sd = sd), nrow = nsim)
        mean_diff <- rowMeans(g2) - rowMeans(g1)
        se <- sqrt(2 * sd^2 / n)
        tstat <- mean_diff / se
        df <- 2 * n - 2
      }

      pvals <- if (alternative == "two.sided") {
        2 * pt(-abs(tstat), df)
      } else {
        pt(tstat, df, lower.tail = (delta > 0))
      }
      mean(pvals < alpha)

    } else if (design == "binary") {
      # --- Binary designs ---
      if (one_sample) {
        x <- rbinom(nsim, n, p1)
        se <- sqrt(p1 * (1 - p1) / n)
        z <- (x / n - 0) / se
      } else if (paired) {
        x1 <- rbinom(nsim, n, p1)
        x2 <- rbinom(nsim, n, p2)
        diff <- x2 - x1
        se <- sqrt(p1 * (1 - p1) + p2 * (1 - p2) -
                     2 * sqrt(p1 * (1 - p1) * p2 * (1 - p2))) / sqrt(n)
        z <- diff / se
      } else {
        x1 <- rbinom(nsim, n, p1)
        x2 <- rbinom(nsim, n, p2)
        p_pool <- (x1 + x2) / (2 * n)
        se <- sqrt(2 * p_pool * (1 - p_pool) / n)
        z <- (x2 / n - x1 / n) / se
      }
      pvals <- if (alternative == "two.sided") {
        2 * pnorm(-abs(z))
      } else {
        pnorm(z, lower.tail = (p2 > p1))
      }
      mean(pvals < alpha)

    } else if (design == "survival") {
      # --- Survival ---
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

  # --- Plot results ---
  if (plot) {
    if (length(unique(rep_powers)) > 1) {
      df_powers <- data.frame(power = rep_powers)

      p <- ggplot(df_powers, aes(x = power)) +
        geom_histogram(aes(y = after_stat(density)), bins = 20, fill="skyblue", color="white") +
        geom_density(color = "blue", lwd = 1) +
        geom_vline(xintercept = mean(rep_powers), color = "darkred", linetype = "dashed", size = 1) +
        annotate("text",
                 x = mean(rep_powers),
                 y = max(density(rep_powers)$y) * 0.9,
                 label = paste("Mean:", round(mean(rep_powers), 3)),
                 color = "darkred", vjust = -5, hjust = -0.1
        ) +
        labs(
          title = "Distribution of Empirical Power Estimates",
          x = "Estimated Power",
          y = "Density"
        ) +
        theme_minimal()
    } else {
      df_powers <- data.frame(power = rep_powers)
      p <- ggplot(df_powers, aes(x = power)) +
        geom_histogram(bins=5, fill="skyblue", color="white") +
        geom_vline(xintercept = mean(rep_powers), color = "darkred", linetype = "dashed", size = 1) +
        labs(
          title = "Empirical Power (single value)",
          x = "Estimated Power",
          y = "Density"
        ) +
        theme_minimal()
    }
    p
  }

  list(mean_power = mean(rep_powers),
       rep_powers = rep_powers,
       plot_powers = if(plot) p else NULL)
}
