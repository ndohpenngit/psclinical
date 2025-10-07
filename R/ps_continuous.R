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
#' ps_continuous_parallel(delta=2, sd=5, power=0.8)
#' ps_continuous_parallel(delta=2, sd=5, n1=50, n2=50)
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
  alpha <- ifelse(type=="superiority" & alternative=="two.sided", sig.level/2, sig.level)
  z_alpha <- qnorm(1-alpha)

  compute_effective_for_sample <- function(type, delta, margin) {
    if(type=="superiority"){return(list(delta_eff=abs(delta)))}
    if(type=="noninferiority"){return(list(delta_eff=delta+margin))}
    if(type=="equivalence"){return(list(delta_eff=margin-abs(delta)))}
  }

  compute_n_for_power <- function(power){
    z_beta <- qnorm(power)
    eff <- compute_effective_for_sample(type, delta, margin)
    n2 <- ceiling(((z_alpha+z_beta)^2*sd^2*(1+1/r))/(eff$delta_eff^2))
    n1 <- ceiling(r*n2)
    list(n1=n1, n2=n2, total=n1+n2)
  }

  compute_power_for_n <- function(n1,n2){
    se <- sd*sqrt(1/n1+1/n2)
    if(type=="superiority"){z_beta <- abs(delta)/se - z_alpha; return(pnorm(z_beta))}
    if(type=="noninferiority"){z_beta <- (delta+margin)/se - z_alpha; return(pnorm(z_beta))}
    if(type=="equivalence"){upper <- (margin-z_alpha*se-delta)/se; lower <- (-margin+z_alpha*se-delta)/se; return(pnorm(upper)-pnorm(lower))}
  }

  if(!is.null(power)&is.null(n1)&is.null(n2)){out <- compute_n_for_power(power)}
  else if(is.null(power)&!is.null(n1)&!is.null(n2)){out <- list(power=compute_power_for_n(n1,n2), n1=n1, n2=n2, total=n1+n2)}
  else if(!is.null(power)&!is.null(n1)&!is.null(n2)){
    nreq <- compute_n_for_power(power)
    pw <- compute_power_for_n(n1,n2)
    out <- c(nreq, list(achieved_power=pw))
  } else stop("Provide power OR n1/n2 or both")
  class(out) <- "ssize_result"
  out
}
