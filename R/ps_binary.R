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
#' ps_binary_parallel(p1=0.3, p2=0.2, power=0.8)
ps_binary_parallel <- function(
    p1,
    p2,
    type=c("superiority","noninferiority","equivalence"),
    margin=NULL,
    power=NULL,
    n1=NULL,
    n2=NULL,
    sig.level=0.05,
    ratio=1,
    alternative=c("two.sided","one.sided")
) {
  type <- match.arg(type)
  alternative <- match.arg(alternative)
  r <- ratio
  var1 <- p1*(1-p1); var2 <- p2*(1-p2)
  alpha <- ifelse(type=="superiority"&alternative=="two.sided", sig.level/2, sig.level)
  z_alpha <- qnorm(1-alpha)

  compute_effective_for_sample <- function(type,p1,p2,margin){
    diff <- p1-p2
    if(type=="superiority") return(list(diff_eff=abs(diff)))
    if(type=="noninferiority") return(list(diff_eff=diff+margin))
    if(type=="equivalence") return(list(diff_eff=margin-abs(diff)))
  }

  compute_n_for_power <- function(power){
    z_beta <- qnorm(power)
    eff <- compute_effective_for_sample(type,p1,p2,margin)
    n2 <- ceiling(((z_alpha+z_beta)^2*(var1+var2/r))/(eff$diff_eff^2))
    n1 <- ceiling(r*n2)
    list(n1=n1, n2=n2, total=n1+n2)
  }

  compute_power_for_n <- function(n1,n2){
    se <- sqrt(var1/n1+var2/n2); diff <- p1-p2
    if(type=="superiority"){z_beta <- abs(diff)/se - z_alpha; return(pnorm(z_beta))}
    if(type=="noninferiority"){z_beta <- (diff+margin)/se - z_alpha; return(pnorm(z_beta))}
    if(type=="equivalence"){upper <- (margin-z_alpha*se-diff)/se; lower <- (-margin+z_alpha*se-diff)/se; return(pnorm(upper)-pnorm(lower))}
  }

  if(!is.null(power)&is.null(n1)&is.null(n2)) out <- compute_n_for_power(power)
  else if(is.null(power)&!is.null(n1)&!is.null(n2)) out <- list(power=compute_power_for_n(n1,n2), n1=n1, n2=n2, total=n1+n2)
  else if(!is.null(power)&!is.null(n1)&!is.null(n2)){nreq<-compute_n_for_power(power); pw<-compute_power_for_n(n1,n2); out<-c(nreq,list(achieved_power=pw))}
  else stop("Provide power OR n1/n2 or both")
  class(out)<-"ssize_result"
  out
}
