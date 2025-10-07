#' Simulate empirical power
#'
#' Simulates parallel or survival trials to empirically estimate power under complex settings.
#'
#' @param design "continuous", "binary", or "survival".
#' @param nsim Number of simulations.
#' @param n Sample size per arm.
#' @param delta Mean difference for continuous outcome.
#' @param sd Standard deviation for continuous outcome.
#' @param p1,p2 Probabilities for binary outcome.
#' @param HR Hazard ratio for survival.
#' @param lambdaC Baseline hazard in control group for survival.
#' @param accrual,followup,dropout For survival.
#' @param alpha Significance level.
#' @param alternative "two.sided" or "one.sided".
#'
#' @return Empirical power (proportion of significant tests).
#' @export
#'
#' @examples
#' sim_empirical_power("continuous", nsim=100, n=20, delta=1, sd=2)
sim_empirical_power <- function(design=c("continuous","binary","survival"),
                                nsim=1000, n=50, delta=NULL, sd=NULL,
                                p1=NULL, p2=NULL, HR=NULL, lambdaC=NULL,
                                accrual=NULL, followup=NULL, dropout=0,
                                alpha=0.05, alternative=c("two.sided","one.sided")){
  design <- match.arg(design)
  alternative <- match.arg(alternative)
  results <- logical(nsim)

  if(design=="continuous"){
    for(i in 1:nsim){
      g1 <- rnorm(n,0,sd)
      g2 <- rnorm(n,delta,sd)
      p <- t.test(g1,g2,var.equal=TRUE)$p.value
      results[i] <- if(alternative=="two.sided") p<alpha else p/2<alpha
    }
  }

  if(design=="binary"){
    for(i in 1:nsim){
      x1 <- rbinom(1,n,p1)
      x2 <- rbinom(1,n,p2)
      p <- prop.test(c(x1,x2),c(n,n))$p.value
      results[i] <- if(alternative=="two.sided") p<alpha else p/2<alpha
    }
  }

  if(design=="survival"){
    for(i in 1:nsim){
      n_total <- 2*n
      group <- rep(c(0,1),each=n)
      lambdaT <- lambdaC*HR
      time <- rexp(n_total, rate=ifelse(group==0,lambdaC,lambdaT))
      status <- rbinom(n_total,1,1-dropout)
      df <- data.frame(time=time,status=status,group=factor(group))
      fit <- survival::coxph(survival::Surv(time,status)~group,data=df)
      p <- summary(fit)$coefficients[,"Pr(>|z|)"]
      results[i] <- if(alternative=="two.sided") p<alpha else p/2<alpha
    }
  }
  mean(results)
}
