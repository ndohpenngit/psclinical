#' Survival sample size computation
#'
#' Computes required number of events or total sample size for survival trials,
#' accounting for accrual, follow-up, and dropout rates.
#'
#' @param hr Hazard ratio (treatment vs control).
#' @param power Desired power (0–1). Provide either `power` or `events`.
#' @param sig.level Significance level (default 0.05, two-sided).
#' @param accrual Accrual duration in years.
#' @param followup Additional follow-up duration in years.
#' @param dropout Annual dropout rate (0–1). Default 0.
#' @param allocation Allocation proportion to treatment (default 0.5).
#' @param events Optional: fixed number of events to compute achieved power.
#' @param avg_event_prob Optional: approximate event probability for total N.
#'
#' @return A list of class \code{ssize_result} with events required, total N, or achieved power.
#' @export
#'
#' @examples
#' ps_survival(hr=0.7, power=0.8, accrual=2, followup=1)
ps_survival <- function(
    hr,
    power=NULL,
    sig.level=0.05,
    accrual,
    followup,
    dropout=0,
    allocation=0.5,
    events=NULL,
    avg_event_prob=NULL
) {
  alpha <- sig.level/2
  z_alpha <- qnorm(1-alpha)
  loghr <- log(hr)
  p <- allocation
  k <- p*(1-p)

  compute_events <- function(power){z_beta<-qnorm(power); ((z_alpha+z_beta)^2)/((loghr)^2*k)}
  compute_power <- function(events){z_beta<-sqrt(events*(loghr)^2*k)-z_alpha; pnorm(z_beta)}

  if(!is.null(power)&is.null(events)){
    e <- compute_events(power)
    approx_N <- if(!is.null(avg_event_prob)) ceiling(e/avg_event_prob) else NA
    out <- list(mode="sample size", requested_power=power, events_required=ceiling(e),
                approx_N=approx_N, assumptions=list(accrual=accrual, followup=followup, dropout=dropout))
  } else if(is.null(power)&!is.null(events)){
    pw <- compute_power(events)
    out <- list(mode="power", power=pw, events=events,
                assumptions=list(accrual=accrual, followup=followup, dropout=dropout))
  } else {
    e <- compute_events(power)
    pw <- compute_power(events)
    approx_N <- if(!is.null(avg_event_prob)) ceiling(e/avg_event_prob) else NA
    out <- list(mode="both", requested_power=power, achieved_power=pw,
                events_required=ceiling(e), approx_N=approx_N,
                assumptions=list(accrual=accrual, followup=followup, dropout=dropout))
  }
  class(out) <- "ssize_result"
  out
}
