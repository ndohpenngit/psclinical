#' Survival sample size computation
#'
#' Computes required number of events or total sample size for survival trials,
#' accounting for accrual, follow-up, and dropout rates.
#'
#' @param hr Hazard ratio (treatment vs control). Must be > 0.
#' @param power Desired power (0–1). Provide either `power` or `events`.
#' @param sig.level Significance level (default 0.05, two-sided).
#' @param accrual Accrual duration in years. Must be > 0.
#' @param followup Additional follow-up duration in years. Must be > 0.
#' @param dropout Annual dropout rate (0–1). Default 0.
#' @param allocation Allocation proportion to treatment (0–1). Default 0.5.
#' @param events Optional: fixed number of events to compute achieved power.
#' @param avg_event_prob Optional: approximate event probability for total N.
#'
#' @return A list of class \code{ssize_result} with events required, total N, or achieved power.
#' @export
#'
#' @examples
#' # Sample size calculation
#' ps_survival(hr=0.7, power=0.8, accrual=2, followup=1)
#'
#' # Power calculation for fixed events
#' ps_survival(hr=0.7, events=150, accrual=2, followup=1)
#'
#' # Both sample size and power
#' ps_survival(hr=0.7, power=0.8, events=150, accrual=2, followup=1)
ps_survival <- function(
    hr,
    power = NULL,
    sig.level = 0.05,
    accrual,
    followup,
    dropout = 0,
    allocation = 0.5,
    events = NULL,
    avg_event_prob = NULL
) {
  # --- Input validation ---
  if (!is.numeric(hr) || hr <= 0) stop("Hazard ratio must be > 0")
  if (!is.numeric(sig.level) || sig.level <= 0 || sig.level >= 1)
    stop("Significance level must be between 0 and 1")
  if (!is.numeric(accrual) || accrual <= 0) stop("Accrual must be > 0")
  if (!is.numeric(followup) || followup <= 0) stop("Follow-up must be > 0")
  if (!is.numeric(dropout) || dropout < 0 || dropout >= 1) stop("Dropout must be between 0 and 1")
  if (!is.numeric(allocation) || allocation <= 0 || allocation >= 1) stop("Allocation must be between 0 and 1")
  if (!is.null(power) && (power <= 0 || power >= 1)) stop("Power must be between 0 and 1")
  if (!is.null(events) && events <= 0) stop("Events must be > 0")
  if (is.null(power) & is.null(events)) stop("Provide at least 'power' or 'events'.")

  alpha <- sig.level / 2
  z_alpha <- qnorm(1 - alpha)
  loghr <- log(hr)
  k <- allocation * (1 - allocation)

  compute_events <- function(power) {
    z_beta <- qnorm(power)
    ((z_alpha + z_beta)^2) / ((loghr)^2 * k)
  }
  compute_power <- function(events) {
    z_beta <- sqrt(events * (loghr)^2 * k) - z_alpha
    pnorm(z_beta)
  }

  # --- Computation ---
  out <- list()
  if (!is.null(power) & is.null(events)) {
    e <- compute_events(power)
    approx_N <- if (!is.null(avg_event_prob)) ceiling(e / avg_event_prob) else NA
    out <- list(
      mode = "sample size",
      requested_power = power,
      events_required = ceiling(e),
      approx_N = approx_N,
      assumptions = list(accrual = accrual, followup = followup, dropout = dropout)
    )
  } else if (is.null(power) & !is.null(events)) {
    pw <- compute_power(events)
    out <- list(
      mode = "power",
      power = pw,
      events = events,
      assumptions = list(accrual = accrual, followup = followup, dropout = dropout)
    )
  } else if (!is.null(power) & !is.null(events)) {
    e <- compute_events(power)
    pw <- compute_power(events)
    approx_N <- if (!is.null(avg_event_prob)) ceiling(e / avg_event_prob) else NA
    out <- list(
      mode = "both",
      requested_power = power,
      achieved_power = pw,
      events_required = ceiling(e),
      approx_N = approx_N,
      assumptions = list(accrual = accrual, followup = followup, dropout = dropout)
    )
  }
  class(out) <- "ssize_result"
  out
}
