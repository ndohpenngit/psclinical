mod_survival_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Time-to-Event (Survival) Power / Sample Size Calculator"),
    fluidRow(
      box(title = "Inputs",
          status = "primary", solidHeader = TRUE, width = 6,
          numericInput(ns("hr"), "Hazard Ratio (Treatment vs Control)", value = 0.7, min = 0.1, max = 2, step = 0.01),
          numericInput(ns("sig.level"), "Significance Level (α)", value = 0.05, min = 0.0001, max = 0.5, step = 0.01),
          numericInput(ns("accrual"), "Accrual Duration (years)", value = 2, min = 0.1, step = 0.1),
          numericInput(ns("followup"), "Follow-up Duration (years)", value = 1, min = 0.1, step = 0.1),
          numericInput(ns("dropout"), "Annual Dropout Rate", value = 0, min = 0, max = 1, step = 0.01),
          numericInput(ns("allocation"), "Proportion Allocated to Treatment", value = 0.5, min = 0.1, max = 0.9, step = 0.05),

          radioButtons(ns("mode"), "Computation Mode",
                       choices = c("Compute Sample Size" = "ss",
                                   "Compute Power" = "pw",
                                   "Compute Both" = "both"),
                       selected = "ss"),

          conditionalPanel(
            condition = paste0("input['", ns("mode"), "'] == 'ss' || input['", ns("mode"), "'] == 'both'"),
            numericInput(ns("power"), "Desired Power", value = 0.8, min = 0.01, max = 0.999, step = 0.01)
          ),
          conditionalPanel(
            condition = paste0("input['", ns("mode"), "'] == 'pw' || input['", ns("mode"), "'] == 'both'"),
            numericInput(ns("events"), "Fixed Number of Events", value = 200, min = 1, step = 1)
          ),
          numericInput(ns("avg_event_prob"), "Approx. Event Probability (for N estimation)", value = 0.6, min = 0.01, max = 1, step = 0.01),
          actionButton(ns("go"), "Compute", class = "btn-primary")
      ),

      conditionalPanel(
        condition = paste0("input['", ns("go"), "'] > 0"),
        box(title = "Results", status = NULL, solidHeader = TRUE, width = 6, collapsible = TRUE,
            uiOutput(ns("error_msg")),
            verbatimTextOutput(ns("result")),
            hr(),
            plotlyOutput(ns("powerPlot"), height = "300px") %>% withSpinner(type = 8)
        )
      )
    ),

    # Instructions
    fluidRow(
      box(title = "Instructions", status = NULL, solidHeader = FALSE, width = 12,
          collapsible = TRUE, collapsed = TRUE,
          h4("How to Use the Survival Module"),
          tags$ul(
            tags$li("Specify the hazard ratio (treatment/control), significance level, accrual and follow-up durations."),
            tags$li("If applicable, include annual dropout rate and allocation ratio."),
            tags$li("Select the computation mode:"),
            tags$ul(
              tags$li(strong("Compute Sample Size: "), "Given desired power, calculate the required number of events and approximate total sample size."),
              tags$li(strong("Compute Power: "), "Given the number of events, estimate the achieved power."),
              tags$li(strong("Compute Both: "), "Compute both sample size and achieved power for comparison.")
            ),
            tags$li("Enter approximate event probability for total sample size estimation."),
            tags$li("Click 'Compute' to generate results and interactive plots showing how power or required events vary with key parameters.")
          )
      )
    )
  )
}

mod_survival_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$error_msg <- renderUI(NULL)
    output$result <- renderPrint(NULL)
    output$powerPlot <- renderPlotly(NULL)

    observeEvent(input$go, {

      output$error_msg <- renderUI(NULL)
      output$result <- renderPrint(NULL)
      output$powerPlot <- renderPlotly(NULL)

      # local vars ---
      hr <- input$hr
      sig.level <- input$sig.level
      accrual <- input$accrual
      followup <- input$followup
      dropout <- input$dropout
      allocation <- input$allocation
      mode <- input$mode
      power_req <- input$power
      events_fixed <- input$events
      avg_event_prob <- input$avg_event_prob

      # Validation ---
      validate_inputs <- function() {
        if (is.null(hr) || !is.numeric(hr) || hr <= 0) return("Hazard ratio must be > 0")
        if (is.null(sig.level) || !is.numeric(sig.level) || sig.level <= 0 || sig.level >= 1)
          return("Significance level must be between 0 and 1")
        if (!is.numeric(accrual) || accrual <= 0) return("Accrual must be > 0")
        if (!is.numeric(followup) || followup <= 0) return("Follow-up must be > 0")
        if (!is.numeric(dropout) || dropout < 0 || dropout >= 1) return("Dropout rate must be between 0 and 1")
        if (!is.numeric(allocation) || allocation <= 0 || allocation >= 1) return("Allocation must be between 0 and 1")
        if (mode %in% c("ss", "both") && (is.null(power_req) || power_req <= 0 || power_req >= 1)) return("Provide valid desired power")
        if (mode %in% c("pw", "both") && (is.null(events_fixed) || events_fixed <= 0)) return("Provide valid number of events")
        if (!is.numeric(avg_event_prob) || avg_event_prob <= 0 || avg_event_prob > 1) return("Provide valid approx. event probability (0 < p <= 1)")
        return(NULL)
      }

      err <- validate_inputs()
      if (!is.null(err)) {
        output$error_msg <- renderUI(div(style = "color:red; font-weight:bold;", err))
        return()
      }

      # Compute results ---
      res <- tryCatch({
        ps_survival(
          hr = hr,
          power = if (mode %in% c("ss","both")) power_req else NULL,
          sig.level = sig.level,
          accrual = accrual,
          followup = followup,
          dropout = dropout,
          allocation = allocation,
          events = if (mode %in% c("pw","both")) events_fixed else NULL,
          avg_event_prob = avg_event_prob
        )
      }, error = function(e) {
        output$error_msg <- renderUI(div(style = "color:red; font-weight:bold;", e$message))
        return(NULL)
      })

      if (is.null(res)) return()

      # results ---
      output$result <- renderPrint({
        cat("=== Time-to-Event (Survival) Outcome ===\n\n")
        cat("Design Type: Two-Sample Parallel\n")
        cat("Significance Level (α):", sig.level, "\n")
        cat("Hazard Ratio (HR):", hr, "\n")
        cat("Accrual (years):", accrual, " | Follow-up (years):", followup, "\n")
        cat("Dropout (annual):", dropout, " | Allocation (treatment):", allocation, "\n\n")

        if (res$mode == "sample size") {
          cat("Computation Mode: Sample Size determination\n")
          cat("Requested Power:", res$requested_power, "\n")
          cat("Required Events:", res$events_required, "\n")
          if (!is.null(res$approx_N)) cat("Approx. Total N:", res$approx_N, "\n")
          if (!is.null(res$achieved_power)) cat("Achieved Power (check):", round(res$achieved_power, 4), "\n")
        } else if (res$mode == "power") {
          cat("Computation Mode: Power estimation\n")
          cat("Number of Events:", res$events, "\n")
          cat("Achieved Power:", round(res$power, 4), "\n")
        } else if (res$mode == "both") {
          cat("Computation Mode: Both Sample Size & Power\n")
          cat("Requested Power:", res$requested_power, "\n")
          cat("Required Events:", res$events_required, "\n")
          if (!is.null(res$approx_N)) cat("Approx. Total N:", res$approx_N, "\n")
          cat("Achieved Power:", round(res$achieved_power, 4), "\n")
        } else {
          print(res)
        }
      })

      # Render plot ---
      output$powerPlot <- renderPlotly({
        tryCatch({

          library(ggplot2)
          library(plotly)

          # 1) If we're in sample-size mode (or both), plot required events vs HR
          if (res$mode %in% c("sample size", "both")) {
            # adaptive HR range: half to 1.5x around chosen HR, limited to sensible bounds
            hr_low <- max(0.05, hr * 0.5)
            hr_high <- min(2, hr * 1.5)
            hr_vals <- seq(hr_low, hr_high, length.out = 13)

            ev_vals <- sapply(hr_vals, function(h) {
              tmp <- tryCatch(
                ps_survival(
                  hr = h,
                  power = res$requested_power,
                  sig.level = sig.level,
                  accrual = accrual,
                  followup = followup,
                  dropout = dropout,
                  allocation = allocation,
                  avg_event_prob = avg_event_prob
                )$events_required,
                error = function(e) NA
              )
              tmp
            })

            df <- data.frame(HR = hr_vals, Events = ev_vals)
            df <- df[!is.na(df$Events), ]
            if (nrow(df) == 0) return(NULL)

            p <- ggplot(df, aes(x = HR, y = Events)) +
              geom_line() + geom_point() +
              labs(title = "Required Events vs Hazard Ratio",
                   x = "Hazard Ratio (treatment / control)",
                   y = "Required Number of Events") +
              theme_minimal()

            ggplotly(p)

            # 2) If we're in power mode, plot achieved power vs number of events
          } else if (res$mode == "power") {
            ev_center <- events_fixed
            ev_low <- max(1, ev_center - 100)
            ev_high <- ev_center + 100
            if (ev_high <= ev_low) { ev_low <- 1; ev_high <- ev_center + 50 }
            ev_vals <- seq(ev_low, ev_high, by = round(max(1, (ev_high-ev_low)/40)))

            pw_vals <- sapply(ev_vals, function(ev) {
              tmp <- tryCatch(
                ps_survival(
                  hr = hr,
                  sig.level = sig.level,
                  accrual = accrual,
                  followup = followup,
                  dropout = dropout,
                  allocation = allocation,
                  events = ev
                )$power,
                error = function(e) NA
              )
              tmp
            })

            df <- data.frame(Events = ev_vals, Power = pw_vals)
            df <- df[!is.na(df$Power), ]
            if (nrow(df) == 0) return(NULL)

            p <- ggplot(df, aes(x = Events, y = Power)) +
              geom_line() + geom_point() +
              labs(title = "Achieved Power vs Number of Events",
                   x = "Number of Events",
                   y = "Achieved Power") +
              theme_minimal() +
              ylim(0, 1)

            ggplotly(p)
          } else {
            return(NULL)
          }
        }, error = function(e) {
          message("Plot generation failed: ", e$message)
          return(NULL)
        })
      })

    })
  })
}
