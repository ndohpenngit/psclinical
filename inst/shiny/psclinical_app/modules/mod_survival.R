# ===============================================================
# Module: mod_survival.R
# Purpose: UI + Server for survival sample size / power computation
# Uses shinydashboard boxes for inputs and results
# ===============================================================
mod_survival_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Survival Sample Size and Power Calculation"),
    fluidRow(
      box(title = "Inputs", status = "primary", solidHeader = TRUE, width = 6,
          numericInput(ns("hr"), "Hazard Ratio (Treatment vs Control)", value = 0.7, min = 0.1, max = 2, step = 0.01),
          numericInput(ns("sig.level"), "Significance Level (alpha)", value = 0.05, min = 0.0001, max = 0.5, step = 0.01),
          numericInput(ns("accrual"), "Accrual Duration (years)", value = 2, min = 0.1, step = 0.1),
          numericInput(ns("followup"), "Follow-up Duration (years)", value = 1, min = 0.1, step = 0.1),
          numericInput(ns("dropout"), "Annual Dropout Rate", value = 0, min = 0, max = 1, step = 0.01),
          numericInput(ns("allocation"), "Proportion Allocated to Treatment", value = 0.5, min = 0.1, max = 0.9, step = 0.05),
          radioButtons(ns("mode"), "Computation Mode",
                       choices = c("Compute Sample Size" = "ss",
                                   "Compute Power" = "pw",
                                   "Both" = "both"),
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

      box(title = "Results", status = "success", solidHeader = TRUE, width = 6,
          verbatimTextOutput(ns("result")),
          plotOutput(ns("powerPlot"), height = "300px")
      )
    )
  )
}

mod_survival_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Reactive computation triggered by button
    results <- eventReactive(input$go, {
      mode <- input$mode
      hr <- input$hr
      sig.level <- input$sig.level
      accrual <- input$accrual
      followup <- input$followup
      dropout <- input$dropout
      allocation <- input$allocation
      avg_event_prob <- input$avg_event_prob

      # Call ps_survival depending on mode
      if (mode == "ss") {
        ps_survival(
          hr = hr,
          power = input$power,
          sig.level = sig.level,
          accrual = accrual,
          followup = followup,
          dropout = dropout,
          allocation = allocation,
          avg_event_prob = avg_event_prob
        )
      } else if (mode == "pw") {
        ps_survival(
          hr = hr,
          sig.level = sig.level,
          accrual = accrual,
          followup = followup,
          dropout = dropout,
          allocation = allocation,
          events = input$events
        )
      } else { # both
        ps_survival(
          hr = hr,
          power = input$power,
          sig.level = sig.level,
          accrual = accrual,
          followup = followup,
          dropout = dropout,
          allocation = allocation,
          events = input$events,
          avg_event_prob = avg_event_prob
        )
      }
    })

    # Render textual results
    output$result <- renderPrint({
      req(results())
      print(results())
    })

    # Optional: plot events vs HR when in sample size mode
    output$powerPlot <- renderPlot({
      req(input$mode == "ss")
      hr_vals <- seq(0.4, 1.0, by = 0.05)
      pw_vals <- sapply(hr_vals, function(h) {
        e <- ps_survival(hr = h, power = input$power,
                         sig.level = input$sig.level,
                         accrual = input$accrual,
                         followup = input$followup,
                         dropout = input$dropout,
                         allocation = input$allocation,
                         avg_event_prob = input$avg_event_prob)
        e$events_required
      })
      plot(hr_vals, pw_vals, type = "b", pch = 19, col = "blue",
           xlab = "Hazard Ratio", ylab = "Required Events",
           main = "Required Events vs Hazard Ratio")
    })

  })
}
