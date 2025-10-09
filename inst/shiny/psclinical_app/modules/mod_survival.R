mod_survival_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Binary Outcome: Two-Sample Parallel Design"),

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

      box(title = "Results", status = NULL, solidHeader = TRUE, width = 6,
          uiOutput(ns("error_msg")),
          verbatimTextOutput(ns("result")),
          hr(),
          plotlyOutput(ns("powerPlot"), height = "300px") %>% withSpinner(type = 8)
      )
    ),

    # Instructions
    fluidRow(
      box(title = "Instructions", status = NULL, solidHeader = FALSE, width = 12, collapsible = TRUE, collapsed = TRUE,
          p("This module calculates sample size or achieved power for survival trials."),
          tags$ul(
            tags$li("Specify the hazard ratio, accrual and follow-up duration, dropout rate, and allocation."),
            tags$li("Select the computation mode: sample size, power, or both."),
            tags$li("Provide either desired power (for sample size) or number of events (for power)."),
            tags$li("Click 'Compute' to view results and plots.")
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

      # --- Validation ---
      validate_inputs <- function() {
        if (is.null(input$hr) || input$hr <= 0) return("Hazard ratio must be > 0")
        if (is.null(input$sig.level) || input$sig.level <= 0 || input$sig.level >= 1)
          return("Significance level must be between 0 and 1")
        if (input$accrual <= 0 || input$followup <= 0) return("Accrual and follow-up must be > 0")
        if (input$dropout < 0 || input$dropout >= 1) return("Dropout rate must be between 0 and 1")
        if (input$allocation <=0 || input$allocation >=1) return("Allocation must be between 0 and 1")
        if (input$mode %in% c("ss","both") && (is.null(input$power) || input$power <=0 || input$power >=1))
          return("Provide valid desired power")
        if (input$mode %in% c("pw","both") && (is.null(input$events) || input$events <=0))
          return("Provide valid number of events")
        return(NULL)
      }

      err <- validate_inputs()
      if (!is.null(err)) {
        output$error_msg <- renderUI(div(style="color:red; font-weight:bold;", err))
        return()
      }

      # --- Compute results safely ---
      res <- tryCatch({
        ps_survival(
          hr = input$hr,
          power = if(input$mode %in% c("ss","both")) input$power else NULL,
          sig.level = input$sig.level,
          accrual = input$accrual,
          followup = input$followup,
          dropout = input$dropout,
          allocation = input$allocation,
          events = if(input$mode %in% c("pw","both")) input$events else NULL,
          avg_event_prob = input$avg_event_prob
        )
      }, error=function(e){
        output$error_msg <- renderUI(div(style="color:red; font-weight:bold;", e$message))
        return(NULL)
      })

      if (is.null(res)) return()

      # --- Render textual results ---
      output$result <- renderPrint({
        cat("=== Survival Outcome ===\n\n")
        if(res$mode=="sample size") {
          cat("Computation Mode: Sample Size determination\n")
          cat("Requested Power:", res$requested_power, "\n")
          cat("Required Events:", res$events_required, "\n")
          cat("Approx. Total N:", res$approx_N, "\n")
        } else if(res$mode=="power") {
          cat("Computation Mode: Power estimation\n")
          cat("Number of Events:", res$events, "\n")
          cat("Achieved Power:", round(res$power,4), "\n")
        } else if(res$mode=="both") {
          cat("Computation Mode: Both Sample Size & Power\n")
          cat("Requested Power:", res$requested_power, "\n")
          cat("Required Events:", res$events_required, "\n")
          cat("Approx. Total N:", res$approx_N, "\n")
          cat("Achieved Power:", round(res$achieved_power,4), "\n")
        }
      })

      # --- Render ggplotly plot ---
      output$powerPlot <- renderPlotly({
        try({
          library(ggplot2)
          library(plotly)

          if(res$mode %in% c("sample size","both")) {
            hr_vals <- seq(0.4, 1.0, by = 0.05)
            ev_vals <- sapply(hr_vals, function(h) {
              ps_survival(
                hr = h,
                power = res$requested_power,
                sig.level = input$sig.level,
                accrual = input$accrual,
                followup = input$followup,
                dropout = input$dropout,
                allocation = input$allocation,
                avg_event_prob = input$avg_event_prob
              )$events_required
            })
            df <- data.frame(HR = hr_vals, Events = ev_vals)
            p <- ggplot(df, aes(x=HR, y=Events)) +
              geom_point(color="skyblue") +
              geom_line(color="skyblue") +
              labs(title="Required Events vs Hazard Ratio",
                   x="Hazard Ratio", y="Required Events") +
              theme_minimal()
            ggplotly(p)

          } else if(res$mode=="power") {
            ev_vals <- seq(50, 500, by=10)
            pw_vals <- sapply(ev_vals, function(ev){
              ps_survival(
                hr=input$hr,
                sig.level=input$sig.level,
                accrual=input$accrual,
                followup=input$followup,
                dropout=input$dropout,
                allocation=input$allocation,
                events=ev
              )$power
            })
            df <- data.frame(Events = ev_vals, Power = pw_vals)
            p <- ggplot(df, aes(x=Events, y=Power)) +
              geom_line(color="skyblue", size=1) +
              geom_point(color="skyblue") +
              labs(title="Achieved Power vs Number of Events",
                   x="Number of Events", y="Achieved Power") +
              theme_minimal() +
              ylim(0,1)
            ggplotly(p)
          }
        })
      })
    })
  })
}
