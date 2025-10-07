# ===============================================================
# Module: mod_simulation.R
# Purpose: UI + Server for empirical power simulation
# Depends on: sim_empirical_power() from package psclinical
# ===============================================================

#' Simulation Module UI
#' @param id Module ID
#' @noRd
mod_simulation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Empirical Power Simulation"),
    fluidRow(
      # Input Box
      box(title = "Inputs", status = "primary", solidHeader = TRUE, width = 6,
          selectInput(ns("design"), "Design Type",
                      choices = c("Continuous"="continuous",
                                  "Binary"="binary",
                                  "Survival"="survival")),
          numericInput(ns("nsim"), "Number of Simulations", value = 1000, min = 1, step = 1),
          numericInput(ns("n"), "Sample Size per Group", value = 50, min = 1, step = 1),
          numericInput(ns("alpha"), "Significance Level (alpha)", value = 0.05, min = 0.001, max = 0.5, step = 0.01),
          selectInput(ns("alternative"), "Alternative Hypothesis", choices=c("two.sided","one.sided")),

          # Continuous Inputs
          conditionalPanel(
            condition = paste0("input['", ns("design"), "'] == 'continuous'"),
            numericInput(ns("delta"), "Mean Difference", value = 1),
            numericInput(ns("sd"), "Standard Deviation", value = 2)
          ),

          # Binary Inputs
          conditionalPanel(
            condition = paste0("input['", ns("design"), "'] == 'binary'"),
            numericInput(ns("p1"), "Probability (Control)", value = 0.3, min=0, max=1),
            numericInput(ns("p2"), "Probability (Treatment)", value = 0.5, min=0, max=1)
          ),

          # Survival Inputs
          conditionalPanel(
            condition = paste0("input['", ns("design"), "'] == 'survival'"),
            numericInput(ns("HR"), "Hazard Ratio (Treatment vs Control)", value = 0.7, min=0.01, max=2, step=0.01),
            numericInput(ns("lambdaC"), "Baseline Hazard (Control)", value = 0.1, min=0.001),
            numericInput(ns("accrual"), "Accrual Duration (years)", value = 2),
            numericInput(ns("followup"), "Follow-up Duration (years)", value = 1),
            numericInput(ns("dropout"), "Annual Dropout Rate", value = 0, min=0, max=1, step=0.01)
          ),
          actionButton(ns("go"), "Compute Empirical Power", class="btn-primary")
      ),

      # Results Box
      box(title = "Results", status = "success", solidHeader = TRUE, width = 6,
          verbatimTextOutput(ns("result")),
          plotOutput(ns("simPlot"), height="300px")
      )
    )
  )
}


#' Simulation Module Server
#' @param id Module ID
#' @noRd
mod_simulation_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Reactive computation triggered by button
    sim_result <- eventReactive(input$go, {
      design <- input$design

      if (design == "continuous") {
        sim_empirical_power(
          design = "continuous",
          nsim = input$nsim,
          n = input$n,
          delta = input$delta,
          sd = input$sd,
          alpha = input$alpha,
          alternative = input$alternative
        )
      } else if (design == "binary") {
        sim_empirical_power(
          design = "binary",
          nsim = input$nsim,
          n = input$n,
          p1 = input$p1,
          p2 = input$p2,
          alpha = input$alpha,
          alternative = input$alternative
        )
      } else { # survival
        sim_empirical_power(
          design = "survival",
          nsim = input$nsim,
          n = input$n,
          HR = input$HR,
          lambdaC = input$lambdaC,
          accrual = input$accrual,
          followup = input$followup,
          dropout = input$dropout,
          alpha = input$alpha,
          alternative = input$alternative
        )
      }
    })

    # Render textual result
    output$result <- renderPrint({
      req(sim_result())
      cat("Empirical Power (proportion of significant tests):\n")
      cat(round(sim_result(),3), "\n")
    })

    # Histogram of simulated results for continuous/binary
    output$simPlot <- renderPlot({
      req(input$go)
      design <- input$design
      if (design %in% c("continuous","binary")) {
        nrep <- 100
        sims <- replicate(nrep, {
          sim_empirical_power(design=design, nsim=input$nsim, n=input$n,
                              delta=input$delta, sd=input$sd,
                              p1=input$p1, p2=input$p2,
                              alpha=input$alpha, alternative=input$alternative)
        })
        hist(sims, main="Distribution of Empirical Power", xlab="Power", col="skyblue", border="white")
      }
    })

  })
}
