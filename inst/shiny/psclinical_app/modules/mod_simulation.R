mod_simulation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Empirical Power Simulation"),
    fluidRow(
      box(title = "Inputs", status = "primary", solidHeader = TRUE, width = 6,
          selectInput(ns("design"), "Design Type",
                      choices = c("Continuous"="continuous",
                                  "Binary"="binary",
                                  "Survival"="survival")),
          numericInput(ns("nrep"), "Number of Repetitions (outer loop)", value = 100, min = 1, step = 1),
          numericInput(ns("nsim"), "Simulations per Rep (inner loop)", value = 1000, min = 1, step = 1),
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

      # Results
      box(title = "Results", status = NULL, solidHeader = TRUE, width = 6,
          verbatimTextOutput(ns("result")),
          hr(),
          plotly::plotlyOutput(ns("simPlot"), height="300px") %>% withSpinner(type = 8)
      )
    )
  )
}

mod_simulation_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    sim_result <- eventReactive(input$go, {
      sim_empirical_power(
        design = input$design,
        nsim = input$nsim,
        n = input$n,
        delta = input$delta,
        sd = input$sd,
        p1 = input$p1,
        p2 = input$p2,
        HR = input$HR,
        lambdaC = input$lambdaC,
        accrual = input$accrual,
        followup = input$followup,
        dropout = input$dropout,
        alpha = input$alpha,
        alternative = input$alternative,
        nrep = input$nrep,
        plot = FALSE
      )
    })

    # --- Summary Output ---
    output$result <- renderPrint({
      req(sim_result())
      res <- sim_result()
      powers <- res$rep_powers
      mean_power <- res$mean_power
      sd_power <- sd(powers)
      nrep <- length(powers)

      if (nrep > 1) {
        se_power <- sd_power / sqrt(nrep)
        ci_lower <- mean_power - 1.96 * se_power
        ci_upper <- mean_power + 1.96 * se_power
      } else {
        se_power <- NA
        ci_lower <- NA
        ci_upper <- NA
      }

      cat("Summary of Empirical Power Estimates:\n")
      print(summary(powers))
      cat("\nMean Empirical Power:", round(mean_power, 3))
      cat("\nMonte Carlo SE:", round(se_power, 4))
      cat("\n95% Monte Carlo CI: [", round(ci_lower, 3), ", ", round(ci_upper, 3), "]\n")
    })

    # --- Plot ---
    output$simPlot <- renderPlotly({
      req(sim_result())
      res <- sim_result()
      powers <- res$rep_powers
      df <- data.frame(power = powers)

      if (length(unique(powers)) > 1) {
        p <- ggplot(df, aes(x = power)) +
          geom_histogram(aes(y = after_stat(density)), bins = 20,
                         fill = "skyblue", color = "white") +
          geom_density(color = "lightblue", linewidth = 1) +
          geom_vline(aes(xintercept = mean(power)),
                     color = "darkred", linetype = "dashed", linewidth = 1) +
          annotate("text", x = mean(powers),
                   y = max(density(powers)$y) * 0.9,
                   label = paste0("Mean = ", round(mean(powers), 3)),
                   color = "darkred", hjust = -0.1) +
          labs(title = "Distribution of Empirical Power Estimates",
               x = "Empirical Power", y = "Density") +
          theme_minimal()
      } else {
        # Fallback: single-value histogram
        p <- ggplot(df, aes(x = power)) +
          geom_histogram(bins = 5, fill = "skyblue", color = "white") +
          geom_vline(aes(xintercept = mean(power)),
                     color = "darkred", linetype = "dashed", linewidth = 1) +
          labs(title = "Empirical Power (Single Value)",
               x = "Empirical Power", y = "Count") +
          theme_minimal()
      }

      ggplotly(p)
    })
  })
}
