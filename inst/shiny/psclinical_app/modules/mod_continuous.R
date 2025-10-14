mod_continuous_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Continuous Outcome Power/Sample Size Calculator"),
    fluidRow(
      box(
        title = "Inputs", width = 6, solidHeader = TRUE, status = "primary", collapsible = TRUE,
        selectInput(ns("type"), "Design Type",
                    choices = c("Two-Sample Parallel" = "parallel",
                                "One-Sample" = "one_sample",
                                "Paired" = "paired"),
                    selected = "parallel"),
        numericInput(ns("delta"), "Mean Difference (Δ)", value = 2, step = 0.1),
        numericInput(ns("sd"), "Standard Deviation (σ)", value = 5, step = 0.1, min = 0),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'paired'", ns("type")),
          numericInput(ns("sd_diff"), "SD of Differences (σ_diff)", value = 3, step = 0.1)
        ),
        numericInput(ns("sig"), "Significance Level (α)", value = 0.05, min = 0, max = 1, step = 0.01),
        selectInput(ns("alternative"), "Alternative Hypothesis", choices = c("two.sided", "one.sided")),
        radioButtons(ns("mode"), "Computation Mode",
                     choices = c("Compute Sample Size" = "n", "Compute Power" = "power"),
                     selected = "n"),
        conditionalPanel(
          condition = paste0("input['", ns("mode"), "'] == 'n'"),
          numericInput(ns("power"), "Desired Power (1 - β)", value = 0.8, min = 0.01, max = 0.999, step = 0.01)
        ),
        conditionalPanel(
          condition = paste0("input['", ns("mode"), "'] == 'power'"),
          numericInput(ns("n"), "Sample Size (per group for parallel, total for one-sample/paired)", value = 50, min = 1, step = 1)
        ),
        actionButton(ns("go"), "Compute", icon = icon("calculator"), class = "btn-primary"),
        hr(), uiOutput(ns("error_msg"))
      ),

      # Results only appear after clicking "Compute"
      conditionalPanel(
        condition = paste0("input['", ns("go"), "'] > 0"),
        box(title = "Results", width = 6, solidHeader = TRUE, collapsible = TRUE,
            verbatimTextOutput(ns("result"))
        )
      )
    ),

    fluidRow(
      box(title = "Instructions", width = 12, solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE,
          p("This module calculates sample size or achieved power for continuous outcomes."),
          tags$ul(
            tags$li("Select the design type: Two-Sample Parallel, One-Sample, or Paired."),
            tags$li("Enter mean difference (Δ) and standard deviation (σ). For paired designs, enter SD of differences (σ_diff)."),
            tags$li("Choose the alternative hypothesis: one-sided or two-sided."),
            tags$li("Choose computation mode: Sample Size or Power using radio buttons."),
            tags$li("Provide desired power (for sample size) or sample size (for power estimation)."),
            tags$li("Click 'Compute' to see results.")
          )
      )
    )
  )
}

mod_continuous_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$error_msg <- renderUI(NULL)
    output$result <- renderPrint(NULL)

    observeEvent(input$go, {

      # --- Assign local variables safely ---
      type <- input$type
      delta <- input$delta
      sd <- input$sd
      sd_diff <- if (!is.null(input$sd_diff)) input$sd_diff else NA
      sig <- input$sig
      alternative <- input$alternative
      mode <- input$mode
      power <- input$power
      n <- input$n

      # --- Validation ---
      validate_inputs <- function() {
        if (is.null(delta) || delta <= 0) return("Provide Δ > 0.")
        if (is.null(sd) || sd <= 0) return("Provide σ > 0.")
        if (type == "paired" && (is.na(sd_diff) || sd_diff <= 0)) return("Provide σ_diff for paired design.")
        if (mode == "n" && (is.null(power) || power <= 0 || power >= 1)) return("Provide valid desired power.")
        if (mode == "power" && (is.null(n) || n <= 0)) return("Provide valid sample size n.")
        return(NULL)
      }

      err <- validate_inputs()
      if (!is.null(err)) {
        output$error_msg <- renderUI(div(style = "color:red;font-weight:bold;", err))
        return()
      }

      # --- Compute ---
      res <- tryCatch({
        if (type == "one_sample") {
          ps_continuous_onesample(
            delta = delta, sd = sd,
            power = if (mode == "n") power else NULL,
            n = if (mode == "power") n else NULL,
            sig.level = sig,
            alternative = alternative
          )
        } else if (type == "paired") {
          ps_continuous_paired(
            delta = delta, sd_diff = sd_diff,
            power = if (mode == "n") power else NULL,
            n = if (mode == "power") n else NULL,
            sig.level = sig,
            alternative = alternative
          )
        } else if (type == "parallel") {
          ps_continuous_parallel(
            delta = delta, sd = sd,
            power = if (mode == "n") power else NULL,
            n1 = if (mode == "power") n else NULL,
            n2 = if (mode == "power") n else NULL,
            sig.level = sig,
            alternative = alternative
          )
        }
      }, error = function(e) {
        output$error_msg <- renderUI(div(style = "color:red;font-weight:bold;", e$message))
        return(NULL)
      })

      if (is.null(res)) return()

      # --- Display results ---
      output$result <- renderPrint({
        cat("=== Continuous Outcome ===\n\n")
        cat("Design Type:", switch(type,
                                   one_sample = "One-Sample",
                                   paired = "Paired",
                                   parallel = "Two-Sample Parallel"
        ), "\n")
        cat("Alternative Hypothesis:", alternative, "\n")
        cat("Significance Level (α):", sig, "\n\n")

        if (mode == "n") {
          cat("Computation Mode: Sample Size determination\n")
          if (!is.null(res$n)) {
            cat("Required Sample Size:", res$n, "\n")
          } else if (!is.null(res$n1) && !is.null(res$n2)) {
            cat("Required Sample Size per Group:\n")
            cat("  n₁ =", res$n1, " | n₂ =", res$n2, "\n")
            if (!is.null(res$total)) cat("Total =", res$total, "\n")
          }
          if (!is.null(res$achieved_power))
            cat("Achieved Power (check):", round(res$achieved_power, 4), "\n")
        } else {
          cat("Computation Mode: Power estimation\n")
          if (!is.null(res$achieved_power)) {
            cat("Achieved Power:", round(res$achieved_power, 4), "\n")
          } else if (!is.null(res$power)) {
            cat("Achieved Power:", round(res$power, 4), "\n")
          }
          if (!is.null(res$n)) cat("Sample Size:", res$n, "\n")
          if (!is.null(res$n1) && !is.null(res$n2))
            cat("Sample Sizes: n₁ =", res$n1, ", n₂ =", res$n2, "\n")
        }
      })
    })
  })
}
