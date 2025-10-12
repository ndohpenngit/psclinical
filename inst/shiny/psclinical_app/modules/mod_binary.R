mod_binary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Binary Outcome Power/Sample Size Calculator"),
    fluidRow(
      box(
        title = "Inputs", width = 6, solidHeader = TRUE, status = "primary", collapsible = TRUE,
        selectInput(ns("type"), "Design Type",
                    choices = c("Two-Sample Parallel" = "parallel",
                                "One-Sample" = "one_sample",
                                "Paired" = "paired"),
                    selected = "parallel"),
        numericInput(ns("p1"), "Proportion in Group 1 (p1)", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput(ns("p2"), "Proportion in Group 2 (p2)", value = 0.7, min = 0, max = 1, step = 0.01),
        conditionalPanel(
          condition = sprintf("input['%s'] != 'parallel'", ns("type")),
          helpText("For one-sample or paired designs, p2 is treated as the baseline or paired proportion.")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'paired'", ns("type")),
          numericInput(ns("rho"), "Correlation (ρ) between paired responses", value = 0.5, min = 0, max = 1, step = 0.05)
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

      conditionalPanel(
        condition = paste0("input['", ns("go"), "'] > 0"),
        box(title = "Results", width = 6, solidHeader = TRUE, collapsible = TRUE,
            verbatimTextOutput(ns("result"))
        )
      )
    ),

    fluidRow(
      box(title = "Instructions", width = 12, solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE,
          p("This module calculates sample size or achieved power for binary outcomes."),
          tags$ul(
            tags$li("Select the design type: Two-Sample Parallel, One-Sample, or Paired."),
            tags$li("Enter the expected proportions (p1 and p2). For one-sample or paired designs, p2 represents the baseline or paired proportion."),
            tags$li("For paired designs, specify the correlation (ρ) between paired binary outcomes."),
            tags$li("Choose the alternative hypothesis: one-sided or two-sided."),
            tags$li("Choose computation mode: Sample Size or Power."),
            tags$li("Provide desired power (for sample size) or sample size (for power estimation)."),
            tags$li("Click 'Compute' to see results.")
          )
      )
    )
  )
}

mod_binary_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$error_msg <- renderUI(NULL)
    output$result <- renderPrint(NULL)

    observeEvent(input$go, {

      # --- Assign local variables ---
      type <- input$type
      p1 <- input$p1
      p2 <- input$p2
      sig <- input$sig
      alternative <- input$alternative
      mode <- input$mode
      power <- input$power
      n <- input$n
      rho <- input$rho

      # --- Validation ---
      validate_inputs <- function() {
        if (is.null(p1) || p1 < 0 || p1 > 1) return("Provide valid p1 (0–1).")
        if (is.null(p2) || p2 < 0 || p2 > 1) return("Provide valid p2 (0–1).")
        if (type == "paired" && (is.null(rho) || rho < 0 || rho > 1)) return("Provide valid correlation ρ (0–1).")
        if (mode == "n" && (is.null(power) || power <= 0 || power >= 1)) return("Provide valid desired power.")
        if (mode == "power" && (is.null(n) || n <= 0)) return("Provide valid sample size n.")
        return(NULL)
      }

      err <- validate_inputs()
      if (!is.null(err)) {
        output$error_msg <- renderUI(div(style="color:red;font-weight:bold;", err))
        return()
      }

      # --- Compute ---
      res <- tryCatch({
        if (type == "one_sample") {
          ps_binary_onesample(
            p = p1,
            p0 = p2,
            power = if (mode == "n") power else NULL,
            n = if (mode == "power") n else NULL,
            sig.level = sig,
            alternative = alternative
          )
        } else if (type == "paired") {
          ps_binary_paired(
            p1 = p1,
            p2 = p2,
            rho = rho,  # pass correlation here
            power = if (mode == "n") power else NULL,
            n = if (mode == "power") n else NULL,
            sig.level = sig,
            alternative = alternative
          )
        } else {
          ps_binary_parallel(
            p1 = p1,
            p2 = p2,
            power = if (mode == "n") power else NULL,
            n1 = if (mode == "power") n else NULL,
            sig.level = sig,
            alternative = alternative
          )
        }
      }, error = function(e) {
        output$error_msg <- renderUI(div(style="color:red;font-weight:bold;", e$message))
        return(NULL)
      })

      if (is.null(res)) return()

      # --- Display results ---
      output$result <- renderPrint({
        cat("=== Binary Outcome ===\n\n")
        cat("Design Type:", switch(type,
                                   one_sample = "One-Sample",
                                   paired = "Paired",
                                   parallel = "Two-Sample Parallel"
        ), "\n")
        cat("Alternative Hypothesis:", alternative, "\n")
        cat("Significance Level (α):", sig, "\n")
        if (type == "paired" && !is.null(rho))
          cat("Correlation (ρ):", rho, "\n\n")
        else
          cat("\n")

        if (mode == "n") {
          cat("Computation Mode: Sample Size determination\n")
          if (!is.null(res$n)) {
            cat("Required Sample Size:", res$n, "\n")
          } else if (!is.null(res$n1) && !is.null(res$n2)) {
            cat("Required Sample Size per Group:\n")
            cat("  n₁ =", res$n1, " | n₂ =", res$n2, "\n")
            if (!is.null(res$total)) cat("Total =", res$total, "\n")
          }
        } else {
          cat("Computation Mode: Power estimation\n")
          if (!is.null(res$achieved_power))
            cat("Achieved Power:", round(res$achieved_power, 4), "\n")
        }
      })
    })
  })
}

