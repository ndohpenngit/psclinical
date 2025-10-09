mod_binary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Binary Outcome: Two-Sample Parallel Design"),
    fluidRow(
      box(
        title = "Inputs",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,

        # Design type
        selectInput(
          ns("type"), "Design Type",
          choices = c("superiority", "noninferiority", "equivalence"),
          selected = "superiority"
        ),

        # Common parameters
        numericInput(ns("p1"), "Proportion in Group 1 (p1)", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput(ns("p2"), "Proportion in Group 2 (p2)", value = 0.7, min = 0, max = 1, step = 0.01),
        conditionalPanel(
          condition = sprintf("input['%s-type'] != 'superiority'", ns("type")),
          numericInput(ns("margin"), "Margin (for NI/Equivalence)", value = NA, step = 0.01)
        ),
        numericInput(ns("sig"), "Significance Level (α)", value = 0.05, min = 0, max = 1, step = 0.01),
        selectInput(ns("alternative"), "Alternative Hypothesis", choices = c("two.sided", "one.sided")),
        numericInput(ns("ratio"), "Allocation Ratio (n1/n2)", value = 1, min = 0.1, step = 0.1),

        # Computation mode
        selectInput(
          ns("mode"),
          "Computation Mode",
          choices = c("Compute Sample Size" = "n", "Compute Power" = "power")
        ),
        conditionalPanel(
          condition = paste0("input['", ns("mode"), "'] == 'n'"),
          numericInput(ns("power"), "Desired Power (1 - β)", value = 0.8, min = 0.01, max = 0.999, step = 0.01)
        ),
        conditionalPanel(
          condition = paste0("input['", ns("mode"), "'] == 'power'"),
          numericInput(ns("n1"), "Sample Size Group 1 (n1)", value = 50, min = 1, step = 1),
          numericInput(ns("n2"), "Sample Size Group 2 (n2)", value = 50, min = 1, step = 1)
        ),

        actionButton(ns("go"), "Compute", icon = icon("calculator"), class = "btn-primary"),
        hr(),
        uiOutput(ns("error_msg"))
      ),

      box(
        title = "Results",
        width = 6,
        solidHeader = TRUE,
        status = NULL,
        collapsible = TRUE,
        verbatimTextOutput(ns("result"))
      )
    ),

    fluidRow(
      box(
        title = "Instructions",
        width = 12,
        solidHeader = FALSE,
        status = NULL,
        collapsible = TRUE,
        collapsed = TRUE,
        p("This module calculates sample size or achieved power for a binary outcome using a parallel group design."),
        tags$ul(
          tags$li("Enter the expected proportions for both groups (p1 and p2)."),
          tags$li("Select the design type: superiority, noninferiority, or equivalence."),
          tags$li("Provide margin for noninferiority or equivalence designs."),
          tags$li("Choose computation mode: Sample Size or Power."),
          tags$li("Provide desired power (for sample size) or sample sizes (for power estimation)."),
          tags$li("Click 'Run' to compute and see the results.")
        )
      )
    )
  )
}

# mod_binary_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#
#     output$error_msg <- renderUI(NULL)
#
#     observeEvent(input$go, {
#       output$error_msg <- renderUI(NULL)
#       output$result <- renderPrint(NULL)
#
#       # --- Validation ---
#       validate_inputs <- function() {
#         if (is.null(input$p1) || is.null(input$p2) || input$p1 < 0 || input$p1 > 1 || input$p2 < 0 || input$p2 > 1)
#           return("Please provide valid probabilities between 0 and 1 for both groups.")
#
#         if (input$type != "superiority" && (is.null(input$margin) || is.na(input$margin)))
#           return("Margin is required for noninferiority or equivalence designs.")
#
#         if (input$mode == "n" && (is.null(input$power) || input$power <= 0 || input$power >= 1))
#           return("Please provide a valid desired power between 0 and 1.")
#
#         if (input$mode == "power" && (is.null(input$n1) || is.null(input$n2) || input$n1 <= 0 || input$n2 <= 0))
#           return("Please provide valid sample sizes for both groups.")
#
#         return(NULL)
#       }
#
#       err <- validate_inputs()
#       if (!is.null(err)) {
#         output$error_msg <- renderUI(div(style = "color:red; font-weight:bold;", err))
#         return()
#       }
#
#       res <- tryCatch({
#         ps_binary_parallel(
#           p1 = input$p1,
#           p2 = input$p2,
#           type = input$type,
#           margin = if (!is.null(input$margin) && !is.na(input$margin)) input$margin else NULL,
#           power = if (input$mode == "n") input$power else NULL,
#           n1 = if (input$mode == "power") input$n1 else NULL,
#           n2 = if (input$mode == "power") input$n2 else NULL,
#           sig.level = input$sig,
#           ratio = input$ratio,
#           alternative = input$alternative
#         )
#       }, error = function(e) {
#         output$error_msg <- renderUI(div(style = "color:red; font-weight:bold;", e$message))
#         return(NULL)
#       })
#
#       if (is.null(res)) return()
#
#       # --- Display result ---
#       output$result <- renderPrint({
#         cat("=== Binary Outcome: Two-Sample Parallel Design ===\n\n")
#
#         if (input$mode == "n") {
#           cat("Computation mode: Sample Size determination\n")
#           cat("Design:", input$type, "\n")
#           if (input$type %in% c("noninferiority","equivalence")) cat("Margin:", input$margin, "\n")
#           cat("Desired Power:", input$power, "\n\n")
#           cat("Required n1 =", res$n1, ", n2 =", res$n2, "\n")
#           cat("Total Sample Size =", res$total, "\n")
#         } else {
#           cat("Computation mode: Power estimation\n")
#           cat("Design:", input$type, "\n")
#           if (input$type %in% c("noninferiority","equivalence")) cat("Margin:", input$margin, "\n")
#           cat("n1 =", input$n1, ", n2 =", input$n2, "\n\n")
#
#           power_value <- if (!is.null(res$power)) res$power else
#             if (!is.null(res$achieved_power)) res$achieved_power else NA
#
#           if (is.na(power_value)) {
#             cat("Power could not be computed.\n")
#           } else {
#             cat("Achieved Power =", round(power_value, 4), "\n")
#           }
#         }
#       })
#     })
#   })
# }
mod_binary_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$error_msg <- renderUI(NULL)
    output$result <- renderPrint(NULL)

    observeEvent(input$go, {
      output$error_msg <- renderUI(NULL)
      output$result <- renderPrint(NULL)

      # --- Validation ---
      validate_inputs <- function() {
        if (is.null(input$p1) || is.null(input$p2))
          return("Provide valid p1 and p2 between 0 and 1.")
        if (input$type != "superiority" && (is.null(input$margin) || is.na(input$margin)))
          return("Margin is required for noninferiority or equivalence designs.")
        if (input$mode == "n" && (is.null(input$power) || input$power <= 0 || input$power >= 1))
          return("Provide valid desired power between 0 and 1.")
        if (input$mode == "power" && (is.null(input$n1) || is.null(input$n2) || input$n1 <= 0 || input$n2 <= 0))
          return("Provide valid sample sizes for both groups.")
        return(NULL)
      }

      err <- validate_inputs()
      if (!is.null(err)) {
        output$error_msg <- renderUI(div(style="color:red; font-weight:bold;", err))
        return()
      }

      res <- tryCatch({
        ps_binary_parallel(
          p1 = input$p1,
          p2 = input$p2,
          type = input$type,
          margin = if (!is.null(input$margin) && !is.na(input$margin)) input$margin else NULL,
          power = if (input$mode == "n") input$power else NULL,
          n1 = if (input$mode == "power") input$n1 else NULL,
          n2 = if (input$mode == "power") input$n2 else NULL,
          sig.level = input$sig,
          ratio = input$ratio,
          alternative = input$alternative
        )
      }, error = function(e) {
        output$error_msg <- renderUI(div(style="color:red; font-weight:bold;", e$message))
        return(NULL)
      })

      if (is.null(res)) return()

      # --- Display ---
      output$result <- renderPrint({
        cat("=== Binary Outcome: Two-Sample Parallel Design ===\n\n")

        if (input$mode == "n") {
          cat("Computation mode: Sample Size determination\n")
          cat("Design:", input$type, "\n")
          cat("Desired Power:", input$power, "\n\n")
          cat("Required n1 =", res$n1, ", n2 =", res$n2, "\n")
          cat("Total Sample Size =", res$total, "\n")
        } else {
          cat("Computation mode: Power estimation\n")
          cat("Design:", input$type, "\n")
          cat("n1 =", input$n1, ", n2 =", input$n2, "\n\n")

          power_value <- if (!is.null(res$power)) res$power else
            if (!is.null(res$achieved_power)) res$achieved_power else NA

          if (is.na(power_value)) cat("Power could not be computed.\n")
          else cat("Achieved Power =", round(power_value, 4), "\n")
        }
      })
    })
  })
}
