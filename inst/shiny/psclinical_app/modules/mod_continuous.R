mod_continuous_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = "Continuous Outcome Sample Size",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      numericInput(ns("delta"), "Mean Difference (Δ)", value = 2, step = 0.1),
      numericInput(ns("sd"), "Standard Deviation (σ)", value = 5, step = 0.1, min = 0),
      numericInput(ns("power"), "Desired Power (1 - β)", value = 0.8, min = 0, max = 1, step = 0.01),
      numericInput(ns("sig"), "Significance Level (α)", value = 0.05, min = 0, max = 1, step = 0.01),
      selectInput(ns("alternative"), "Alternative Hypothesis", choices = c("two.sided", "one.sided")),
      actionButton(ns("go"), "Compute Sample Size", icon = icon("calculator"), class = "btn-primary"),
      hr(),
      verbatimTextOutput(ns("result"))
    )
  )
}

mod_continuous_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$go, {
      req(input$delta, input$sd, input$power, input$sig)

      res <- ps_continuous_parallel(
        delta = input$delta,
        sd = input$sd,
        power = input$power,
        sig.level = input$sig,
        alternative = input$alternative
      )

      output$result <- renderPrint({
        cat("=== Continuous Outcome Sample Size ===\n")
        print(res)
      })
    })
  })
}
