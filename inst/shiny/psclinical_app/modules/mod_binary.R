mod_binary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = "Binary Outcome Sample Size",
      width = 12,
      solidHeader = TRUE,
      status = "success",
      collapsible = TRUE,
      numericInput(ns("p1"), "Proportion in Group 1", value = 0.5, min = 0, max = 1, step = 0.01),
      numericInput(ns("p2"), "Proportion in Group 2", value = 0.7, min = 0, max = 1, step = 0.01),
      numericInput(ns("power"), "Desired Power (1 - β)", value = 0.8, min = 0, max = 1, step = 0.01),
      numericInput(ns("sig"), "Significance Level (α)", value = 0.05, min = 0, max = 1, step = 0.01),
      selectInput(ns("alternative"), "Alternative Hypothesis", choices = c("two.sided", "one.sided")),
      actionButton(ns("go"), "Compute Sample Size", icon = icon("calculator"), class = "btn-success"),
      hr(),
      verbatimTextOutput(ns("result"))
    )
  )
}

mod_binary_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$go, {
      req(input$p1, input$p2, input$power, input$sig)

      res <- ps_binary_parallel(
        p1 = input$p1,
        p2 = input$p2,
        power = input$power,
        sig.level = input$sig,
        alternative = input$alternative
      )

      output$result <- renderPrint({
        cat("=== Binary Outcome Sample Size ===\n")
        print(res)
      })
    })
  })
}
