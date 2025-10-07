# ============================================================
# PSClinical Shiny App
# ============================================================

library(shiny)
library(shinydashboard)
library(psclinical)

# Dynamically detect module path
if (dir.exists("inst/shiny/psclinical_app/modules")) {
  mod_path <- "inst/shiny/psclinical_app/modules"
} else {
  mod_path <- system.file("shiny/psclinical_app/modules", package = "psclinical")
}

# Source all modules
module_files <- list.files(mod_path, pattern = "^mod_.*\\.R$", full.names = TRUE)
for (f in module_files) source(f, local = TRUE)

# -------------------------------
# Main UI
# -------------------------------
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "'PSClinical' Power and Sample size app"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Continuous", tabName = "continuous", icon = icon("chart-line")),
      menuItem("Binary", tabName = "binary", icon = icon("square-poll-vertical")),
      menuItem("Survival", tabName = "survival", icon = icon("heartbeat")),
      menuItem("Simulation", tabName = "simulation", icon = icon("flask")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "continuous", mod_continuous_ui("cont")),
      tabItem(tabName = "binary", mod_binary_ui("bin")),
      tabItem(tabName = "survival", mod_survival_ui("surv")),
      tabItem(tabName = "simulation", mod_simulation_ui("sim")),
      tabItem(tabName = "about",
              fluidRow(
                box(
                  width = 12,
                  title = "About PSClinical",
                  status = "primary",
                  solidHeader = TRUE,
                  p("PSClinical — Sample Size and Power Computation Toolkit for Clinical Trials."),
                  p("Compute sample size and power for continuous, binary, and survival designs."),
                  p("Includes simulation-based power estimation and advanced options like accrual/follow-up modeling.")
                )
              )
      )
    )
  )
)
# -------------------------------
# Main Server
# -------------------------------
server <- function(input, output, session) {
  mod_continuous_server("cont")
  mod_binary_server("bin")
  mod_survival_server("surv")
  mod_simulation_server("sim")
}

shinyApp(ui, server)

