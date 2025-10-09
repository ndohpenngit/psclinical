library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
library(DT)
library(ggplot2)
library(plotly)
library(psclinical)

# Load internal modules
mod_path <- if (dir.exists("inst/shiny/psclinical_app/modules")) {
  "inst/shiny/psclinical_app/modules"
} else {
  system.file("shiny/psclinical_app/modules", package = "psclinical")
}

module_files <- list.files(mod_path, pattern = "^mod_.*\\.R$", full.names = TRUE)
for (f in module_files) source(f, local = TRUE)

# -------------------------------
# UI
# -------------------------------
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "PSClinical"),

  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Continuous", tabName = "continuous", icon = icon("chart-line")),
                menuItem("Binary", tabName = "binary", icon = icon("square-poll-vertical")),
                menuItem("Survival", tabName = "survival", icon = icon("heartbeat")),
                menuItem("Simulation", tabName = "simulation", icon = icon("flask")),
                menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    # Dark mode toggle
    tags$div(
      style = "text-align:center; padding: 10px 0;",
      tags$label("Dark theme:", `for` = "dark_mode", style = "margin-right:6px;"),
      tags$label(class = "switch",
                 tags$input(id = "dark_mode", type = "checkbox"),
                 tags$span(class = "slider round switch-mini")
      )
    )
  ),

  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "theme.css"),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('toggle-dark', function(state){
          if(state) document.body.classList.add('dark');
          else document.body.classList.remove('dark');
        });
      "))
    ),

    tabItems(
      tabItem(tabName = "continuous", mod_continuous_ui("cont")),
      tabItem(tabName = "binary", mod_binary_ui("bin")),
      tabItem(tabName = "survival", mod_survival_ui("surv")),
      tabItem(tabName = "simulation", mod_simulation_ui("sim"))
    )
  )
)

# -------------------------------
# Server
# -------------------------------
server <- function(input, output, session) {

  # Launch modules
  mod_continuous_server("cont")
  mod_binary_server("bin")
  mod_survival_server("surv")
  mod_simulation_server("sim")

  # Dark mode toggle
  observeEvent(input$dark_mode, {
    session$sendCustomMessage("toggle-dark", input$dark_mode)
  })

  # Show About modal when sidebar item clicked
  observeEvent(input$tabs, {
    if (input$tabs == "about") {
      showModal(modalDialog(
        title = "About PSClinical",
        easyClose = TRUE,
        size = "l",
        HTML(
          "<div class='about-modal'>
            <p><b>PSClinical</b> — Sample Size and Power Computation Toolkit for Clinical Trials.</p>
            <p>Compute sample size and power for continuous, binary, and survival designs.</p>
            <p>Includes simulation-based power estimation and advanced options like accrual/follow-up modeling.</p>
          </div>"
        )
      ))
      # Reset tab so About can be reopened
      updateTabItems(session, "tabs", "continuous")
    }
  })
}

# -------------------------------
# Run App
# -------------------------------
shinyApp(ui, server)
