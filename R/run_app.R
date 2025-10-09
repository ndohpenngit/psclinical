#' Launch PSClinical Shiny App
#' @export
run_app <- function() {
  pkgs <- c("shiny", "shinydashboard", "shinythemes", "DT", "ggplot2", "survival")
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]

  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing)
  }

  app_dir <- system.file("shiny/psclinical_app", package = "psclinical")
  if (app_dir == "") stop("App not found. Please reinstall the 'psclinical' package.")
  shiny::runApp(app_dir, display.mode = "normal")
}
