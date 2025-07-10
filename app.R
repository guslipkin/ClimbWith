# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

source('data-raw/internal-data.R')
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options("golem.app.prod" = TRUE, "shiny.sessionTimeout" = 0, "shiny.autoreload" = FALSE)
ClimbWith::run_app() # add parameters here (if any)
