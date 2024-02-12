#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  bs4Dash::dashboardPage(
    header = bs4Dash::bs4DashNavbar(
      title = bs4Dash::dashboardBrand(
        title = 'Touch Rocks',
        color = 'primary'
      )
    ),
    sidebar = bs4Dash::dashboardSidebar(
      disable = TRUE
    ),
    body = bs4Dash::dashboardBody(
      bs4Dash::box(
        title = 'Map',
        color = 'secondary',
        width = 12,
        maximizable = TRUE,
        id = 'box_map'
      ),
      bs4Dash::box(
        title = 'Filters',
        width = 12,
        id = 'box_filter'
      ),
      bs4Dash::box(
        title = 'Table',
        width = 12,
        id = 'box_table'
      ),
      golem_add_external_resources()
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "TouchRocks"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
