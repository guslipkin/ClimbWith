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
        leaflet::leafletOutput('map')
      ),
      bs4Dash::box(
        title = 'Filters',
        width = 12,
        id = 'box_filter',
        shiny::fluidRow(
          shiny::column(
            width = 6,
            bs4Dash::box(
              title = 'Climbing',
              width = 12,
              collapsible = FALSE,
              shinyWidgets::awesomeCheckboxGroup(
                inputId = 'filter_climbing',
                label = NULL,
                choices = c('Bouldering', 'Top Rope', 'Lead', 'Auto Belay')
              )
            )
          ),
          shiny::column(
            width = 6,
            bs4Dash::box(
              title = 'Fitness',
              width = 12,
              collapsible = FALSE,
              shinyWidgets::awesomeCheckboxGroup(
                inputId = 'filter_fitness',
                label = NULL,
                choices = c('Yoga Studio', 'Free Weights', 'Weight Machines', 'Cardio Machines')
              )
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            bs4Dash::box(
              title = 'Training Boards',
              width = 12,
              collapsible = FALSE,
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  bs4Dash::box(
                    title = 'Board Angle',
                    width = 12,
                    collapsible = FALSE,
                    shiny::tags$p('An angle of 0° is a vertical wall and 90° is a horizontal wall.'),
                    shinyWidgets::sliderTextInput(
                      inputId = 'filter_board_angle',
                      label = NULL,
                      choices = c('Adjustable', seq(0L, 90L, by = 5L)),
                      selected = c('Adjustable', '90'),
                      force_edges = TRUE,
                      grid = TRUE
                    )
                  )
                ),
                shiny::column(
                  width = 6,
                  bs4Dash::box(
                    title = 'Generic Boards',
                    width = 12,
                    collapsible = FALSE,
                    shinyWidgets::awesomeCheckboxGroup(
                      inputId = 'filter_generic_board',
                      label = NULL,
                      choices = c('Campus Board', 'Spray Wall')
                    )
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 3,
                  bs4Dash::box(
                    title = 'Kilter',
                    width = 12,
                    collapsible = FALSE,
                    shinyWidgets::pickerInput(
                      inputId = 'filter_kilter_board_size',
                      label = 'Size:',
                      choices = c('7x10 (Home)', '8x12', '12x12', '16x12'),
                      multiple = TRUE,
                      options = shinyWidgets::pickerOptions(
                        actionsBox = TRUE,
                        size = 'auto'
                      )
                    )
                  )
                ),
                shiny::column(
                  width = 3,
                  bs4Dash::box(
                    title = 'Tension 1',
                    width = 12,
                    collapsible = FALSE,
                    shiny::fluidRow(
                      shiny::column(
                        width = 12,
                        shinyWidgets::pickerInput(
                          inputId = 'filter_tension1_board_size',
                          label = 'Size:',
                          choices = c('8x10', '10x12'),
                          multiple = TRUE,
                          options = shinyWidgets::pickerOptions(
                            actionsBox = TRUE,
                            size = 'auto'
                          )
                        )
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(
                        width = 12,
                        shinyWidgets::pickerInput(
                          inputId = 'filter_tension1_board_set',
                          label = 'Set:',
                          choices = c('A', 'B', 'C'),
                          multiple = TRUE,
                          options = shinyWidgets::pickerOptions(
                            actionsBox = TRUE,
                            size = 'auto'
                          )
                        )
                      )
                    )
                  )
                ),
                shiny::column(
                  width = 3,
                  bs4Dash::box(
                    title = 'Tension 2',
                    width = 12,
                    collapsible = FALSE,
                    shiny::fluidRow(
                      shiny::column(
                        width = 12,
                        shinyWidgets::pickerInput(
                          inputId = 'filter_tension2_board_size',
                          label = 'Size:',
                          choices = c('8x10', '12x10', '8x12', '12x12'),
                          multiple = TRUE,
                          options = shinyWidgets::pickerOptions(
                            actionsBox = TRUE,
                            size = 'auto'
                          )
                        )
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(
                        width = 12,
                        shinyWidgets::pickerInput(
                          inputId = 'filter_tension2_board_set',
                          label = 'Set:',
                          choices = c('Spray', 'Mirror'),
                          multiple = TRUE,
                          options = shinyWidgets::pickerOptions(
                            actionsBox = TRUE,
                            size = 'auto'
                          )
                        )
                      )
                    )
                  )
                ),
                shiny::column(
                  width = 3,
                  bs4Dash::box(
                    title = 'MoonBoard',
                    width = 12,
                    collapsible = FALSE,
                    shinyWidgets::pickerInput(
                      inputId = 'filter_moonboard_board_set',
                      label = 'Set:',
                      choices = c('2016', '2017', '2019', '2024', 'Mini 2020'),
                      multiple = TRUE,
                      options = shinyWidgets::pickerOptions(
                        actionsBox = TRUE,
                        size = 'auto'
                      )
                    )
                  )
                ),
              )
            )
          )
        )
      ),
      bs4Dash::box(
        title = 'Table',
        width = 12,
        id = 'box_table',
        DT::DTOutput('table')
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
