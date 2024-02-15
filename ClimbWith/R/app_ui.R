#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  bs4Dash::dashboardPage(
    scrollToTop = TRUE,
    preloader = list(html = shiny::tagList(waiter::spin_1(), "Loading ..."), color = "#3c8dbc"),
    header = bs4Dash::bs4DashNavbar(
      status = 'primary',
      title = bs4Dash::dashboardBrand(
        title = 'ClimbWith',
        color = 'primary'
      )
    ),
    sidebar = bs4Dash::dashboardSidebar(
      disable = TRUE
    ),
    body = bs4Dash::dashboardBody(
      bs4Dash::box(
        title = 'Map',
        width = 12,
        status = 'secondary',
        solidHeader = TRUE,
        # maximizable = TRUE,
        leaflet::leafletOutput('map')
      ),
      bs4Dash::box(
        title = 'Filters',
        id = 'box_filter',
        width = 12,
        status = 'secondary',
        solidHeader = TRUE,
        dropdownMenu = bs4Dash::actionButton(
          inputId = 'clear_filters',
          label = NULL,
          status = 'warning',
          size = 'xs'
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            bs4Dash::box(
              title = 'Climbing',
              width = 12,
              status = 'info',
              solidHeader = TRUE,
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
              status = 'info',
              solidHeader = TRUE,
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
              status = 'info',
              solidHeader = TRUE,
              collapsible = TRUE,
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  bs4Dash::box(
                    title = 'Board Angle',
                    width = 12,
                    collapsible = FALSE,
                    shiny::tags$p('An angle of 0\u00B0 is a vertical wall and 90\u00B0 is a horizontal wall.'),
                    shinyWidgets::sliderTextInput(
                      inputId = 'filter_board_angle',
                      label = NULL,
                      choices = c('Adjustable', as.character(seq(0L, 90L, by = 5L))),
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
                      choices = .get_kilter_size(),
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
                          choices = .get_tension1_size(),
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
                          choices = .get_tension1_set(),
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
                          choices = .get_tension2_size(),
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
                          choices = .get_tension2_set(),
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
                      choices = .get_moonboard_set(),
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
        status = 'secondary',
        solidHeader = TRUE,
        DT::DTOutput('table')
      ),
      golem_add_external_resources()
    )
    # footer = bs4Dash::dashboardFooter(
    #   left = shiny::div(
    #     'Jump To: ',
    #     shinyWidgets::actionBttn(inputId = 'jump_to_map', label = 'Map', style = 'material-flat'),
    #     shinyWidgets::actionBttn(inputId = 'jump_to_filters', label = 'Filters', style = 'material-flat'),
    #     shinyWidgets::actionBttn(inputId = 'jump_to_table', label = 'Table', style = 'material-flat')
    #   ),
    #   fixed = TRUE
    # )
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
      app_title = "ClimbWith"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
