#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  loading_message <-
    c('Chalking up', 'Brushing holds', 'Tying in', 'Flaking rope') |>
    sample() |>
    utils::head(1)
  bs4Dash::dashboardPage(
    scrollToTop = TRUE,
    dark = NULL,
    preloader = list(
      html = shiny::tagList(
        waiter::spin_cube_grid(),
        shiny::tags$b(glue::glue("{loading_message} ..."))
      ),
      color = "#3c8dbc"
    ),
    header = bs4Dash::dashboardHeader(
      fixed = FALSE,
      status = 'lightblue',
      title = bs4Dash::dashboardBrand(
        title = 'ClimbWith',
        color = 'lightblue'
      ),
      shiny::tags$head(
        shinyjs::useShinyjs(),
        shiny::tags$style(
          shiny::HTML(glue::glue("
            .filter_boulder_height .irs--shiny .irs-line {
              background: linear-gradient(90deg, [.get_color_scale(full_data$bouldering_wall_height_ft, .get_height_range('boulder', 'ft'))]);
            }
            .filter_rope_height .irs--shiny .irs-line {
              background: linear-gradient(90deg, [.get_color_scale(full_data$rope_wall_height_ft, .get_height_range('rope', 'ft'))]);
            }
            .filter_board_angle .irs--shiny .irs-bar { background: black; }
          ", .open = '[', .close = ']'))
          # .filter_board_angle .irs--shiny .irs-line {
          #   background: linear-gradient(90deg, [.get_color_scale(full_data |> dplyr::select(tidyselect::matches('_board_')) |> unlist() |> na.omit() |> sort(), (-1):90)]);
          # }
        )
      )
    ),
    sidebar = bs4Dash::dashboardSidebar(disable = TRUE),
    body = bs4Dash::dashboardBody(
      bs4Dash::box(
        title = 'Filters',
        id = 'box_filter',
        width = 12,
        status = 'primary',
        solidHeader = TRUE,
        dropdownMenu = bs4Dash::actionButton(
          inputId = 'clear_filters',
          label = NULL,
          status = 'warning',
          size = 'xs'
        ),
        collapsed = TRUE,
        shiny::fluidRow(
          shiny::column(
            width = 4,
            bs4Dash::box(
              title = 'Climbing',
              width = 12,
              status = 'info',
              solidHeader = TRUE,
              collapsible = FALSE,
              shinyWidgets::awesomeCheckboxGroup(
                inputId = 'filter_climbing',
                label = NULL,
                choices = names(.column_grouping$Climbing)
              )
            )
          ),
          shiny::column(
            width = 4,
            bs4Dash::box(
              title = 'Wall Heights',
              width = 12,
              status = 'info',
              solidHeader = TRUE,
              collapsible = FALSE,
              dropdownMenu = bs4Dash::actionButton(
                inputId = 'height_unit',
                label = NULL,
                status = 'warning',
                size = 'xs',
                icon = NULL, width = NULL
              ),
              shiny::div(
                class = 'filter_boulder_height',
                bs4Dash::tooltip(
                  shinyWidgets::sliderTextInput(
                    inputId = 'filter_boulder_height',
                    label = 'Bouldering',
                    choices = .get_height_range('boulder', 'm'),
                    selected = .get_height_range('boulder', 'm', TRUE),
                    force_edges = TRUE,
                    grid = TRUE
                  ),
                  title = 'Colors represent how common that height is'
                )
              ),
              shiny::div(
                class = 'filter_rope_height',
                bs4Dash::tooltip(
                  shinyWidgets::sliderTextInput(
                    inputId = 'filter_rope_height',
                    label = 'Ropes',
                    choices = .get_height_range('rope', 'm'),
                    selected = .get_height_range('rope', 'm', TRUE),
                    force_edges = TRUE,
                    grid = TRUE
                  ),
                  title = 'Colors represent how common that height is'
                )
              )
            )
          ),
          shiny::column(
            width = 4,
            bs4Dash::box(
              title = 'Fitness',
              width = 12,
              status = 'info',
              solidHeader = TRUE,
              collapsible = FALSE,
              shinyWidgets::awesomeCheckboxGroup(
                inputId = 'filter_fitness',
                label = NULL,
                choices = names(.column_grouping$Fitness)
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
              collapsed = TRUE,
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  bs4Dash::box(
                    title = 'Board Angle',
                    width = 12,
                    collapsible = FALSE,
                    shiny::tags$p('An angle of 0\u00B0 is a vertical wall and 90\u00B0 is a horizontal wall.'),
                    shiny::div(
                      class = 'filter_board_angle',
                      shinyWidgets::sliderTextInput(
                        inputId = 'filter_board_angle',
                        label = NULL,
                        choices = c('Adjustable', as.character(seq(0L, 90L, by = 5L))),
                        selected = c('Adjustable', '90'),
                        force_edges = TRUE,
                        grid = TRUE
                      )
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
                  width = 4,
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
                  width = 4,
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
                  width = 4,
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
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 4,
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
                shiny::column(
                  width = 4,
                  bs4Dash::box(
                    title = 'Grasshopper Board',
                    width = 12,
                    collapsible = FALSE,
                    shinyWidgets::pickerInput(
                      inputId = 'filter_grasshopper_board_size',
                      label = 'Set:',
                      choices = .get_grasshopper_size(),
                      multiple = TRUE,
                      options = shinyWidgets::pickerOptions(
                        actionsBox = TRUE,
                        size = 'auto'
                      )
                    )
                  )
                ),
                shiny::column(
                  width = 4,
                  bs4Dash::box(
                    title = 'Decoy Board',
                    width = 12,
                    collapsible = FALSE,
                    shinyWidgets::pickerInput(
                      inputId = 'filter_decoy_board_size',
                      label = 'Set:',
                      choices = .get_decoy_size(),
                      multiple = TRUE,
                      options = shinyWidgets::pickerOptions(
                        actionsBox = TRUE,
                        size = 'auto'
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      bs4Dash::box(
        id = 'map_box',
        title = 'Map',
        width = 12,
        status = 'primary',
        solidHeader = TRUE,
        dropdownMenu = list(
          bs4Dash::actionButton(
            inputId = 'toggle_clusters',
            label = NULL,
            status = 'warning',
            size = 'xs'
          ),
          bs4Dash::actionButton(
            inputId = 'fit_zoom',
            label = NULL,
            status = 'warning',
            size = 'xs'
          ),
          bs4Dash::actionButton(
            inputId = 'reset_zoom',
            label = NULL,
            status = 'warning',
            size = 'xs'
          )
        ),
        leaflet::leafletOutput('map')
      ),
      bs4Dash::box(
        title = 'Table',
        width = 12,
        id = 'box_table',
        status = 'primary',
        solidHeader = TRUE,
        shinyWidgets::checkboxGroupButtons(
          inputId = 'table_columns',
          label = NULL,
          choices = c('Stats', 'Climbing', 'Training Boards', 'Fitness'),
          selected = c('Climbing', 'Fitness'),
          status = 'table-columns',
          justified = TRUE,
          individual = TRUE,
          checkIcon = list(
            'yes' = shiny::icon("ok", lib = "glyphicon"),
            'no' = shiny::icon("remove", lib = "glyphicon")
          )
        ),
        DT::DTOutput('table')
      ),
      bs4Dash::box(
        title = 'Help (us or you)',
        width = 12,
        id = 'box_help',
        status = 'secondary',
        solidHeader = TRUE,
        collapsible = FALSE,
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::actionButton(
              inputId = 'add_gym',
              shiny::tags$span(htmltools::img(src = 'www/images/plus-circle.svg'), ' Add my gym'),
              icon = NULL, width = NULL,
              class = 'bg-success',
              style = 'width: 100%;',
              onclick = "window.open('https://docs.google.com/forms/d/e/1FAIpQLScWrOgaPLNc8XTY6zSXAUOM3kfpVvGI8gOI-jd70D3IuybF7A/viewform', '_blank')"
            )
          ),
          shiny::column(
            width = 4,
            shiny::actionButton(
              inputId = 'email_me',
              shiny::tags$span(htmltools::img(src = 'www/images/envelope-at.svg'), ' Email us'),
              icon = NULL, width = NULL,
              class = 'bg-warning',
              style = 'width: 100%;',
              onclick = glue::glue("window.open('mailto:contact@{.url()}?subject=ClimbWith', '_blank')")
            )
          ),
          shiny::column(
            width = 4,
            shiny::actionButton(
              inputId = 'report_problems',
              shiny::tags$span(htmltools::img(src = 'www/images/flag.svg'), ' Report a problem'),
              icon = NULL, width = NULL,
              class = 'bg-danger',
              style = 'width: 100%;',
              onclick = "window.open('https://github.com/guslipkin/ClimbWith/issues', '_blank')"
            )
          )
        ),
        shiny::tags$br(),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::actionButton(
              inputId = 'about_us',
              shiny::tags$span(htmltools::img(src = 'www/images/info-circle.svg'), ' About us'),
              icon = NULL, width = NULL,
              class = 'bg-info',
              style = 'width: 100%;'
            )
          )
        ),
        shiny::tags$br(),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::actionButton(
              inputId = 'terms',
              shiny::tags$span(htmltools::img(src = 'www/images/shield-check.svg'), ' Terms'),
              icon = NULL, width = NULL,
              class = 'bg-info',
              style = 'width: 100%;'
            )
          )
        )
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
