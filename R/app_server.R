#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  shiny::updateActionButton(inputId = 'clear_filters', label = 'Clear Filters')
  shiny::updateActionButton(
    inputId = 'height_unit',
    label = shiny::HTML('<img src="www/images/gear.svg"/>', ' Units: m')
  )
  dat <- shiny::reactiveVal(full_data)
  selected_dat <- shiny::reactiveVal(full_data)
  height_unit <- shiny::reactiveVal('m')
  boulder_height <- shiny::reactiveVal()
  rope_height <- shiny::reactiveVal()
  map_zoom <- shiny::reactiveVal()

  output$map <-
    dat() |>
    .create_map() |>
    leaflet::renderLeaflet()
  output$table <-
    dat() |>
    .create_table() |>
    DT::renderDT()

  shiny::observe({
    if (height_unit() == 'm') height_unit('ft') else height_unit('m')
    shiny::updateActionButton(
      inputId = 'height_unit',
      label = shiny::HTML('<img src="www/images/gear.svg"/>', glue::glue(' Units: {height_unit()}'))
    )
  }) |>
    shiny::bindEvent(input$height_unit, ignoreInit = TRUE)

  shiny::observe({
    boulder_height(.get_height_range('boulder', height_unit()))
    rope_height(.get_height_range('rope', height_unit()))
    shinyWidgets::updateSliderTextInput(
      session, inputId = 'filter_boulder_height', choices = boulder_height()
    )
    shinyWidgets::updateSliderTextInput(
      session, inputId = 'filter_rope_height', choices = rope_height()
    )
  }) |>
    shiny::bindEvent(height_unit(), ignoreInit = TRUE)

  shiny::observe({
    shinyWidgets::updateAwesomeCheckboxGroup(session, inputId = 'filter_climbing', selected = FALSE)
    shinyWidgets::updateAwesomeCheckboxGroup(session, inputId = 'filter_fitness', selected = FALSE)
    shinyWidgets::updateSliderTextInput(
      session, inputId = 'filter_boulder_height',
      choices = boulder_height(),
      selected = boulder_height()[c(1, length(boulder_height()))]
    )
    shinyWidgets::updateSliderTextInput(
      session, inputId = 'filter_rope_height',
      choices = rope_height(),
      selected = rope_height()[c(1, length(rope_height()))]
    )
    shinyWidgets::updateSliderTextInput(
      session, inputId = 'filter_board_angle',
      choices = c('Adjustable', as.character(seq(0L, 90L, by = 5L))),
      selected = c('Adjustable', '90')
    )
    shinyWidgets::updateAwesomeCheckboxGroup(session, inputId = 'filter_generic_board', selected = FALSE)
    shinyWidgets::updatePickerInput(session, inputId = 'filter_kilter_board_size', selected = FALSE)
    shinyWidgets::updatePickerInput(session, inputId = 'filter_tension1_board_size', selected = FALSE)
    shinyWidgets::updatePickerInput(session, inputId = 'filter_tension1_board_set', selected = FALSE)
    shinyWidgets::updatePickerInput(session, inputId = 'filter_tension2_board_size', selected = FALSE)
    shinyWidgets::updatePickerInput(session, inputId = 'filter_tension2_board_set', selected = FALSE)
    shinyWidgets::updatePickerInput(session, inputId = 'filter_moonboard_board_set', selected = FALSE)
  }) |>
    shiny::bindEvent(input$clear_filters, ignoreNULL = TRUE, ignoreInit = FALSE)


  shiny::observe({
    dt <- if (is.null(input$table_rows_selected)) dat() else dat()[input$table_rows_selected,]
    selected_dat(dt)
    if (nrow(dt) == 0) show_no_matches()
  }) |>
    shiny::bindEvent(
      dat(), input$table_rows_selected, ignoreNULL = FALSE, ignoreInit = FALSE
    )

  shiny::observe({
    b <- .get_bounds(selected_dat())
    leaflet::leafletProxy('map', data = selected_dat()) |>
      leaflet::fitBounds(
        b[1], b[2], b[3], b[4],
        options = list('maxZoom' = 12, 'padding' = rep(24, 2))
      ) |>
      .add_markers(selected_dat(), cluster = isTRUE(input$map_zoom < 10))
    map_zoom(input$map_zoom)
  }) |>
    shiny::bindEvent(
      dat(), selected_dat(), ignoreNULL = FALSE, ignoreInit = TRUE
    )

  shiny::observe({
    if (is.null(map_zoom())) map_zoom(input$map_zoom)
    if (input$map_zoom >= 10 & map_zoom() < 10) {
      cluster <- FALSE
    } else if (input$map_zoom < 10 & map_zoom() >= 10) {
      cluster <- TRUE
    } else {
      cluster <- NULL
    }
    map_zoom(input$map_zoom)
    if (!is.null(cluster)) {
      leaflet::leafletProxy('map', data = selected_dat()) |>
        .add_markers(selected_dat(), cluster = cluster)
    }
  }) |>
    shiny::bindEvent(input$map_zoom, ignoreInit = TRUE, ignoreNULL = TRUE)

  shiny::observe({
    full_data |>
      .filter_climbing(input$filter_climbing, .column_grouping$Climbing) |>
      .filter_heights(input$filter_boulder_height, input$filter_rope_height, height_unit()) |>
      .filter_fitness(input$filter_fitness, .column_grouping$Fitness) |>
      .filter_generic_board(input$filter_generic_board) |>
      .filter_sictb(
        input$filter_kilter_board_size,
        input$filter_tension1_board_size, input$filter_tension1_board_set,
        input$filter_tension2_board_size, input$filter_tension2_board_set,
        input$filter_moonboard_board_set,
        .board_model, .board_insets
      ) |>
      .filter_board_angle(input$filter_board_angle) |>
      dat()
  }) |>
    shiny::bindEvent(
      input$filter_climbing,
      input$filter_boulder_height, input$filter_rope_height,
      input$filter_fitness,
      input$filter_board_angle, input$filter_generic_board,
      input$filter_kilter_board_size,
      input$filter_tension1_board_size, input$filter_tension1_board_set,
      input$filter_tension2_board_size, input$filter_tension2_board_set,
      input$filter_moonboard_board_set,
      ignoreNULL = FALSE, ignoreInit = TRUE
    ) |>
    shiny::debounce(300)

  shiny::observe({
    if (is.null(input$table_columns)) {
      shinyWidgets::updateCheckboxGroupButtons(
        inputId = 'table_columns',
        selected = c('Climbing', 'Training Boards', 'Fitness')
      )
    }
    wanted_cols <- .column_grouping[input$table_columns]
    if (!is.null(wanted_cols$Stats)) {
      wanted_cols$Stats <-
        wanted_cols$Stats[!grepl(glue::glue('height_{height_unit()}'), wanted_cols$Stats)]
    }

    DT::dataTableProxy('table') |>
      DT::showCols(
        c(0, which(unlist(.column_grouping) %in% unlist(wanted_cols))),
        reset = TRUE
      )
  }) |>
    shiny::bindEvent(input$table_columns, height_unit(), dat(), ignoreNULL = FALSE, ignoreInit = FALSE)

  shiny::observe({ show_about_us() }) |>
    shiny::bindEvent(input$about_us)

  shiny::observe({ shiny::invalidateLater(1e4) })
}
