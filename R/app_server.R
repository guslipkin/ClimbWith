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
  shiny::updateActionButton(
    inputId = 'toggle_clusters',
    label = shiny::HTML(
      '<img src="www/images/toggle-on.svg"/>', 'Toggle Clusters'
    )
  )
  shiny::updateActionButton(
    inputId = 'fit_zoom',
    label = shiny::HTML(
      '<img src="www/images/arrows-angle-expand.svg"/>', 'Fit Points'
    )
  )
  shiny::updateActionButton(
    inputId = 'reset_zoom',
    label = shiny::HTML(
      '<img src="www/images/arrows-angle-contract.svg"/>', 'Reset Zoom'
    )
  )
  shinyjs::runjs("
    $('.card-tools')
      .contents()
      .filter(function() { return this.nodeType == 3; })
      .replaceWith(' ');
  ")
  dat <- shiny::reactiveVal(full_data)
  table_dat <- shiny::reactiveVal(full_data)
  selected_dat <- shiny::reactiveVal(full_data)
  height_unit <- shiny::reactiveVal('m')
  boulder_height <- shiny::reactiveVal()
  rope_height <- shiny::reactiveVal()
  map_zoom <- shiny::reactiveVal()
  stop_clusters_default <- 11
  stop_clusters <- shiny::reactiveVal(stop_clusters_default)

  output$map <-
    dat() |>
    .create_map() |>
    leaflet::renderLeaflet()
  output$table <-
    table_dat() |>
    .create_table() |>
    DT::renderDT()

  #----user settings----
  shiny::observe({
    if (stop_clusters() == stop_clusters_default) {
      stop_clusters(Inf)
      status <- 'off'
    } else {
      stop_clusters(stop_clusters_default)
      status <- 'on'
    }
    shiny::updateActionButton(
      inputId = 'toggle_clusters',
      label = shiny::HTML(
        glue::glue('<img src="www/images/toggle-{status}.svg"/>'),
        'Toggle Clusters'
      )
    )
  }) |>
    shiny::bindEvent(input$toggle_clusters)

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

  #----filters----
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
    shinyWidgets::updatePickerInput(session, inputId = 'filter_grasshopper_board_size', selected = FALSE)
    shinyWidgets::updatePickerInput(session, inputId = 'filter_decoy_board_size', selected = FALSE)
  }) |>
    shiny::bindEvent(input$clear_filters, ignoreNULL = TRUE, ignoreInit = FALSE)

  shiny::observe({
    if (is.null(input$table_rows_selected) & !is.null(input$map_bounds)) {
      dat() |>
        dplyr::filter(
          dplyr::between(.data$lon, input$map_bounds$west, input$map_bounds$east),
          dplyr::between(.data$lat, input$map_bounds$south, input$map_bounds$north)
        ) |>
        table_dat()
    }
    if (nrow(table_dat()) == 0) show_no_matches()
  }) |>
    shiny::bindEvent(
      dat(), input$table_rows_selected, input$map_bounds,
      ignoreNULL = FALSE, ignoreInit = FALSE
    )

  #----user zoom----
  shiny::observe({
    shiny::req(selected_dat())
    leaflet::leafletProxy('map', data = selected_dat()) |>
      .fit_bounds(selected_dat())
  }) |>
    shiny::bindEvent(input$fit_zoom, ignoreInit = TRUE)

  shiny::observe({
    leaflet::leafletProxy('map', data = dat()) |>
      .fit_bounds(dat())
  }) |>
    shiny::bindEvent(input$reset_zoom, ignoreInit = TRUE)

  #----row selection----
  shiny::observe({
    dt <- table_dat()[rlang::`%||%`(input$table_rows_selected, seq_len(nrow(table_dat()))),]
    selected_dat(dt)
  }) |>
    shiny::bindEvent(
      table_dat(), input$table_rows_selected, ignoreNULL = FALSE, ignoreInit = FALSE
    )

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
        input$filter_grasshopper_board_size,
        input$filter_decoy_board_size,
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
      input$filter_grasshopper_board_size,
      input$filter_decoy_board_size,
      ignoreNULL = FALSE, ignoreInit = TRUE
    ) |>
    shiny::debounce(300)

  #----clusters----
  shiny::observe({
    shiny::req(selected_dat())
    if (is.null(map_zoom())) map_zoom(input$map_zoom)
    if (
        !is.null(input$table_rows_selected) |
        stop_clusters() == Inf |
        (input$map_zoom >= stop_clusters() & map_zoom() < stop_clusters())
      ) {
      cluster <- FALSE
    } else if (input$map_zoom < stop_clusters()) {
      cluster <- TRUE
    } else {
      cluster <- FALSE
    }
    map_zoom(input$map_zoom)
    if (!is.null(cluster)) {
      leaflet::leafletProxy('map', data = selected_dat()) |>
        .add_markers(selected_dat(), cluster = cluster)
    }
  }) |>
    shiny::bindEvent(
      input$map_zoom, stop_clusters(), input$table_rows_selected, selected_dat(),
      ignoreInit = TRUE, ignoreNULL = FALSE
    )

  #----table columns----
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
    shiny::bindEvent(
      input$table_columns, height_unit(), table_dat(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )

  #----misc----
  shiny::observe({ show_about_us() }) |>
    shiny::bindEvent(input$about_us)
  shiny::observe({ show_terms() }) |>
    shiny::bindEvent(input$terms)

  shiny::observe({ shiny::invalidateLater(1e4) })
}
