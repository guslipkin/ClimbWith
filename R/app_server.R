#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  shiny::updateActionButton(inputId = 'clear_filters', label = 'Clear Filters')
  .column_grouping <- .table_column_grouping()
  full_data <- .get_data(.column_grouping)
  dat <- shiny::reactiveVal(full_data)

  output$map <-
    dat() |>
    .create_map() |>
    leaflet::renderLeaflet()
  output$table <-
    dat() |>
    .create_table() |>
    DT::renderDT()

  shiny::observe({
    if (is.null(input$table_columns))
      shinyWidgets::updateCheckboxGroupButtons(
        inputId = 'table_columns',
        selected = names(.column_grouping)
      )
    wanted_cols <- .column_grouping[input$table_columns]
    DT::dataTableProxy('table') |>
      DT::showCols(
        c(0, which(unlist(.column_grouping) %in% unlist(wanted_cols))),
        reset = TRUE
      )
  }) |>
    shiny::bindEvent(input$table_columns, ignoreNULL = FALSE)

  shiny::observe({
    shinyWidgets::updateAwesomeCheckboxGroup(session, inputId = 'filter_climbing', selected = FALSE)
    shinyWidgets::updateAwesomeCheckboxGroup(session, inputId = 'filter_fitness', selected = FALSE)
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
    if (nrow(dt) == 0) {
      shinyWidgets::show_alert(
        title = 'No Matches',
        text = 'No gyms match your filters',
        type = 'warning'
      )
    }

    b <- .get_bounds(dt)
    leaflet::leafletProxy('map', data = dt) |>
      .add_markers_and_fit(dt) |>
      leaflet::fitBounds(
        b[1], b[2], b[3], b[4],
        options = list('maxZoom' = 12, 'padding' = c(15, 15))
      )
  }) |>
    shiny::bindEvent(dat(), input$table_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE)

  shiny::observe({
    full_data |>
      .filter_climbing(input$filter_climbing, .column_grouping$Climbing) |>
      .filter_fitness(input$filter_fitness, .column_grouping$Fitness) |>
      .filter_board_angle(input$filter_board_angle) |>
      .filter_generic_board(input$filter_generic_board) |>
      .filter_sictb(
        input$filter_kilter_board_size,
        input$filter_tension1_board_size, input$filter_tension1_board_set,
        input$filter_tension2_board_size, input$filter_tension2_board_set,
        input$filter_moonboard_board_set
      ) |>
      dat()
  }) |>
    shiny::bindEvent(
      input$filter_climbing, input$filter_fitness,
      input$filter_board_angle, input$filter_generic_board,
      input$filter_kilter_board_size,
      input$filter_tension1_board_size, input$filter_tension1_board_set,
      input$filter_tension2_board_size, input$filter_tension2_board_set,
      input$filter_moonboard_board_set,
      ignoreNULL = FALSE, ignoreInit = TRUE
    )
}
