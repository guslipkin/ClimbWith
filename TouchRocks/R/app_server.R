#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  full_data <- .get_data()
  data <- shiny::reactiveVal(full_data)
  output$map <-
    data() |>
    .create_map() |>
    leaflet::renderLeaflet()
  output$table <-
    data() |>
    .create_table() |>
    DT::renderDT()

  shiny::observe({
    dt <- if (is.null(input$table_rows_selected)) data() else data()[input$table_rows_selected,]
    if (nrow(dt) == 0) {
      shinyWidgets::show_alert(
        title = 'No Matches',
        text = 'No gyms match your filters',
        type = 'warning'
      )
    }
      leaflet::leafletProxy('map', data = dt) |>
        .add_markers_and_fit(.get_bounds(dt))
  }) |>
    shiny::bindEvent(data(), input$table_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE)

  shiny::observe({
    full_data |>
      .filter_climbing(input$filter_climbing) |>
      .filter_fitness(input$filter_fitness) |>
      .filter_board_angle(input$filter_board_angle) |>
      .filter_generic_board(input$filter_generic_board) |>
      .filter_sictb(
        input$filter_kilter_board_size,
        input$filter_tension1_board_size, input$filter_tension1_board_set,
        input$filter_tension2_board_size, input$filter_tension2_board_set,
        input$filter_moonboard_board_set
      ) |>
      data()
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
