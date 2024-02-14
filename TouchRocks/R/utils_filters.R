.filter_climbing <- function(.data, input_filter_climbing) {
  if (is.null(input_filter_climbing)) return(.data)
  climbing <- c(
    'Bouldering' = 'bouldering',
    'Top Rope' = 'top_rope',
    'Lead' = 'lead',
    'Auto Belay' = 'auto_belay'
  )
  .data |>
    dplyr::filter(
      dplyr::if_any(tidyselect::all_of(climbing[input_filter_climbing]))
    ) |>
    dplyr::select(.data$name) |>
    dplyr::inner_join(
      y = .data,
      by = 'name'
    )
}

.filter_fitness <- function(.data, input_filter_fitness) {
  if (is.null(input_filter_fitness)) return(.data)
  fitness <- c(
    'Yoga Studio' = 'yoga_studio',
    'Free Weights' = 'free_weights',
    'Weight Machines' = 'weight_machines',
    'Cardio Machines' = 'cardio_machines'
  )
  .data |>
    dplyr::filter(
      dplyr::if_any(tidyselect::all_of(fitness[input_filter_fitness]))
    ) |>
    dplyr::select(.data$name) |>
    dplyr::inner_join(
      y = .data,
      by = 'name'
    )
}

.filter_board_angle <- function(.data, input_filter_board_angle) {
  if (all(input_filter_board_angle == c('Adjustable', '90'))) return(.data)
  board_angle <-
    (input_filter_board_angle == "Adjustable") |>
    ifelse(-1L, as.integer(input_filter_board_angle)) |>
    sort() |>
    suppressWarnings()
  .data |>
    dplyr::select('name', tidyselect::matches('_board_|spray_wall')) |>
    dplyr::select(
      'name',
      tidyselect::where(
        \(x) {
          x <- as.integer(x) |> suppressWarnings()
          any(!is.na(x) & (x == -2 | dplyr::between(x, board_angle[1], board_angle[2])))
        }
      )
    ) |>
    dplyr::filter(
      dplyr::if_any(tidyselect::matches('_board_|spray_wall'), \(x) !is.na(x))
    ) |>
    dplyr::select(.data$name) |>
    dplyr::inner_join(
      y = .data,
      by = 'name'
    )
}

.filter_generic_board <- function(.data, input_filter_generic_board) {
  if (is.null(input_filter_generic_board)) return(.data)
  generic_board <- c(
    'Campus Board' = 'campus_board',
    'Spray Wall' = 'spray_wall'
  )
  .data |>
    dplyr::mutate(
      'spray_wall' = !is.na(.data$spray_wall)
    ) |>
    dplyr::filter(
      dplyr::if_all(tidyselect::all_of(generic_board[input_filter_generic_board]))
    ) |>
    dplyr::select(.data$name) |>
    dplyr::inner_join(
      y = .data,
      by = 'name'
    )
}
