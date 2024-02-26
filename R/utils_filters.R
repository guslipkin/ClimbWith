.filter_climbing <- function(.data, input_filter_climbing, climbing) {
  if (is.null(input_filter_climbing)) return(.data)
  .data |>
    dplyr::filter(
      dplyr::if_all(tidyselect::all_of(climbing[input_filter_climbing]))
    ) |>
    dplyr::select(.data$name) |>
    unique() |>
    dplyr::left_join(
      y = .data,
      by = 'name'
    )
}

.check_range <- function(x, min_max) {
  !is.na(x) & dplyr::between(x, min_max[1], min_max[2])
}

.filter_heights <- function(.data, input_filter_boulder_height, input_filter_rope_height, height_unit) {
  if (height_unit == 'm') {
    input_filter_boulder_height <- round(.meters_to_feet(input_filter_boulder_height), 0)
    input_filter_rope_height <- round(.meters_to_feet(input_filter_rope_height), 0)
  }
  default_boulder <-
    all(input_filter_boulder_height == .get_height_range('boulder', 'ft', TRUE))
  default_rope <-
    all(input_filter_rope_height == .get_height_range('rope', 'ft', TRUE))
  if (default_boulder & default_rope) return(.data)

  .data |>
    dplyr::mutate(
      '.boulder_in_range' =
        .check_range(.data$bouldering_wall_height_ft, input_filter_boulder_height),
      '.rope_in_range' =
        .check_range(.data$rope_wall_height_ft, input_filter_rope_height)
    ) |>
    dplyr::filter(
      (!.env$default_boulder & !.env$default_rope & .data$.rope_in_range & .data$.boulder_in_range)
      |
      (.env$default_boulder & !.env$default_rope & .data$.rope_in_range)
      |
      (!.env$default_boulder & .env$default_rope & .data$.boulder_in_range)
    ) |>
    dplyr::select(-'.boulder_in_range', -'.rope_in_range')
}

.filter_fitness <- function(.data, input_filter_fitness, fitness) {
  if (is.null(input_filter_fitness)) return(.data)
  .data |>
    dplyr::filter(
      dplyr::if_all(tidyselect::all_of(fitness[input_filter_fitness]))
    ) |>
    dplyr::select(.data$name) |>
    unique() |>
    dplyr::left_join(
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
    unique() |>
    dplyr::left_join(
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
    unique() |>
    dplyr::left_join(
      y = .data,
      by = 'name'
    )
}

.join_board_model <- function(.data, cols, .board_model) {
  dplyr::inner_join(
    x = dplyr::select(.data, tidyselect::all_of(cols)),
    y = dplyr::select(.board_model, 'id', tidyselect::all_of(cols)),
    by = cols
  )
}

.make_board_name <- function(brand, model, size, set, default_size, default_set, .board_model, .board_insets) {
  if (is.null(size) & is.null(set)) {
    dt <-
      tibble::tibble(
        'brand' = NA_character_, 'model' = NA_character_,
        'size' = NA_character_, 'set' = NA_character_
      ) |>
      utils::head(0)
    return(dt)
  }
  if (is.null(size)) size <- default_size
  if (is.null(set)) set <- default_set

  expand.grid('size' = size, 'set' = set) |>
    dplyr::mutate(
      'brand' = brand,
      'model' = model
    ) |>
    .join_board_model(cols = .get_sictb_identifiers()[[brand]], .board_model) |>
    dplyr::rename('contains' = 'id') |>
    dplyr::inner_join(
      y = .board_insets,
      by = 'contains'
    ) |>
    dplyr::select('id') |>
    unique() |>
    dplyr::inner_join(
      y = .board_model,
      by = 'id'
    )
}

.filter_sictb <- function(
    .data,
    input_filter_kilter_board_size,
    input_filter_tension1_board_size, input_filter_tension1_board_set,
    input_filter_tension2_board_size, input_filter_tension2_board_set,
    input_filter_moonboard_set,
    .board_model, .board_insets
  ) {

  user_wants <-
    dplyr::bind_rows(
      .make_board_name(
        'Kilter', '', input_filter_kilter_board_size, NULL,
        .get_kilter_size(), .get_kilter_set(), .board_model, .board_insets
      ),
      .make_board_name(
        'Tension', '1', input_filter_tension1_board_size, input_filter_tension1_board_set,
        .get_tension1_size(), .get_tension1_set(), .board_model, .board_insets
      ),
      .make_board_name(
        'Tension', '2', input_filter_tension2_board_size, input_filter_tension2_board_set,
        .get_tension2_size(), .get_tension2_set(), .board_model, .board_insets
      ),
      .make_board_name(
        'MoonBoard', '', NULL, input_filter_moonboard_set,
        .get_moonboard_size(), .get_moonboard_set(), .board_model, .board_insets
      ),
      .id = 'board_id'
    )
  if (nrow(user_wants) == 0) return(.data)

  .data |>
    dplyr::select('name', tidyselect::all_of(.board_model$column_name)) |>
    tidyr::pivot_longer(
      cols = -'name',
      names_to = 'column_name',
      values_to = 'angle'
    ) |>
    dplyr::filter(!is.na(.data$angle)) |>
    dplyr::inner_join(
      y = .board_model,
      by = 'column_name'
    ) |>
    dplyr::select('name', 'id') |> # ^gyms actually have
    dplyr::inner_join(
      y = user_wants,
      by = 'id'
    ) |>
    dplyr::mutate('boards_needed' = length(unique(.data$board_id))) |>
    dplyr::filter(
      length(unique(.data$board_id)) == .data$boards_needed,
      .by = 'name'
    ) |>
    dplyr::select('name') |>
    dplyr::inner_join(
      y = .data,
      by = 'name'
    ) |>
    unique()
}
