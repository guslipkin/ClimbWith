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

.make_board_name <- function(brand = '', model = '', size = '', set = '') {
  if (any(purrr::map_lgl(list(brand, model, size, set), is.null))) {
    dt <-
      tibble::tibble(
        'brand' = NA_character_,
        'model' = NA_character_,
        'size' = NA_character_,
        'set' = NA_character_
      ) |>
      head(0)
    return(dt)
  }
  if (any(size != '')) {
    size <-
      size |>
      janitor::make_clean_names() |>
      stringr::str_sub(start = 2)
  }
  if (any(set != '')) set <- janitor::make_clean_names(set)
  expand.grid('brand' = brand, 'model' = model, 'size' = size, 'set' = set) |>
    tibble::as_tibble()
}

.filter_sictb <- function(
    .data,
    input_filter_kilter_board_size,
    input_filter_tension1_board_size, input_filter_tension1_board_set,
    input_filter_tension2_board_size, input_filter_tension2_board_set,
    input_filter_moonboard_set
  ) {

  board_types <-
    dplyr::bind_rows(
      {
        .data |>
          dplyr::select('name', tidyselect::matches('kilter_board')) |>
          tidyr::pivot_longer(
            cols = !name,
            names_to = c('brand', 'model', 'size', 'set'),
            names_pattern = '(kilter)_board_()(\\d+x\\d+(_home)?)'
          ) |>
          dplyr::mutate('set' = '')
      },
      {
        .data |>
          dplyr::select('name', tidyselect::matches('tension_board')) |>
          tidyr::pivot_longer(
            cols = !name,
            names_to = c('brand', 'model', 'size', 'set'),
            names_pattern = '(tension)_board_(tension_[12])_(\\d{1,2}x\\d{1,2})_(.*)'
          )
      },
      {
        .data |>
          dplyr::select('name', tidyselect::matches('moon_board')) |>
          tidyr::pivot_longer(
            cols = !name,
            names_to = c('brand', 'model', 'size', 'set'),
            names_pattern = '(moon)_board_(moon_board)_(mini)?_?(\\d{4})'
          )
      }
    ) |>
    dplyr::filter(!is.na(.data$value))
  user_wants <-
    dplyr::bind_rows(
      .make_board_name('kilter', '', input_filter_kilter_board_size, ''),
      .make_board_name('tension', 'tension_1', input_filter_tension1_board_size, input_filter_tension1_board_set),
      .make_board_name('tension', 'tension_2', input_filter_tension2_board_size, input_filter_tension2_board_set),
      .make_board_name('moon', 'moon_board', '', input_filter_moonboard_set)
    )
  if (nrow(user_wants) > 0) {
    board_types <-
      board_types |>
      dplyr::inner_join(
        y = user_wants,
        by = c('brand', 'model', 'size', 'set')
      )
  }
  board_types |>
    dplyr::select(.data$name) |>
    dplyr::inner_join(
      y = .data,
      by = 'name'
    )
}
