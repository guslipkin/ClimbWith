.filter_climbing <- function(tab, input_filter_climbing, climbing) {
  if (is.null(input_filter_climbing)) return(tab)
  tab |>
    dplyr::filter(
      dplyr::if_all(tidyselect::all_of(climbing[input_filter_climbing]))
    ) |>
    dplyr::select(.data$name) |>
    unique() |>
    dplyr::left_join(
      y = tab,
      by = 'name'
    )
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

.make_board_name <- function(brand, model, size, set, default_size, default_set) {
  has_size_set <- purrr::map_lgl(list(size, set), is.null)
  if (all(has_size_set)) {
      dt <-
        tibble::tibble(
          'brand' = NA_character_,
          'model' = NA_character_,
          'size' = NA_character_,
          'set' = NA_character_
        ) |>
        utils::head(0)
      return(dt)
  } else if (any(has_size_set)) {
    dt <-
      expand.grid(
        'brand' = brand,
        'model' = model,
        'size' = `if`(is.null(size), default_size, size),
        'set' = `if`(is.null(set), default_set, set)
      ) |>
      tibble::as_tibble() |>
      utils::head(0) |>
      purrr::map(as.character)
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

  user_wants <-
    dplyr::bind_rows(
      .make_board_name(
        'kilter', '', input_filter_kilter_board_size, '',
        .get_kilter_size(), .get_kilter_set()
      ),
      .make_board_name(
        'tension', 'tension_1', input_filter_tension1_board_size, input_filter_tension1_board_set,
        .get_tension1_size(), .get_tension1_set()
      ),
      .make_board_name(
        'tension', 'tension_2', input_filter_tension2_board_size, input_filter_tension2_board_set,
        .get_tension2_size(), .get_tension2_set()
      ),
      .make_board_name(
        'moon', 'moon_board', '', input_filter_moonboard_set,
        .get_moonboard_size(), .get_moonboard_set()
      )
    ) |>
    dplyr::mutate(
      'set' = ifelse(.data$brand == 'moon', stringr::str_replace(.data$set, '\\w', ''), .data$set)
    )
  if (nrow(user_wants) == 0) return(.data)

  board_types <-
    dplyr::bind_rows(
      'kilter' = {
        .data |>
          dplyr::select('name', tidyselect::matches('kilter_board')) |>
          tidyr::pivot_longer(
            cols = -'name',
            names_to = c('brand', 'model', 'size', 'set'),
            names_pattern = '(kilter)_board_()(\\d+x\\d+(_home)?)'
          ) |>
          dplyr::mutate('set' = '')
      },
      'tension' = {
        .data |>
          dplyr::select('name', tidyselect::matches('tension_board')) |>
          tidyr::pivot_longer(
            cols = -'name',
            names_to = c('brand', 'model', 'size', 'set'),
            names_pattern = '(tension)_board_(tension_[12])_(\\d{1,2}x\\d{1,2})_(.*)'
          )
      },
      'moonboard' = {
        .data |>
          dplyr::select('name', tidyselect::matches('moon_board')) |>
          tidyr::pivot_longer(
            cols = -'name',
            names_to = c('brand', 'model', 'size', 'set'),
            names_pattern = '(moon)_board_(moon_board)_(mini)?_?(\\d{4})'
          )
      }
    ) |>
    dplyr::filter(!is.na(.data$value))

  user_wants |>
    dplyr::mutate('.brand_count' = length(unique(.data$brand))) |>
    dplyr::inner_join(
      y = board_types,
      by = c('brand', 'model', 'size', 'set')
    ) |>
    dplyr::filter(
      length(unique(.data$brand)) == .data$.brand_count,
      .by = 'name'
    ) |>
    dplyr::select('name') |>
    dplyr::inner_join(
      y = .data,
      by = 'name'
    ) |>
    unique()
}
