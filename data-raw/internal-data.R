.column_grouping <-
  readr::read_csv('data-raw/column_grouping.csv', show_col_types = FALSE) |>
  tidyr::pivot_wider(names_from = 'columnGroup', values_from = 'activity') |>
  dplyr::select('Stats', 'Climbing', 'Training Boards', 'Fitness') |>
  suppressWarnings() |>
  as.list() |>
  unlist(recursive = FALSE) |>
  purrr::map(\(x) {
    stats::setNames(janitor::make_clean_names(x), x)
  })

.column_group_wider <- function(.data, column, values) {
  .data |>
    dplyr::select('name', tidyselect::all_of(column)) |>
    dplyr::mutate(
      'new_var' = (\(x) {
        x |>
          stringr::str_split(', ') |>
          purrr::map(janitor::make_clean_names)
      })(.data[[column]])
    ) |>
    dplyr::select(-tidyselect::all_of(column)) |>
    tidyr::unnest_longer('new_var') |>
    dplyr::filter(.data$new_var != 'na') |>
    dplyr::bind_rows(
      tibble::tibble(
        'name' = NA_character_,
        'new_var' = values
      )
    ) |>
    dplyr::mutate('has_new_var' = TRUE) |>
    tidyr::pivot_wider(
      id_cols = 'name',
      names_from = 'new_var',
      values_from = 'has_new_var',
      values_fill = FALSE
    ) |>
    dplyr::filter(!is.na(.data$name)) |>
    dplyr::right_join(
      y = .data,
      by = 'name'
    ) |>
    dplyr::relocate(
      tidyselect::all_of(unname(values)),
      .after = tidyselect::all_of(column)
    ) |>
    dplyr::select(-tidyselect::all_of(column))
}

full_data <-
  readr::read_csv('data-raw/data.csv', show_col_types = FALSE) |>
  janitor::clean_names() |>
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches('spray_wall|board_'), # fix training boards
      \(x) {
        dplyr::case_when(
          x == 'Unknown' ~ '-2',
          x == 'Adjustable' ~ '-1',
          .default = as.character(x)
        ) |>
          stringr::str_split(', ') |>
          purrr::map(as.integer)
      }
    ),
    dplyr::across(
      tidyselect::everything(),
      \(x) {
        if (!all(unique(x) %in% c('Yes', 'No', ''))) return(x)
        dplyr::case_match(x, 'Yes' ~ TRUE, 'No' ~ FALSE, '' ~ NA)
      }
    ),
    'lat' = purrr::map_dbl(.data$google_maps_link, \(x) {
      stringr::str_match(x, '/@(-?\\d+\\.\\d+),')[1,2] |>
        as.numeric()
    }),
    'lon' = purrr::map_dbl(.data$google_maps_link, \(x) {
      stringr::str_match(x, ',(-?\\d+\\.\\d+),\\d+')[1,2] |>
        as.numeric()
    })
  ) |>
  dplyr::mutate(
    'bouldering_wall_height_m' = .data$bouldering_wall_height_ft* .3048,
    'rope_wall_height_m' = .data$rope_wall_height_ft * .3048,
    .after = 'rope_wall_height_ft'
  ) |>
  dplyr::mutate(
    'full_name' = .data$gym_name,
    'name' = janitor::make_clean_names(.data$gym_name),
    .before = 1
  ) |>
  dplyr::select(-'gym_name') |>
  dplyr::arrange(.data$full_name) |>
  .column_group_wider('climbing', .column_grouping$Climbing) |>
  .column_group_wider('fitness', .column_grouping$Fitness)

.board_lookup <- c(
  "kilter_board_7x10_home" = 'Kilter 7x10 (Home)',
  "kilter_board_8x12" = 'Kilter 8x12',
  "kilter_board_12x12" = 'Kilter 12x12',
  "kilter_board_16x12" = 'Kilter 16x12',
  "tension_board_tension_1_8x10_set_a" = 'Tension 1 8x10 Set A',
  "tension_board_tension_1_8x10_set_b" = 'Tension 1 8x10 Set B',
  "tension_board_tension_1_8x10_set_c" = 'Tension 1 8x10 Set C',
  "tension_board_tension_1_8x12_set_a" = 'Tension 1 8x12 Set A',
  "tension_board_tension_1_8x12_set_b" = 'Tension 1 8x12 Set B',
  "tension_board_tension_1_8x12_set_c" = 'Tension 1 8x12 Set C',
  "tension_board_tension_2_8x10_spray" = 'Tension 2 8x10 Spray',
  "tension_board_tension_2_8x10_mirror" = 'Tension 2 8x10 Mirror',
  "tension_board_tension_2_8x12_spray" = 'Tension 2 8x12 Spray',
  "tension_board_tension_2_8x12_mirror" = 'Tension 2 8x12 Mirror',
  "tension_board_tension_2_12x10_spray" = 'Tension 2 12x10 Spray',
  "tension_board_tension_2_12x10_mirror" = 'Tension 2 12x10 Mirror',
  "tension_board_tension_2_12x12_spray" = 'Tension 2 12x12 Spray',
  "tension_board_tension_2_12x12_mirror" = 'Tension 2 12x12 Mirror',
  "moon_board_moon_board_2016" = 'MoonBoard 2016',
  "moon_board_moon_board_2017" = 'MoonBoard 2017',
  "moon_board_moon_board_2019" = 'MoonBoard 2019',
  "moon_board_moon_board_2024" = 'MoonBoard 2024',
  "moon_board_moon_board_mini_2020" = 'MoonBoard Mini 2020',
  "spray_wall" = "Spray Wall"
)

.board_model <-
  readr::read_csv('data-raw/board_model.csv', show_col_types = FALSE) |>
  dplyr::mutate(
    dplyr::across(
      tidyselect::everything(),
      \(x) ifelse(is.na(x), '', x)
    )
  )

.board_insets <- readr::read_csv('data-raw/board_insets.csv', show_col_types = FALSE)

usethis::use_data(
  .board_insets,
  .board_lookup,
  .board_model,
  .column_grouping,
  full_data,
  internal = TRUE,
  overwrite = TRUE
)

