.create_table <- function(.data) {
  .data |>
    dplyr::mutate(
      'full_name' = glue::glue("<a href='{.data$gym_website}' target='_blank'>{.data$full_name}</a>"),
      .by = 'full_name'
    ) |>
    dplyr::mutate(
      dplyr::across(tidyselect::where(is.logical), \(x) ifelse(x, 'x', ''))
    ) |>
    dplyr::left_join(
      y = .get_board_table(.data),
      by = 'name'
    ) |>
    dplyr::select(
      'Gym Name' = 'full_name',
      'Bouldering' = 'bouldering', 'Top Rope' = 'top_rope', 'Lead' = 'lead',
      'Auto Belay' = 'auto_belay', 'Speed' = 'speed',
      'Treadwall' = 'treadwall',
      'Spray Wall', 'Kilter Board', 'Tension Board', 'MoonBoard',
      'Cardio Machines' = 'cardio_machines', 'Free Weights' = 'free_weights',
      'Weight Machines' = 'weight_machines', 'Yoga Studio' = 'yoga_studio'
    ) |>
    .get_dt_table()
}

.get_dt_table <- function(.data) {
  .data |>
    DT::datatable(
      container = htmltools::withTags(
        shiny::tags$table(
          class = 'display',
          shiny::tags$thead(
            shiny::tags$tr(
              shiny::tags$th(rowspan = 2, 'Gym Name', style = 'vertical-align: bottom;'),
              shiny::tags$th(colspan = 6, 'Climbing', style = 'text-align: center;'),
              shiny::tags$th(colspan = 4, 'Training Boards', style = 'text-align: center;'),
              shiny::tags$th(colspan = 4, 'Fitness', style = 'text-align: center;')
            ),
            shiny::tags$tr(
              lapply(colnames(.data)[-1], shiny::tags$th)
            )
          )
        )
      ),
      escape = FALSE,
      rownames = FALSE,
      extensions = 'Buttons',
      selection = list(
        mode = 'multiple',
        selected = NULL,
        target = 'row'
      ),
      options = list(
        'scrollX' = TRUE,
        'buttons' = 'colvis'
      )
    )
}

.get_board_table <- function(.data) {
  .data |>
    dplyr::select('name', tidyselect::matches('_board_|spray_wall')) |>
    tidyr::pivot_longer(
      cols = -'name',
      names_to = 'board',
      values_to = 'angle'
    ) |>
    dplyr::filter(!is.na(.data$angle)) |>
    dplyr::mutate(
      'board' = .board_lookup[.data$board],
      'angle' = dplyr::case_match(
        .data$angle,
        -2 ~ 'Unknown',
        -1 ~ 'Adjustable',
        .default = as.character(.data$angle)
      )
    ) |>
    dplyr::mutate(
      'brand' = stringr::str_extract(.data$board, '^(\\w+)'),
      'board' = stringr::str_remove_all(.data$board, glue::glue('{.data$brand} ')),
      'brand' = dplyr::case_match(
        .data$brand,
        'Spray' ~ 'Spray Wall',
        'MoonBoard' ~ 'MoonBoard',
        .default = glue::glue('{.data$brand} Board')
      ),
      'board' = glue::glue('{.data$board} @ {.data$angle}\u00B0'),
      .by = c('name', 'board')
    ) |>
    dplyr::select(-'angle') |>
    dplyr::bind_rows(
      tibble::tibble(
        'name' = rep('TEMP_GYM_MUST_DELETE', 4),
        'board' = rep('TEMP_BOARD_MUST_DELETE', 4),
        'brand' = c('Spray Wall', 'Kilter Board', 'Tension Board', 'MoonBoard')
      )
    ) |>
    tidyr::pivot_wider(
      id_cols = 'name',
      names_from = 'brand',
      values_from = 'board',
      values_fn = \(x) shiny::HTML(paste0(x, collapse = '<br><hr>'))
    ) |>
    dplyr::filter(.data$name != 'TEMP_GYM_MUST_DELETE')
}
