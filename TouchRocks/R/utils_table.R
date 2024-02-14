.create_table <- function(.data) {
  .data |>
    dplyr::mutate(
      'full_name' = glue::glue("<a href='{.data$gym_website}' target='_blank'>{.data$full_name}</a>"),
      .by = 'full_name'
    ) |>
    dplyr::mutate(
      dplyr::across(tidyselect::where(is.logical), \(x) ifelse(x, 'x', ''))
    ) |>
    dplyr::select(
      'Name' = 'full_name', 'Bouldering' = 'bouldering', 'Top Rope' = 'top_rope',
      'Lead' = 'lead', 'Auto Belay' = 'auto_belay'
    ) |>
    DT::datatable(
      escape = FALSE,
      selection = list(
        mode = 'multiple',
        selected = NULL,
        target = 'row'
      ),
      options = list(
        'scrollX' = TRUE
      )
    )
}
