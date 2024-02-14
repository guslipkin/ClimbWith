#' data
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
.get_data <- function() {
  # googlesheets4::read_sheet('1583wkUTU7XSDyFwZHIfIEhNZYJNz3y2y5VF8wUt7i7g') |> readr::write_csv('data/data.csv')
  readr::read_csv('data/data.csv', show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::matches('spray_wall|board_'), # fix training boards
        \(x) {
          dplyr::case_when(
            x == 'Unknown' ~ '-2',
            x == 'Adjustable' ~ '-1',
            .default = x
          ) |>
            as.integer()
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
      'full_name' = .data$gym_name,
      'name' = janitor::make_clean_names(.data$gym_name),
      .before = 1
    ) |>
    dplyr::select(-'gym_name', -'google_maps_link')
}
