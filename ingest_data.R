raw_dt <- 
  googlesheets4::read_sheet('1583wkUTU7XSDyFwZHIfIEhNZYJNz3y2y5VF8wUt7i7g') |>
  janitor::clean_names()

dt <-
  raw_dt |>
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches('spray_wall|board_'), # fix training boards
      \(x) {
        dplyr::case_when(
          # is.na(x) ~ '-3',
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
    'lat' = as.numeric(stringr::str_match(.data$google_maps_link, '/@(-?\\d+\\.\\d+),')[1,2]),
    'lon' = as.numeric(stringr::str_match(.data$google_maps_link, ',(-?\\d+\\.\\d+),\\d+')[1,2])
  ) |>
  dplyr::mutate(
    'full_name' = .data$gym_name,
    'name' = janitor::make_clean_names(.data$gym_name),
    .before = 1
  ) |>
  dplyr::select(-'gym_name', -'google_maps_link')

gymLink <-
  dt |>
  dplyr::select('name', 'link' = 'gym_website')

dt |>
  dplyr::select('name', tidyselect::matches('_board'))

dt |>
  purrr::imap_chr(\(x, idx) {
    if (is.character(x)) {
      x <- glue::glue("'{x}'") 
    } else if (is.logical(x)) {
      x <- tolower(as.character(x))
    }
    x <- ifelse(is.na(x), "", x)
    glue::glue("'{idx}': \t[{paste0(x, collapse = ',\t')}]", .sep = ',')
  }) |>
  paste0(collapse = ',\n') |>
  cat()
