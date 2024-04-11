googlesheets4::gs4_deauth()
googlesheets4::read_sheet('1h1hfs-GXo9-9CnAKM25Rd3aagBQcE4pkzDRu9F0_fHA', sheet = 'Validated') |>
  dplyr::mutate(
    dplyr::across(
      tidyselect::where(is.list),
      \(x) sapply(x, \(y) ifelse(is.null(y) | isTRUE(y == 'NULL'), NA, y))
    )
  ) |>
  readr::write_csv('data-raw/data.csv')
