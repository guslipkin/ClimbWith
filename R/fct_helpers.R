.meters_to_feet <- function(x) { (x * 3.28084) }
.feet_to_meters <- function(x) { (x * .3048) }

.get_color_scale <- function(x, height_range) {
  sort_tab <-
    x |>
    table() |>
    sort(decreasing = TRUE)

  col <-
    c(sort_tab[sort_tab > 0], '0' = 0) |>
    length() |>
    grDevices::heat.colors() |>
    `names<-`(c(names(sort_tab[sort_tab > 0]), '0'))

  full_range <- seq(head(height_range, 1), tail(height_range, 1))
  sort_tab |>
    names() |>
    append(full_range[!(full_range %in% x)]) |>
    lapply(\(y) {
      pos <- round(which(full_range == y) / length(full_range) * 100, 0)
      tibble::tibble('col' = col[y], 'pos' = pos)
    }) |>
    dplyr::bind_rows() |>
    dplyr::mutate('col' = ifelse(is.na(.data$col), .env$col['0'], .data$col)) |>
    dplyr::arrange(.data$pos) |>
    apply(1, \(y) { glue::glue("{y['col']} {y['pos']}%")}) |>
    paste0(collapse = ', ')
}
