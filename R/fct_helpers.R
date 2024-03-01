.meters_to_feet <- function(x) { (x * 3.28084) }
.feet_to_meters <- function(x) { (x * .3048) }

.get_color_scale <- function(x, height_range) {
  sort_tab <-
    x |>
    table() |>
    sort(decreasing = TRUE)

  x_0 <- c(sort_tab[sort_tab > 0], '0' = 0)
  col <-
    x_0 |>
    unique() |>
    length() |>
    grDevices::heat.colors() |>
    `names<-`(unique(x_0))
  col <-
    col[as.character(x_0)] |>
    `names<-`(names(x_0))

  full_range <- seq(utils::head(height_range, 1), utils::tail(height_range, 1))
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
