.meters_to_feet <- function(x) { (x * 3.28084) }
.feet_to_meters <- function(x) { (x * .3048) }

.drop_na <- function(x) { x[!is.na(x)] }

.check_range <- function(x, min_max) {
  !is.na(x) & dplyr::between(x, min_max[1], min_max[2])
}

.get_tag_link <- function(link, text) {
  shiny::tags$a(
    href = link,
    text,
    shiny::tags$sup(htmltools::img(
      src = 'www/images/box-arrow-up-right.svg', width = '1.75%'
    )),
    target = '_blank',
    style = 'color: inherit;'
  )
}

.get_color_scale <- function(x, height_range) {
  x <- x[dplyr::between(x, min(height_range, na.rm = TRUE), max(height_range, na.rm = TRUE))]
  sort_tab <-
    x |>
    table() |>
    sort(decreasing = TRUE)

  x_0 <- c(sort_tab[sort_tab > 0], 'Inf' = Inf)
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
      tibble::tibble(
        'col' = col[y],
        'pos' = which(full_range == y) / length(full_range) * 100
      )
    }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      'col' = ifelse(is.na(.data$col), .env$col['Inf'], .data$col)
    ) |>
    dplyr::arrange(.data$pos) |>
    apply(1, \(y) { glue::glue("{y['col']} {y['pos']}%")}) |>
    paste0(collapse = ', ')
}
