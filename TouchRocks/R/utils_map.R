.get_bounds <- function(.data) {
  if (nrow(.data) == 0) return(c(-180, -90, 180, 90))
  c(
    'lng1' = min(.data$lon),
    'lat1' = min(.data$lat),
    'lng2' = max(.data$lon),
    'lat2' = max(.data$lat)
  ) |>
    unname()
}

.create_map <- function(.data) {
  .data |>
    leaflet::leaflet() |>
    leaflet::addTiles(
      group = 'Street Map'
    ) |>
    .add_markers_and_fit(.get_bounds(.data))
}

.add_markers_and_fit <- function(map, b) {
  map |>
    leaflet::clearMarkers() |>
    leaflet::clearPopups() |>
    leaflet::addMarkers(
      lng = ~ lon,
      lat = ~ lat,
      popup = ~ full_name
    ) |>
    leaflet::fitBounds(
      b[1], b[2], b[3], b[4],
      options = list('maxZoom' = 12, 'padding' = c(15, 15))
    )
}
