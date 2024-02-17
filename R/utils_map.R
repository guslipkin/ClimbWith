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
    leaflet::addEasyButton(
      leaflet::easyButton(
        position = "topright",
        icon = "fa-crosshairs",
        title = "Locate Me",
        onClick = leaflet::JS(
          "function(btn,  map){
            map.locate({
              setView: true,
              enableHighAccuracy: true,
              maxZoom: 10
            })
          }"
        )
      )
    ) |>
    # leaflet::addEasyButton(
    #   leaflet::easyButton(
    #     position = "topleft",
    #     icon = "fa-maximize",
    #     title = "Fit",
    #     onClick = leaflet::JS(
    #       "function(btn,  map){
    #         map.eachLayer(function(layer) {
    #           if (layer.id === 'markers') {
    #             // map.fitBounds(map.getLayer(layer.id).getBounds());
    #           }
    #         });
    #       }"
    #     )
    #   )
    # ) |>
    .add_markers_and_fit(.data)
}

.add_markers_and_fit <- function(map, .data) {
  b <- .get_bounds(.data)
  labels <-
    .data |>
    dplyr::mutate('.id' = dplyr::row_number()) |>
    dplyr::mutate(
      'label' = shiny::HTML(
        glue::glue('<b>{.data$full_name}</b>'),
        '<br>',
        glue::glue('<a href="tel:{.data$gym_phone_number}">Call</a>'),
        ' | ',
        glue::glue('<a href="{.data$google_maps_link}" target="_blank">Get Directions</a>')
      ),
      .by = '.id'
    ) |>
    dplyr::pull(.data$label)
  map |>
    leaflet::clearMarkers() |>
    leaflet::clearPopups() |>
    leaflet::addMarkers(
      lng = ~ lon,
      lat = ~ lat,
      popup = labels
    )
    # leaflet::fitBounds(
    #   b[1], b[2], b[3], b[4],
    #   options = list('maxZoom' = 12, 'padding' = c(15, 15))
    # )
}
