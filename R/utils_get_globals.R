.get_kilter_size <- function() { c('(Home) 7x10' = '7x10', '8x12', '12x12', '16x12') }
.get_kilter_set <- function() { '' }

.get_tension1_size <- function() { c('8x10', '8x12') }
.get_tension1_set <- function() { c('Full', 'A', 'B', 'C') }

.get_tension2_size <- function() { c('8x10', '12x10', '8x12', '12x12') }
.get_tension2_set <- function() { c('Spray', 'Mirror') }

.get_moonboard_size <- function() { '' }
.get_moonboard_set <- function() { c('2016', '2017', '2019', '2024', 'Mini 2020' = '2020') }

.get_sictb_identifiers <- function() {
  list(
    'Kilter' = c('brand', 'size'),
    'Tension' = c('brand', 'model', 'size', 'set'),
    'MoonBoard' = c('brand', 'set')
  )
}
