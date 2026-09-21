library(sf)

la_boundaries <- st_read(
  "data/boundaries/Local_Authority_Boundaries.geojson",
  quiet = TRUE
)

region_boundaries <- st_read(
  "data/boundaries/Region_Boundaries.geojson",
  quiet = TRUE
)



