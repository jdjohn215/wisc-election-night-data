rm(list = ls())

library(tidyverse)
library(sf)
library(leaflet)
library(gt)

matched.rep.units <- st_read("2024-aug/processed/matched-polygons.geojson")

old.results <- st_read("2024-aug/rep-unit-polygons/rep-unit-polygons-with-votes-2012-2022.geojson")
matched.rep.units |>
  st_transform(crs = 4326) |>
  leaflet() |>
  addProviderTiles(provider = providers$CartoDB.Positron) |>
  addPolygons(label = ~reporting_unit)

# old.results |>
#   st_transform(crs = 4326) |>
#   leaflet() |>
#   addProviderTiles(provider = providers$CartoDB.Positron) |>
#   addPolygons(label = ~rep_unit)

mke.df <- old.results |>
  tibble() |>
  filter(municipality == "MILWAUKEE") |>
  mutate(vmargin20 = PREDEM20 - PREREP20,
         vmargin16 = PREDEM16 - PREREP16,
         marginshift = vmargin20-vmargin16) |>
  select(rep_unit, contains("margin"))

mke.df |>
  ggplot(aes(vmargin20, marginshift)) +
  geom_point()

old.results |>
  filter(rep_unit == "CITY OF MILWAUKEE WARD 173") |>
  st_transform(crs = 4326) |>
  leaflet() |>
  addTiles() |>
  addPolygons()

old.results |>
  filter(rep_unit == "CITY OF MILWAUKEE WARD 173") |> glimpse()
old.results |>
  tibble() |>
  filter(municipality == "WAUKESHA",
         ctv == "C") |>
  mutate(vmargin20 = PREDEM20 - PREREP20,
         vmargin16 = PREDEM16 - PREREP16,
         marginshift = vmargin20-vmargin16) |>
  select(rep_unit, contains("margin")) |>
  ggplot(aes(vmargin20, marginshift)) +
  geom_point()
