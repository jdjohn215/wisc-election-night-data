rm(list = ls())

library(tidyverse)
library(sf)

################################################################################
# these are the reporting unit polygons
#   derived from WEC and LTSB sources
rep.units.shp <- st_read("2024-nov/wec/pres-gov-senate_2024-rep-units_1990-2024.geojson") |>
  select(reporting_unit, county, ctv, municipality, MCD_FIPS) |>
  tibble() |>
  st_as_sf() |>
  st_make_valid()

block.shp <- st_read("~/Dropbox/Projects/2024/January/wi-legis-map-proposals-2024/census-blocks/WI_BLOCKS_2020_TIGER_PL94171.geojson")
blocks.to.rep.units <- block.shp |>
  mutate(geometry = st_centroid(geometry)) |>
  st_transform(crs = st_crs(rep.units.shp)) |>
  st_join(rep.units.shp)

block.demog <- read_csv("~/Dropbox/Projects/2024/January/wi-legis-map-proposals-2024/census-blocks/wi-blocks-simple.csv") |>
  mutate(GEOID = as.character(GEOID)) |>
  select(GEOID, starts_with("pop"), starts_with("vap"))

rep.unit.demog <- blocks.to.rep.units |>
  st_drop_geometry() |>
  inner_join(block.demog) |>
  group_by(county, ctv, municipality, MCD_FIPS, reporting_unit) |>
  summarise(across(.cols = where(is.numeric), .fn = sum),
            .groups = "drop")

# some reporting units don't have a 2020 census pop
surface.water <- st_read("~/Dropbox/Projects/SHPfiles/24k_Hydro_Waterbodies_(Open_Water).geojson") |>
  st_transform(crs = st_crs(rep.units.shp))

landarea <- left_join(rep.units.shp, rep.unit.demog) |>
  # erase open waterbodies, so as to calculate population density
  rmapshaper::ms_erase(surface.water) |>
  st_make_valid() |>
  mutate(land_area = st_area(geometry),
         sq_mi = as.numeric(land_area)/27878288.487,
         pop_per_sq_mi = pop/sq_mi) |>
  st_drop_geometry() |>
  select(county, reporting_unit, pop_per_sq_mi)

rep.unit.demog.2 <- left_join(rep.units.shp, rep.unit.demog) |>
  st_drop_geometry() |>
  left_join(landarea)

write_csv(rep.unit.demog.2, "2024-nov/wec/reporting-unit-demographics-2020census.csv")

rep.unit.demog.2 |>
  select(county, reporting_unit, pop_per_sq_mi, starts_with("pop"), starts_with("vap")) |>
  write_csv("2024-nov/wec/rep-units-with-2020-census-block-stats.csv")
