rm(list = ls())

library(tidyverse)
library(sf)

# read in the July 2024 LTSB ward collection polygons
#   perform munging as needed
ltsb.wards <- st_read("2024-aug/rep-unit-polygons/WI_Municipal_Wards_(July_2024).geojson") |>
  select(WARD_FIPS, WARDID, ward_label = LABEL, county = CNTY_NAME, ctv = CTV,
         municipality = MCD_NAME) |>
  mutate(municipality = str_to_upper(municipality),
         county = str_to_upper(county),
         municipality = if_else(WARD_FIPS == "55009313750001", "GREENLEAF", municipality),
         ctv = if_else(WARD_FIPS == "55009313750001", "V", ctv),
         geometry_type = st_geometry_type(geometry))

ltsb.2 <- ltsb.wards |>
  st_make_valid() |>
  filter(!st_is_empty(geometry)) |>
  mutate(geometry_type = st_geometry_type(geometry))

ltsb.3 <- ltsb.2 |>
  group_by(WARD_FIPS, WARDID, ward_label, county, ctv, municipality) |>
  summarise() |>
  mutate(geometry_type = st_geometry_type(geometry))

################################################################################
# substitute the ward polygons for counties where I've obtained a more recent GIS file
kenosha <- st_read("2024-aug/rep-unit-polygons/kenosha") |>
  mutate(county = "KENOSHA",
         municipality = str_to_upper(word(FULLNAME, 3, -1)),
         ctv = str_sub(FULLNAME, 1, 1),
         WARDID = str_pad(Ward, side = "left", pad = "0", width = 4),
         WARD_FIPS = paste0(CountyFp, CountySubF, WARDID),
         ward_label = paste(municipality, "-", ctv, WARDID),
         geometry_type = st_geometry_type(geometry)) |>
  select(WARD_FIPS, WARDID, ward_label, county, ctv, municipality, geometry_type) |>
  st_transform(crs = st_crs(ltsb.wards))

kenosha.2 <- kenosha |>
  group_by(WARD_FIPS, WARDID, ward_label, county, ctv, municipality) |>
  summarise(geometry = st_union(geometry, is_coverage = TRUE)) |>
  ungroup() |>
  mutate(geometry_type = st_geometry_type(geometry))

################################################################################
final.wards <- ltsb.3 |>
  filter(county != "KENOSHA") |>
  bind_rows(kenosha.2) |>
  st_make_valid() |>
  group_by(WARD_FIPS, WARDID, ward_label, county, ctv, municipality) |>
  filter(row_number() == 1) |>
  ungroup()

st_write(final.wards, "2024-aug/rep-unit-polygons/wards.geojson",
         delete_dsn = T)

final.wards |>
  leaflet() |>
  addTiles() |>
  addPolygons(label = ~ward_label)

