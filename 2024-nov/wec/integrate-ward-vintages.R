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
kenosha <- st_read("2024-nov/rep-unit-polygons/Kenosha") |>
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
  mutate(geometry_type = st_geometry_type(geometry)) |>
  st_transform(crs = st_crs(ltsb.3))

################################################################################
# the LTBS file has Wauwatosa wards 8a and 8b as identical, when in reality they are different
# here is the correct wauwatosa ward 8a
wauwatosa.ward.8a <- st_read("2024-aug/rep-unit-polygons/wauwatosa-ward-8a.geojson") |>
  st_transform(crs = st_crs(ltsb.wards)) |>
  mutate(WARD_FIPS = "5507984675008A",
         WARDID = "008A",
         ward_label = "Wauwatosa - C 008A",
         county = "MILWAUKEE",
         ctv = "C",
         municipality = "WAUWATOSA")

################################################################################
# the NEW november vintage Dane County wards
#   The City of Madison has added ward 135
dane.wards <- st_read("2024-nov/rep-unit-polygons/Dane") |>
  mutate(MCD_FIPS = paste0("55025", FIPSCODE),
         ctv = str_sub(NAME, 1, 1),
         municipality = word(NAME, 3, -1),
         county = "DANE") |>
  select(county, ctv, municipality, MCD_FIPS, WardNumber) |>
  mutate(WARDID = str_pad(WardNumber, width = 4, side = "left", pad = "0"),
         WARD_FIPS = paste0(MCD_FIPS, WARDID),
         ward_label = paste(municipality, "-", ctv, WARDID)) |>
  st_transform(crs = st_crs(ltsb.3)) |>
  group_by(WARD_FIPS, WARDID, ward_label, county, ctv, municipality) |>
  summarise(.groups = "drop")

################################################################################
# The City of Oshkosh added some wards, thanks to annexations
winnebago <- st_read("2024-nov/rep-unit-polygons/Winnebago/WinnebagoCountyWards_11_05_24.shp") |>
  mutate(MCD_FIPS = paste0("55139", COUSUBFP),
         ctv = str_sub(MUNICIPALI, 1, 1),
         municipality = word(MUNICIPALI, 3, -1),
         county = "WINNEBAGO",
         WardNumber = str_squish(WARDID)) |>
  group_by(county, ctv, municipality, MCD_FIPS, WardNumber) |>
  summarise(.groups = "drop") |>
  st_make_valid() |>
  mutate(WARDID = str_pad(WardNumber, width = 4, side = "left", pad = "0"),
         WARD_FIPS = paste0(MCD_FIPS, WARDID),
         ward_label = paste(municipality, "-", ctv, WARDID)) |>
  select(WARD_FIPS, WARDID, ward_label, county, ctv, municipality) |>
  st_transform(crs = st_crs(ltsb.3))

################################################################################
final.wards <- ltsb.3 |>
  # replace counties with updated wards
  filter(! county %in% c("KENOSHA","DANE","WINNEBAGO")) |>
  bind_rows(kenosha.2) |>
  bind_rows(dane.wards) |>
  bind_rows(winnebago) |>
  st_make_valid() |>
  # add wauwatosa ward 8a
  filter(WARD_FIPS != "5507984675008A") |>
  rmapshaper::ms_erase(erase = wauwatosa.ward.8a, remove_slivers = T) |>
  bind_rows(wauwatosa.ward.8a) |>
  group_by(WARD_FIPS, WARDID, ward_label, county, ctv, municipality) |>
  filter(row_number() == 1) |>
  ungroup() |>
  mutate(across(where(is.character), str_to_upper),
         across(where(is.character), str_squish))

# remove overlapping sections of wards
#   the ward that keeps a contested section is the first in order of appearance
#   this is basically arbitrary
final.wards.no.overlaps <- st_difference(final.wards) |>
  st_make_valid()

# Brown Deer ward 7 gets dropped entirely because it completely overlaps with ward 4
#   i'm ok with this because the election results are for a combined ward 4 and 7 reporting unit
#    and the brown deer village map doesn't actually show a ward 7 existing
anti_join(final.wards, st_drop_geometry(final.wards.no.overlaps))

st_write(final.wards.no.overlaps, "2024-nov/wec/wards.geojson",
         delete_dsn = T)
