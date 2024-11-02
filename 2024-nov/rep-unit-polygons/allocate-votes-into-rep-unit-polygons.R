rm(list = ls())

library(tidyverse)
library(sf)

ltsb.2020 <- st_read("~/Dropbox/Projects/2024/January/wi-legis-map-proposals-2024/election-data/2012-2020/2012-2020_Election_Data_with_2020_Wards/2012-2020_Election_Data_with_2020_Wards.shp") |>
  rename(ltsb_ward_fips = GEOID)
rep.units.2024 <- st_read("2024-aug/rep-unit-polygons/rep-units.geojson") |>
  st_transform(crs = st_crs(ltsb.2020)) |>
  mutate(rep_unit_2024 = paste(county, rep_unit, sep = " - "))
rep.units.2022 <- st_read("~/Dropbox/Projects/2024/January/wi-legis-map-proposals-2024/election-data/ReportingUnitPolygons.geojson") |>
  st_transform(crs = st_crs(ltsb.2020)) |>
  mutate(rep_unit_2022 = paste(CNTY_NAME, rep_unit, sep = " - "))

l2 <- read_csv("~/Dropbox/Projects/2023/March/L2/All_WI_for-crosswalks_2024-10-02.csv")

glimpse(l2)

l2.addresses <- l2 |>
  filter(!is.na(Residence_Addresses_Latitude)) |>
  group_by(Residence_Addresses_Latitude, Residence_Addresses_Longitude) |>
  summarise(l2_count = n(), .groups = "drop") |>
  st_as_sf(coords = c("Residence_Addresses_Longitude",
                      "Residence_Addresses_Latitude"),
           crs = 4326) |>
  st_transform(crs = st_crs(ltsb.2020))

l2.points.in.polygons <- l2.addresses |>
  st_join(rep.units.2024 |> select(rep_unit_2024)) |>
  st_join(rep.units.2022 |> select(rep_unit_2022)) |>
  st_join(ltsb.2020 |> select(ltsb_ward_fips))

# allocate 2022 reporting units into 2024 reporting units
cw.2022.to.2024 <- l2.points.in.polygons |>
  st_drop_geometry() |>
  filter(!is.na(rep_unit_2022),
         !is.na(rep_unit_2024)) |>
  group_by(rep_unit_2022, rep_unit_2024) |>
  summarise(l2_count = sum(l2_count)) |>
  group_by(rep_unit_2022) |>
  mutate(prop_of_rep_unit_2022 = l2_count/sum(l2_count)) |>
  ungroup()

# allocate 2020 wards into 2024 reporting units
cw.2020.to.2024 <- l2.points.in.polygons |>
  st_drop_geometry() |>
  filter(!is.na(ltsb_ward_fips),
         !is.na(rep_unit_2024)) |>
  group_by(ltsb_ward_fips, rep_unit_2024) |>
  summarise(l2_count = sum(l2_count)) |>
  group_by(ltsb_ward_fips) |>
  mutate(prop_of_ward_2020 = l2_count/sum(l2_count)) |>
  ungroup()

# assign unallocated 2022 reporting units to 2024 units by primary physical overlap
extra.2022 <- rep.units.2022 |>
  filter(! rep_unit_2022 %in% cw.2022.to.2024$rep_unit_2022) |>
  select(rep_unit_2022) |>
  mutate(rep_unit_2022_area = st_area(geometry)) |>
  st_intersection(rep.units.2024 |> select(rep_unit_2024)) |>
  mutate(int_area = st_area(geometry),
         prop = as.numeric(int_area)/as.numeric(rep_unit_2022_area)) |>
  st_drop_geometry() |>
  group_by(rep_unit_2022) |>
  slice_max(order_by = prop, n = 1, with_ties = F) |>
  ungroup()

extra.2020 <- ltsb.2020 |>
  filter(! ltsb_ward_fips %in% cw.2020.to.2024$ltsb_ward_fips) |>
  select(ltsb_ward_fips) |>
  mutate(ltsb_ward_fips_area = st_area(geometry)) |>
  st_intersection(rep.units.2024 |> select(rep_unit_2024)) |>
  mutate(int_area = st_area(geometry),
         prop = as.numeric(int_area)/as.numeric(ltsb_ward_fips_area)) |>
  st_drop_geometry() |>
  group_by(ltsb_ward_fips_area) |>
  slice_max(order_by = prop, n = 1, with_ties = F) |>
  ungroup()

final.cw.2022 <- extra.2022 |>
  select(rep_unit_2022, rep_unit_2024) |>
  mutate(prop_of_rep_unit_2022 = 1) |>
  bind_rows(cw.2022.to.2024)

final.cw.2020 <- extra.2020 |>
  select(ltsb_ward_fips, rep_unit_2024) |>
  mutate(prop_of_ward_2020 = 1) |>
  bind_rows(cw.2020.to.2024)

allocated.2022 <- rep.units.2022 |>
  st_drop_geometry() |>
  tibble() |>
  select(rep_unit_2022, starts_with("PRE"), starts_with("GOV"), starts_with("USS")) |>
  pivot_longer(cols = -rep_unit_2022, names_to = "contest", values_to = "votes") |>
  inner_join(final.cw.2022, relationship = "many-to-many") |>
  mutate(allocated_vote = votes*prop_of_rep_unit_2022) |>
  group_by(rep_unit_2024, contest) |>
  summarise(votes = sum(allocated_vote),
            .groups = "drop") |>
  pivot_wider(names_from = contest, values_from = votes)

# test that totals match
inner_join(
  allocated.2022 |>
    summarise(across(where(is.numeric), sum)) |>
    pivot_longer(cols = everything(), names_to = "contest", values_to = "allocated"),
  rep.units.2022 |>
    st_drop_geometry() |>
    summarise(across(where(is.numeric), sum)) |>
    pivot_longer(cols = everything(), names_to = "contest", values_to = "original")
) |>
  mutate(match = allocated == original)

allocated.2020 <- ltsb.2020 |>
  st_drop_geometry() |>
  tibble() |>
  select(ltsb_ward_fips, starts_with("PRE"), starts_with("GOV"), starts_with("USS")) |>
  pivot_longer(cols = -ltsb_ward_fips, names_to = "contest", values_to = "votes") |>
  inner_join(final.cw.2020, relationship = "many-to-many") |>
  mutate(allocated_vote = votes*prop_of_ward_2020) |>
  group_by(rep_unit_2024, contest) |>
  summarise(votes = sum(allocated_vote, na.rm = T),
            .groups = "drop") |>
  filter(str_sub(contest, 4, -3) %in% c("DEM", "REP", "TOT")) |>
  pivot_wider(names_from = contest, values_from = votes) |>
  select(rep_unit_2024, ends_with("20"), ends_with("18"), ends_with("16"),
         ends_with("14"), ends_with("12"))

# test that totals match
inner_join(
  allocated.2020 |>
    summarise(across(where(is.numeric), ~sum(.x, na.rm = T))) |>
    pivot_longer(cols = everything(), names_to = "contest", values_to = "allocated"),
  ltsb.2020 |>
    st_drop_geometry() |>
    summarise(across(where(is.numeric), ~sum(.x, na.rm = T))) |>
    pivot_longer(cols = everything(), names_to = "contest", values_to = "original")
) |>
  mutate(match = allocated == original) |> print(n = 27)

################################################################################
all.votes.in.2024.rep.units <- rep.units.2024 |>
  left_join(allocated.2022) |>
  left_join(allocated.2020) |>
  mutate(across(.cols = where(is.numeric),
                .fn = ~replace(.x, is.na(.x), 0))) |>
  st_as_sf()
st_write(all.votes.in.2024.rep.units, "2024-aug/rep-unit-polygons/rep-unit-polygons-with-votes-2012-2022.geojson")
write_csv(st_drop_geometry(all.votes.in.2024.rep.units), "2024-aug/rep-unit-polygons/rep-units-2024-with-votes-2012-2022.csv")

################################################################################
# allocate 2024 reporting units into census tracts
tracts <- tidycensus::get_acs("tract", variables = "B01003_001", year = 2022,
                              state = "WI", geometry = TRUE, cb = F) |>
  select(tract_GEOID = GEOID, pop = estimate) |>
  st_transform(crs = st_crs(l2.addresses))
l2.points.in.tracts <- l2 |>
  filter(!is.na(Residence_Addresses_Latitude),
         Voters_Active == "A") |> # current RV only
  group_by(Residence_Addresses_Latitude, Residence_Addresses_Longitude) |>
  summarise(l2_count = n(), .groups = "drop") |>
  st_as_sf(coords = c("Residence_Addresses_Longitude",
                      "Residence_Addresses_Latitude"),
           crs = 4326) |>
  st_transform(crs = st_crs(ltsb.2020)) |>
  st_join(tracts) |>
  st_join(rep.units.2024)

cw.2024.to.tracts <- l2.points.in.tracts |>
  st_drop_geometry() |>
  filter(!is.na(tract_GEOID),
         !is.na(rep_unit_2024)) |>
  group_by(rep_unit_2024, tract_GEOID) |>
  summarise(l2_count = sum(l2_count)) |>
  group_by(rep_unit_2024) |>
  mutate(prop_of_rep_unit_2024 = l2_count/sum(l2_count)) |>
  ungroup()

write_csv(cw.2024.to.tracts, "2024-aug/rep-unit-polygons/rep-units-2024_to_tracts-cw.csv")

weighted.mean(cw.2024.to.tracts$prop_of_rep_unit_2024, w = cw.2024.to.tracts$l2_count)
Hmisc::wtd.quantile(cw.2024.to.tracts$prop_of_rep_unit_2024, weights = cw.2024.to.tracts$l2_count, probs = 0.5)

tracts |>
  st_transform(crs = 4326) |>
  leaflet() |>
  addTiles() |>
  addPolygons(label = ~pop)
