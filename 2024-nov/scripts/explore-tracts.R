rm(list = ls())

library(tidyverse)
library(sf)
library(leaflet)
library(tidycensus)

acs.vars <- load_variables(2022, "acs5", cache = T)

tract.cw <- read_csv("2024-aug/rep-unit-polygons/rep-units-2024_to_tracts-cw.csv")
rep.units <- st_read("2024-aug/rep-unit-polygons/rep-unit-polygons-with-votes-2012-2022.geojson")

rep.units |>
  mutate(margin_20 = (PREDEM20/PRETOT20 - PREREP20/PRETOT20)*100,
         margin_16 = (PREDEM16/PRETOT16 - PREREP16/PRETOT16)*100) |>
  select(county, ctv, municipality, MCD_FIPS, rep_unit, margin_20, margin_16,
         PRETOT20, PRETOT16)

tract.cw
tract.votes <- rep.units |>
  st_drop_geometry() |>
  pivot_longer(cols = c(starts_with("PRE"), starts_with("GOV"), starts_with("USS")),
               names_to = "contest", values_to = "votes") |>
  inner_join(tract.cw, relationship = "many-to-many") |>
  mutate(allocated_votes = votes*prop_of_rep_unit_2024) |>
  group_by(tract_GEOID, contest) |>
  summarise(votes = sum(allocated_votes), .groups = "drop") |>
  separate(contest, into = c("office","party","year"), sep = c(3,-2), convert = T) |>
  mutate(year = 2000 + year) |>
  pivot_wider(names_from = party, values_from = votes) |>
  mutate(margin = (DEM/TOT - REP/TOT)*100)

tract.educ <- get_acs("tract", variables = c("B15003_001","B15003_022","B15003_023","B15003_024","B15003_025"),
                      year = 2022, state = "WI", survey = "acs5", geometry = T, cb = T) |>
  select(-moe) |>
  pivot_wider(names_from = variable, values_from = estimate) |>
  mutate(pct_baplus = ((B15003_022 + B15003_023 + B15003_024 + B15003_025) / B15003_001)*100) |>
  select(GEOID, B15003_022, pct_baplus, geometry) |>
  mutate(ba_plus_ntile = percent_rank(pct_baplus)) |>
  mutate(educ_quintile = case_when(
    ba_plus_ntile < 0.2 ~ 1,
    ba_plus_ntile < 0.4 ~ 2,
    ba_plus_ntile < 0.6 ~ 3,
    ba_plus_ntile < 0.8 ~ 4,
    TRUE ~ 5
  ))

tract.educ
tract.vote.educ <- tract.votes |>
  select(GEOID = tract_GEOID, office, year, margin) |>
  mutate(GEOID = as.character(GEOID)) |>
  filter(office == "PRE") |>
  pivot_wider(names_from = year, values_from = margin) |>
  mutate(shift_16 = `2016` - `2012`,
         shift_20 = `2020` - `2016`) |>
  inner_join(tract.educ)

tract.vote.educ |>
  ggplot(aes(pct_baplus, shift_20)) +
  geom_point()

tract.vote.educ

educ.quintile.shifts <- tract.votes |>
  rename(GEOID = tract_GEOID) |>
  mutate(GEOID = as.character(GEOID)) |>
  inner_join(tract.educ) |>
  group_by(year, office, educ_quintile) |>
  summarise(DEM = sum(DEM),
            REP = sum(REP),
            TOT = sum(TOT), .groups = "drop") |>
  mutate(margin = (DEM/TOT - REP/TOT)*100) |>
  filter(office == "PRE") |>
  select(year, office, educ_quintile, margin) |>
  pivot_wider(names_from = year, values_from = margin) |>
  mutate(shift_16 = `2016` - `2012`,
         shift_20 = `2020` - `2016`)

educ.quintile.shifts |>
  mutate(educ_quintile = factor(educ_quintile)) |>
  ggplot(aes(educ_quintile, shift_20)) +
  geom_col() +
  labs(title = "Shift in vote margin from 2016 - 2020, by quintile of educational attainment")
