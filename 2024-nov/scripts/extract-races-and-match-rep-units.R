rm(list = ls())

library(tidyverse)
library(sf)

all.raw <- read_csv("2024-nov/processed/all-raw-processed.csv")
complete.polygons <- st_read("2024-nov/processed/all-polygons-with-match-status.geojson")
matched.polygons <- complete.polygons |> filter(!is.na(reporting_unit))

uss.contest.names <- all.raw |>
  filter(str_detect(candidate, "HOVDE")) |>
  group_by(reporting_unit, contest) |>
  summarise()
pre.contest.names <- all.raw |>
  filter(str_detect(candidate, "TRUMP")) |>
  group_by(reporting_unit, contest) |>
  summarise()

uss.votes <- all.raw |>
  inner_join(uss.contest.names) |>
  mutate(candidate2 = case_when(
    str_detect(candidate, "BALDWIN") ~ "USSDEM24",
    str_detect(candidate, "HOVDE") ~ "USSREP24",
    TRUE ~ "other"
  )) |>
  group_by(county, ctv, municipality, reporting_unit, contest) |>
  mutate(USSTOT24 = sum(votes)) |>
  ungroup() |>
  filter(candidate2 != "other") |>
  select(-c(candidate, contest)) |>
  pivot_wider(names_from = candidate2, values_from = votes)

pre.votes <- all.raw |>
  inner_join(pre.contest.names) |>
  mutate(candidate2 = case_when(
    str_detect(candidate, "HARRIS") ~ "PREDEM24",
    str_detect(candidate, "TRUMP") ~ "PREREP24",
    TRUE ~ "other"
  )) |>
  group_by(county, ctv, municipality, reporting_unit, contest) |>
  mutate(PRETOT24 = sum(votes)) |>
  ungroup() |>
  filter(candidate2 != "other") |>
  select(-c(candidate, contest)) |>
  pivot_wider(names_from = candidate2, values_from = votes)

combined.results <- uss.votes |>
  inner_join(pre.votes) |>
  inner_join(matched.polygons) |>
  st_as_sf() |>
  select(-rep_unit_2024) |>
  select(county, ctv, municipality, MCD_FIPS, reporting_unit, ends_with("24"),
         ends_with("22"), ends_with("20"), ends_with("18"), ends_with("16"),
         ends_with("14"), ends_with("12"))
nrow(combined.results) == nrow(matched.polygons)

st_write(combined.results, "2024-nov/processed/matched-reporting-unit-results.geojson",
         delete_dsn = T)

# complete rep units results, with NA for rep units that do not yet have 2024 data
full.combined.results <- uss.votes |>
  inner_join(pre.votes) |>
  full_join(complete.polygons) |>
  st_as_sf() |>
  select(county, ctv, municipality, MCD_FIPS, reporting_unit, ends_with("24"),
         ends_with("22"), ends_with("20"), ends_with("18"), ends_with("16"),
         ends_with("14"), ends_with("12"))
st_write(full.combined.results, "2024-nov/processed/reporting-unit-results-all.geojson",
         delete_dsn = T)
