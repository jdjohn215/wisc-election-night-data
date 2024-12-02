rm(list = ls())

library(tidyverse)
library(sf)

# all races 2024
votes.2024 <- read_csv("2024-nov/wec/wec-original-all-races-long-format.csv")

# reporting unit polygons
rep.unit.shp <- st_read("2024-nov/wec/rep-units.geojson")

# past votes allocated into 2024 reporting units
past.votes <- read_csv("2024-nov/wec/rep-units-2024-with-votes-1990-2022.csv")

# reformat 2024 votes, keeping president and US senate
votes.2024.2 <- votes.2024 |>
  filter(contest %in% c("PRESIDENT OF THE UNITED STATES",
                        "UNITED STATES SENATOR"),
         party %in% c("DEM", "REP")) |>
  select(county, reporting_unit, contest, total, party, votes) |>
  pivot_wider(names_from = party, values_from = votes) |>
  pivot_longer(cols = c(total, DEM, REP), names_to = "party", values_to = "votes") |>
  mutate(party = str_replace(party, "total", "TOT"),
         contest = if_else(contest == "UNITED STATES SENATOR", "USS", "PRE"),
         name = paste0(contest, party, "24")) |>
  select(-c(contest, party)) |>
  pivot_wider(names_from = name, values_from = votes)

all.votes <- past.votes |>
  inner_join(votes.2024.2) |>
  select(-rep_unit_2024) |>
  select(county, ctv, municipality, MCD_FIPS, reporting_unit, ends_with("24"),
         everything()) |>
  mutate(MCD_FIPS = as.character(MCD_FIPS))

# confirm totals match
sum(all.votes$PRETOT24) == sum(votes.2024$votes[votes.2024$contest == "PRESIDENT OF THE UNITED STATES"])
sum(all.votes$USSTOT24) == sum(votes.2024$votes[votes.2024$contest == "UNITED STATES SENATOR"])

all.votes.sf <- rep.unit.shp |>
  inner_join(all.votes) |>
  as_tibble() |>
  st_as_sf() |>
  # simplify
  rmapshaper::ms_simplify(keep_shapes = T)

st_write(all.votes.sf, "2024-nov/wec/pres-gov-senate_2024-rep-units_1990-2024.geojson",
         delete_dsn = T)
st_write(rmapshaper::ms_simplify(rep.unit.shp, keep_shapes = T),
         "2024-nov/wec/rep-units-simplified.geojson",
         delete_dsn = T)
