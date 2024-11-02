rm(list = ls())

library(tidyverse)
library(sf)
source("2024-aug/scripts/download-returns.R")


################################################################################
# the processed raw election results
##  list of valid countynames
county.names <- readRDS("county-names.rds")

processed.files <- map(.x = county.names,
                       .f = ~latest_file(.x, "raw-processed", T)) |> 
  unlist()


all.orig <- map(.x = processed.files,
                .f = ~read_csv(.x, col_types = cols(.default = "c"))) |>
  list_rbind() |>
  type_convert() |>
  # munge municipality names
  mutate(municipality = str_remove_all(municipality, coll(".")),
         municipality = case_when(
           municipality == "MT CALVARY" ~ "MOUNT CALVARY",
           municipality == "LAGRANGE" ~ "LA GRANGE",
           municipality == "FONTANA-ON- GENEVA LAKE" ~ "FONTANA-ON-GENEVA LAKE",
           municipality == "SAINT LAWRENCE" ~ "ST LAWRENCE",
           municipality == "WISC RAPIDS" ~ "WISCONSIN RAPIDS",
           municipality == "SO MILW" ~ "SOUTH MILWAUKEE",
           municipality == "MT PLEASANT" ~ "MOUNT PLEASANT",
           TRUE ~ municipality
         )) |>
  mutate(
    reporting_unit = case_when(
      reporting_unit == "T BLOOMFIELD W 1 & 2" ~ "T BLOOMFIELD", # Waushara reports this inconsistently across tables
      TRUE ~ reporting_unit),
    county = case_when(
      reporting_unit == "CITY OF MILWAUKEE WARD 316" ~ "WASHINGTON",
      reporting_unit == "CITY OF MILWAUKEE WARD 317" ~ "WAUKESHA",
      TRUE ~ county
    )) |>
  filter(municipality != "CALEDONIA (VOID W6-7)") # seems like a row that should be dropped
################################################################################

################################################################################
# US Senate
us.senate.contests <- all.orig |>
  filter(str_detect(candidate, "BALDWIN|HOVDE")) |>
  group_by(county, contest) |>
  summarise(.groups = "drop")

us.senate.contests |> group_by(county) |> filter(n_distinct(contest) != 2)

us.senate.votes <- all.orig |>
  inner_join(us.senate.contests) |>
  mutate(candidate2 = case_when(
    str_detect(candidate, "HOVDE") ~ "hovde",
    str_detect(candidate, "BALDWIN") ~ "baldwin",
    TRUE ~ "other"
  )) |>
  group_by(county, ctv, municipality, reporting_unit, candidate2) |>
  summarise(votes = sum(votes)) |>
  mutate(total_votes = sum(votes)) |>
  ungroup() |>
  pivot_wider(names_from = candidate2, values_from = votes)
write_csv(us.senate.votes, "2024-aug/processed/us-senate.csv")
################################################################################
