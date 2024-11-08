rm(list = ls())

library(tidyverse)
library(sf)

orig <- sf::st_read("2024-nov/processed/matched-reporting-unit-results.geojson")
my.results <- orig |>
  sf::st_drop_geometry()

################################################################################
# updated at 2:13pm
ap <- read_csv("/Users/johnsonjoh/Downloads/APCountyWSenateV2.csv")
ap2 <- ap |>
  select(-update) |>
  select(county, ap_PREDEM24 = Harris, ap_PREREP24 = Trump,
         ap_USSDEM24 = Baldwin, ap_USSREP24 = Hovde) |>
  mutate(county = str_to_upper(word(county, 1, -2)))

compare <- my.results |>
  select(-rep_unit_2024) |>
  group_by(county) |>
  summarise(across(contains("24"), ~sum(.x, na.rm = T))) |>
  left_join(ap2) |>
  mutate(USSDEMmatch = USSDEM24 == ap_USSDEM24,
         USSREPmatch = USSREP24 == ap_USSREP24,
         PREDEMmatch = PREDEM24 == ap_PREDEM24,
         PREREPmatch = PREREP24 == ap_PREREP24)

# disagreements
##  I'm correct in these 3 places
# Iowa, Trump's total is 6631 not 6571 and Harris' is 7750 not 7730
# Marquette, Hovde's total is 5629 not 5729
# Washington, AP and I disagree by 6 votes. This is because Washington County and Dodge County both report a Dodge county ward of Hartford, a city primarily in Washington County. I remove this from the Washington County total, but AP double counts the ward's 6 voters.

# Shawano - I'm slightly off because shawano isn't reporting Town of Fairbanks in its ward-by-ward file
################################################################################
################################################################################
# create research-ready file
missing.counties <- ap2 |>
  filter(! county %in% my.results$county) |>
  rename(PREDEM24 = ap_PREDEM24, PREREP24 = ap_PREREP24, USSDEM24 = ap_USSDEM24, USSREP24 = ap_USSREP24)
missing.counties

glimpse(my.results)

county.archive <- read_csv("/Users/johnsonjoh/Dropbox/Projects/2023/December/wisc-elections-archive/processed-data/AllElections_Counties.csv")
cnty.shp <- tigris::counties("WI", cb = T)
missing.counties.archive <- county.archive |>
  filter(county %in% str_to_upper(missing.counties$county),
         office %in% c("PRESIDENT", "US SENATE", "GOVERNOR"),
         party %in% c("DEMOCRATIC", "REPUBLICAN"),
         year > 2010) |>
  select(county_fips, county, year, office, party, total_votes, votes) |>
  mutate(party = str_sub(party, 1, 3),
         year = str_sub(year, 3, 4),
         office = case_when(
           office == "PRESIDENT" ~ "PRE",
           office == "US SENATE" ~ "USS",
           office == "GOVERNOR" ~ "GOV"),
         label = paste0(office, party, year)) |>
  select(county_fips, county, label, votes) |>
  mutate(county_fips = as.character(county_fips)) |>
  pivot_wider(names_from = label, values_from = votes) |>
  inner_join(missing.counties) |>
  inner_join(cnty.shp |>
               select(county_fips = GEOID) |>
               st_transform(crs = st_crs(orig))) |>
  st_as_sf() |>
  mutate(level = "county")

################################################################################

final.output <- orig |>
  mutate(level = "reporting unit",
         county_fips = str_sub(MCD_FIPS, 1, 5)) |>
  bind_rows(missing.counties.archive) |>
  mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, .x))) |>
  select(level, everything())

sum(final.output$PREDEM24)
sum(final.output$PREREP24)
sum(final.output$PREREP24) - sum(final.output$PREDEM24)
sum(final.output$PREREP20) - sum(final.output$PREDEM20)
sum(final.output$PREREP16) - sum(final.output$PREDEM16)

final.output.simple <- final.output |>
  rmapshaper::ms_simplify(keep_shapes = T) |>
  st_transform(crs = 4326) |>
  mutate(label = paste(county, "County:", if_else(is.na(reporting_unit), "", reporting_unit))) |>
  select(level, county, county_fips, ctv, municipality, MCD_FIPS, reporting_unit,
         rep_unit_2024, ends_with("24"), ends_with("22"), ends_with("20"),
         ends_with("18"), ends_with("16"), ends_with("14"), ends_with("12"))

st_write(final.output.simple, "2024-nov/analysis/election-results-2012-2024_with-all-party-totals.geojson",
         delete_dsn = T)
final.output.simple |>
  st_drop_geometry() |>
  write_csv("2024-nov/analysis/election-results-2012-2024_with-all-party-totals.csv")
################################################################################