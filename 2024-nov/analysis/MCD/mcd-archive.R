rm(list = ls())

library(tidyverse)

orig.votes <- sf::st_read("2024-nov/analysis/election-results-2012-2024_with-all-party-totals.geojson") |>
  sf::st_drop_geometry()
rep.unit.votes <- orig.votes |>
  filter(level == "reporting unit") |>
  mutate(PREMARG24 = (PREDEM24/PRETOT24 - PREREP24/PRETOT24)*100,
         PREMARG20 = (PREDEM20/PRETOT20 - PREREP20/PRETOT20)*100,
         PREMARG16 = (PREDEM16/PRETOT16 - PREREP16/PRETOT16)*100,
         USSMARG18 = (USSDEM18/USSTOT18 - USSREP18/USSTOT18)*100,
         USSMARG22 = (USSDEM22/USSTOT22 - USSREP22/USSTOT22)*100,
         USSMARG24 = (USSDEM24/USSTOT24 - USSREP24/USSTOT24)*100) |>
  mutate(PREMARGINSHIFT24 = PREMARG24 - PREMARG20)

mcd.votes <- rep.unit.votes |>
  group_by(MCD_FIPS, county, ctv, municipality) |>
  summarise(across(where(is.numeric), sum), .groups = "drop") |>
  mutate(PREMARG24 = (PREDEM24/PRETOT24 - PREREP24/PRETOT24)*100,
         PREMARG20 = (PREDEM20/PRETOT20 - PREREP20/PRETOT20)*100,
         PREMARG16 = (PREDEM16/PRETOT16 - PREREP16/PRETOT16)*100,
         USSMARG18 = (USSDEM18/USSTOT18 - USSREP18/USSTOT18)*100,
         USSMARG22 = (USSDEM22/USSTOT22 - USSREP22/USSTOT22)*100,
         USSMARG24 = (USSDEM24/USSTOT24 - USSREP24/USSTOT24)*100)

mcd.df <- mcd.votes |>
  mutate(marg24 = PREDEM24 - PREREP24,
         marg20 = PREDEM20 - PREREP20,
         marg16 = PREDEM16 - PREREP16) |>
  select(MCD_FIPS, municipality, marg24, marg20, marg16, PRETOT24, PRETOT20, PRETOT16)


mcd.archive <- read_csv("/Users/johnsonjoh/Dropbox/Projects/2023/December/wisc-elections-archive/processed-data/AllElections_MinorCivilDivisions.csv")
mcd.archive
mcd.votes.2024 <- mcd.votes |>
  select(mcd_fips = MCD_FIPS, county, ctv, municipality, ends_with("24")) |>
  select(-contains("MARG")) |>
  pivot_longer(cols = ends_with("24"), values_to = "votes") |>
  separate("name", into = c("office","party","year"), sep = c(3,-2)) |>
  mutate(office = case_when(office == "PRE" ~ "PRESIDENT",
                            office == "USS" ~ "US SENATE"),
         year = as.numeric(paste0("20", year)),
         county_fips = str_sub(mcd_fips, 1, 5)) |>
  pivot_wider(names_from = party, values_from = votes) |>
  rename(total_votes = TOT) |>
  pivot_longer(cols = c(DEM, REP), names_to = "party", values_to = "votes") |>
  mutate(party = if_else(party == "DEM", "DEMOCRATIC", "REPUBLICAN")) |>
  filter(!is.na(mcd_fips))
glimpse(mcd.archive)

mcd.archive.complete <- mcd.archive |>
  filter(party %in% c("DEMOCRATIC", "REPUBLICAN"),
         office %in% c("PRESIDENT", "US SENATOR", "GOVERNOR")) |>
  select(county, county_fips, mcd_fips, municipality, ctv, year, office, party, total_votes, votes) |>
  mutate(across(contains("fips"), as.character)) |>
  bind_rows(mcd.votes.2024)
write_csv(mcd.archive.complete, "2024-nov/analysis/MCD/mcd-PresGovSen-2000-2024_NominalIntegration.csv")
# these 3 incorporated since 2022
mcd.votes.2024 |> filter(! mcd_fips %in% mcd.archive$mcd_fips) |> group_by(mcd_fips, county, ctv, municipality) |> summarise()


################################################################################
# municipality integration
muni.archive <- read_csv("/Users/johnsonjoh/Dropbox/Projects/2023/December/wisc-elections-archive/processed-data/AllElections_Municipalities.csv")

muni.votes <- rep.unit.votes |>
  mutate(muni_fips = paste0("55", str_sub(MCD_FIPS, -5, -1))) |>
  group_by(muni_fips, ctv, municipality) |>
  summarise(across(where(is.numeric), sum), .groups = "drop") |>
  select(-contains("MARG"))
muni.votes.2024 <- muni.votes |>
  select(muni_fips, ctv, municipality, ends_with("24")) |>
  pivot_longer(cols = ends_with("24"), values_to = "votes") |>
  separate("name", into = c("office","party","year"), sep = c(3,-2)) |>
  mutate(office = case_when(office == "PRE" ~ "PRESIDENT",
                            office == "USS" ~ "US SENATE"),
         year = as.numeric(paste0("20", year))) |>
  pivot_wider(names_from = party, values_from = votes) |>
  rename(total_votes = TOT) |>
  pivot_longer(cols = c(DEM, REP), names_to = "party", values_to = "votes") |>
  mutate(party = if_else(party == "DEM", "DEMOCRATIC", "REPUBLICAN")) |>
  filter(!is.na(muni_fips))

muni.archive.complete <- muni.archive |>
  filter(party %in% c("DEMOCRATIC", "REPUBLICAN"),
         office %in% c("PRESIDENT", "US SENATE", "GOVERNOR")) |>
  select(muni_fips, municipality, ctv, year, office, party, total_votes, votes) |>
  mutate(across(contains("fips"), as.character)) |>
  bind_rows(muni.votes.2024)
muni.archive.complete
write_csv(muni.archive.complete, "2024-nov/analysis/MCD/municipality-PresGovSen-2000-2024_NominalIntegration.csv")
