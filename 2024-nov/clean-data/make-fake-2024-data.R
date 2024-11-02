rm(list = ls())

library(tidyverse)
library(sf)

rep.units <- st_read("2024-aug/rep-unit-polygons/rep-unit-polygons-with-votes-2012-2022.geojson")

with.fake.2024.data <- rep.units |>
  rowwise() |>
  mutate(PRETOT24 = rnorm(1, PRETOT20, sd = PRETOT20*0.1),
         PREDEM24 = rnorm(1, PREDEM20, sd = PREDEM20*0.1),
         PREREP24 = rnorm(1, PREREP20, sd = PREREP20*0.1),
         USSTOT24 = rnorm(1, USSTOT22, sd = USSTOT22*0.1),
         USSDEM24 = rnorm(1, USSDEM22, sd = USSDEM22*0.1),
         USSREP24 = rnorm(1, USSREP22, sd = USSREP22*0.1)) |>
  ungroup() |>
  mutate(across(ends_with("24") & where(is.numeric), round)) |>
  select(county, ctv, municipality, MCD_FIPS, rep_unit, ends_with("24"), everything())

saveRDS(with.fake.2024.data, "2024-nov/clean-data/rep-units-with-fake-2024-data.rds")
