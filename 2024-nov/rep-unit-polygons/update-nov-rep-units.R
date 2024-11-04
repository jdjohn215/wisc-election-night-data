rm(list = ls())

library(tidyverse)

# expand a list of numbers including dashes into full list
expand_dash <- function(wardstring){
  wardstring.elements <- str_sort(unlist(str_split(wardstring, ",")), numeric = T)
  wardstring.list <- lapply(
    X = wardstring.elements,
    FUN = function(text){
      if(str_detect(text, "-")){
        limits.c <- unlist(strsplit(text, '-'))
        limits.n <- as.numeric(str_remove_all(limits.c, "[A-Z]"))
        all.elements <- as.character(seq(limits.n[1], limits.n[2]))
        all.elements[1] <- limits.c[1]
        all.elements[length(all.elements)] <- limits.c[2]
        paste(all.elements, collapse = ",")
      } else {
        text
      }
    }
  )
  str_remove_all(paste(wardstring.list, collapse = ","), " ")
}

# these were generated to match the august primary WEC file
rep.unit.polygons <- st_read("2024-nov/rep-unit-polygons/rep-units-aug2024.geojson")

rep.unit.polygons

################################################################################
# the NEW november vintage Dane County wards
#   The City of Madison has added ward 135
dane.wards <- st_read("2024-nov/rep-unit-polygons/Dane") |>
  mutate(MCD_FIPS = paste0("55025", FIPSCODE),
         ctv = str_sub(NAME, 1, 1),
         municipality = word(NAME, 3, -1),
         county = "DANE",
         WardNumber = as.numeric(WardNumber)) |>
  select(county, ctv, municipality, MCD_FIPS, WardNumber)

madison.rep.units <- dane.wards |>
  filter(MCD_FIPS == "5502548000") |>
  mutate(rep_unit = paste("CITY OF MADISON WARD", WardNumber)) |>
  group_by(county, ctv, municipality, MCD_FIPS, rep_unit) |>
  summarise(.groups = "drop") |>
  st_make_valid() |>
  st_transform(crs = st_crs(rep.unit.polygons))

################################################################################
# remove the outdated rep unit polygons and replace with the new ones
updated.rep.units <- rep.unit.polygons |>
  filter(MCD_FIPS != "5502548000") |>
  rmapshaper::ms_erase(erase = madison.rep.units, remove_slivers = T) |>
  bind_rows(madison.rep.units) |>
  mutate(municipality = str_remove_all(municipality, coll(".")),
         across(where(is.character), str_to_upper)) |>
  # combine the two Village of Vernon rep units into a single rep unit
  #   so as to match the new format
  mutate(rep_unit = if_else(MCD_FIPS == "5513382575",
                            "VILLAGE VERNON WARDS 1 - 11",
                            rep_unit)) |>
  group_by(county, ctv, municipality, MCD_FIPS, rep_unit) |>
  st_make_valid() |>
  summarise(.groups = "drop") |>
  st_make_valid()

updated.rep.units.no.overlaps <- st_difference(updated.rep.units)

st_write(updated.rep.units.no.overlaps, "2024-nov/rep-unit-polygons/rep-units-nov2024.geojson",
         delete_dsn = T)
