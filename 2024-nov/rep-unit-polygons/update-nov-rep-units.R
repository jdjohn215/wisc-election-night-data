rm(list = ls())

library(tidyverse)
library(sf)

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
# two Milwaukee county reporting units from August need to be split
#   "VILLAGE OF BAYSIDE WARDS 1,2-3,4-5" becomes:
#     - VILLAGE OF BAYSIDE WARDS 1,3 
#     - VILLAGE OF BAYSIDE WARDS 2,4,5
#   "VILLAGE OF RIVER HILLS WARDS 1-2" becomes:
#     - VILLAGE OF RIVER HILLS WARD 1
#     - VILLAGE OF RIVER HILLS WARD 1
wards.august <- st_read("2024-aug/rep-unit-polygons/wards.geojson")
mke.cnty.replacements <- wards.august |>
  filter(ward_label %in% c("Bayside - V 0001", "Bayside - V 0002",
                           "Bayside - V 0003", "Bayside - V 0004",
                           "Bayside - V 0005", "River Hills - V 0001",
                           "River Hills - V 0002")) |>
  mutate(rep_unit = case_when(
    ward_label %in% c("Bayside - V 0001", "Bayside - V 0003") ~ "VILLAGE OF BAYSIDE WARDS 1,3",
    ward_label %in% c("Bayside - V 0002", "Bayside - V 0004", "Bayside - V 0005") ~ "VILLAGE OF BAYSIDE WARDS 2,4,5",
    ward_label %in% c("River Hills - V 0001") ~ "VILLAGE OF RIVER HILLS WARD 1",
    ward_label %in% c("River Hills - V 0002") ~ "VILLAGE OF RIVER HILLS WARD 2"
  )) |>
  mutate(county = "MILWAUKEE",
         MCD_FIPS = str_sub(WARD_FIPS, 1, 10)) |>
  group_by(county, ctv, municipality, MCD_FIPS, rep_unit) |>
  summarise(.groups = "drop") |>
  st_make_valid() |>
  st_transform(crs = st_crs(rep.unit.polygons))
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
  st_make_valid()
winnebago.replacements <- winnebago |>
  filter(MCD_FIPS == "5513960500") |>
  mutate(rep_unit = paste("CITY OF OSHKOSH WARD", WardNumber)) |> 
  select(county, ctv, municipality, MCD_FIPS, rep_unit) |>
  st_transform(crs = st_crs(rep.unit.polygons)) |>
  mutate(across(where(is.character), str_to_upper))
################################################################################
# remove the outdated rep unit polygons and replace with the new ones
updated.rep.units <- rep.unit.polygons |>
  # replace the Madison wards
  filter(MCD_FIPS != "5502548000") |>
  rmapshaper::ms_erase(erase = madison.rep.units, remove_slivers = T) |>
  bind_rows(madison.rep.units) |>
  # replace the Milwaukee county wards
  filter(! rep_unit %in% c("VILLAGE OF BAYSIDE WARDS 1,2-3,4-5",
                           "VILLAGE OF RIVER HILLS WARDS 1-2")) |>
  rmapshaper::ms_erase(mke.cnty.replacements) |>
  bind_rows(mke.cnty.replacements) |>
  # replace the Winnebago county wards
  filter(MCD_FIPS != "5513960500") |>
  rmapshaper::ms_erase(erase = winnebago.replacements, remove_slivers = T) |>
  bind_rows(winnebago.replacements) |>
  # combine the two Village of Vernon rep units into a single rep unit
  #   so as to match the new format
  mutate(rep_unit = if_else(MCD_FIPS == "5513382575",
                            "VILLAGE VERNON WARDS 1 - 11",
                            rep_unit)) |>
  group_by(county, ctv, municipality, MCD_FIPS, rep_unit) |>
  st_make_valid() |>
  summarise(.groups = "drop") |>
  st_make_valid() |>
  mutate(municipality = str_remove_all(municipality, coll(".")),
         across(where(is.character), str_to_upper))

updated.rep.units.no.overlaps <- st_difference(updated.rep.units)

st_write(updated.rep.units.no.overlaps, "2024-nov/rep-unit-polygons/rep-units-nov2024.geojson",
         delete_dsn = T)
