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
# several Richland county reporting units from August need to be combined
#   "CITY OF RICHLAND CENTER" becomes single reporting unit
richland.replacements <- wards.august |>
  mutate(MCD_FIPS = str_sub(WARD_FIPS, 1, 10)) |>
  filter((MCD_FIPS == "5510367625")) |>
  mutate(rep_unit = "CITY OF RICHLAND WARDS 1-14") |>
  group_by(county, ctv, municipality, MCD_FIPS, rep_unit) |>
  summarise(.groups = "drop") |>
  st_make_valid() |>
  st_transform(crs = st_crs(rep.unit.polygons))
################################################################################
# several Hudson county reporting units from August need to be split
#   "TOWN OF HUDSON WARDS 1-14" becomes 
#     - TOWN OF HUDSON WARDS 1-2
#     - TOWN OF HUDSON WARDS 3-6
#     - TOWN OF HUDSON WARDS 7-14
hudson.replacements <- wards.august |>
  mutate(MCD_FIPS = str_sub(WARD_FIPS, 1, 10)) |>
  filter((MCD_FIPS == "5510936275")) |>
  mutate(rep_unit = case_when(
    ward_label %in% c("HUDSON - T 0001", "HUDSON - T 0001") ~ "TOWN OF HUDSON WARDS 1-2",
    ward_label %in% c("HUDSON - T 0003", "HUDSON - T 0004",
                      "HUDSON - T 0005", "HUDSON - T 0006") ~ "TOWN OF HUDSON WARDS 3-6",
    TRUE ~ "TOWN OF HUDSON WARDS 7-14")) |>
  group_by(county, ctv, municipality, MCD_FIPS, rep_unit) |>
  summarise(.groups = "drop") |>
  st_make_valid() |>
  st_transform(crs = st_crs(rep.unit.polygons))
################################################################################
# several Walworth county reporting units from August need to be split
#   "CITY OF WHITEWATER WARDS 1-10" becomes 
#     - CITY OF WHITEWATER WARDS 1-7
#     - CITY OF WHITEWATER WARDS 8-10
walworth.replacements <- wards.august |>
  mutate(MCD_FIPS = str_sub(WARD_FIPS, 1, 10)) |>
  filter((MCD_FIPS == "5512786925")) |>
  mutate(rep_unit = case_when(
    ward_label %in% c("Whitewater - C 0001", "Whitewater - C 0002", "Whitewater - C 0003",
                      "Whitewater - C 0004", "Whitewater - C 0005", "Whitewater - C 0006",
                      "Whitewater - C 0007") ~ "CITY OF WHITEWATER WARDS 1-7",
    ward_label %in% c("Whitewater - C 0008", "Whitewater - C 0009", "Whitewater - C 0010") ~ "CITY OF WHITEWATER WARDS 8-10")) |>
  group_by(county, ctv, municipality, MCD_FIPS, rep_unit) |>
  summarise(.groups = "drop") |>
  st_make_valid() |>
  st_transform(crs = st_crs(rep.unit.polygons))
################################################################################
# several Door county reporting units from August need to be split
#   "CITY OF STURGEON BAY WARDS 1-9" becomes:
#     - CITY OF STURGEON BAY WARDS 1-7,9
#     - CITY OF STURGEON BAY WARD 8
#   "CITY OF STURGEON BAY WARDS 16-21" becomes:
#     - CITY OF STURGEON BAY WARDS 16
#     - CITY OF STURGEON BAY WARDS 17-21
#   "TOWN OF EGG HARBOR WARDS 1-3" becomes:
#     - TOWN OF EGG HARBOR WARDS 1-2
#     - TOWN OF EGG HARBOR WARDS 3
#   "TOWN OF STURGEON BAY WARDS 1-2" becomes:
#     - TOWN OF STURGEON BAY WARD 1
#     - TOWN OF STURGEON BAY WARD 2
#   "TOWN OF SEVASTOPOL WARDS 1-5" becomes:
#     - TOWN OF SEVASTOPOL WARDS 2-5
#     - TOWN OF SEVASTOPOL WARD 1
door.cnty.replacements <- wards.august |>
  mutate(rep_unit = case_when(
    ward_label %in% c("Sturgeon Bay - C 0001", "Sturgeon Bay - C 0002", "Sturgeon Bay - C 0003",
                      "Sturgeon Bay - C 0004", "Sturgeon Bay - C 0005", "Sturgeon Bay - C 0006",
                      "Sturgeon Bay - C 0007", "Sturgeon Bay - C 0009") ~ "CITY OF STURGEON BAY WARDS 1-7,9",
    ward_label %in% c("Sturgeon Bay - C 0008") ~ "CITY OF STURGEON BAY WARD 8",
    ward_label %in% c("Sturgeon Bay - C 0016") ~ "CITY OF STURGEON BAY WARD 16",
    ward_label %in% c("Sturgeon Bay - C 0017","Sturgeon Bay - C 0018","Sturgeon Bay - C 0019",
                      "Sturgeon Bay - C 0020","Sturgeon Bay - C 0021") ~ "CITY OF STURGEON BAY WARDS 17-21",
    ward_label %in% c("EGG HARBOR - T 0001", "EGG HARBOR - T 0001") ~ "TOWN OF EGG HARBOR WARDS 1-2",
    ward_label %in% c("EGG HARBOR - T 0003") ~ "TOWN OF EGG HARBOR WARD 3",
    ward_label %in% c("STURGEON BAY - T 0001") ~ "TOWN OF STURGEON BAY WARD 1",
    ward_label %in% c("STURGEON BAY - T 0002") ~ "TOWN OF STURGEON BAY WARD 2",
    ward_label %in% c("SEVASTOPOL - T 0002", "SEVASTOPOL - T 0003",
                      "SEVASTOPOL - T 0004", "SEVASTOPOL - T 0005") ~ "TOWN OF SEVASTOPOL WARDS 2-5",
    ward_label %in% c("SEVASTOPOL - T 0001") ~ "TOWN OF SEVASTOPOL WARD 1",
    TRUE ~ NA
  )) |>
  filter(!is.na(rep_unit)) |>
  mutate(county = "DOOR",
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
# The City of Kenosah added some wards, thanks to annexations I think
kenosha <- st_read("2024-nov/rep-unit-polygons/Kenosha") |>
  mutate(MCD_FIPS = paste0("55059", CountySubF),
         ctv = str_sub(FULLNAME, 1, 1),
         municipality = word(FULLNAME, 3, -1),
         county = "KENOSHA",
         WardNumber = str_squish(Ward)) |>
  group_by(county, ctv, municipality, MCD_FIPS, WardNumber) |>
  summarise(.groups = "drop") |>
  st_make_valid()
kenosha.replacements <- kenosha |>
  filter(MCD_FIPS == "5505939225") |>
  mutate(rep_unit = paste("CITY OF KENOSHA WARD", WardNumber)) |> 
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
  # replace the Door county wards
  filter(! rep_unit %in% c("CITY OF STURGEON BAY WARDS 1-9",
                           "CITY OF STURGEON BAY WARDS 16-21",
                           "TOWN OF EGG HARBOR WARDS 1-3",
                           "TOWN OF STURGEON BAY WARDS 1-2",
                           "TOWN OF SEVASTOPOL WARDS 1-5")) |>
  rmapshaper::ms_erase(door.cnty.replacements) |>
  bind_rows(door.cnty.replacements) |>
  # replace the Winnebago county wards
  filter(MCD_FIPS != "5513960500") |>
  rmapshaper::ms_erase(erase = winnebago.replacements, remove_slivers = T) |>
  bind_rows(winnebago.replacements) |>
  # replace the Richland county wards
  filter(MCD_FIPS != "5510367625") |>
  rmapshaper::ms_erase(erase = richland.replacements, remove_slivers = T) |>
  bind_rows(richland.replacements) |>
  # replace the Hudson wards
  filter(MCD_FIPS != "5510936275") |>
  rmapshaper::ms_erase(erase = hudson.replacements, remove_slivers = T) |>
  bind_rows(hudson.replacements) |>
  # replace the Walworth wards
  filter(MCD_FIPS != "5512786925") |>
  rmapshaper::ms_erase(erase = walworth.replacements, remove_slivers = T) |>
  bind_rows(walworth.replacements) |>
  # replace the Kenosha county wards
  filter(MCD_FIPS != "5505939225") |>
  rmapshaper::ms_erase(erase = kenosha.replacements, remove_slivers = T) |>
  bind_rows(kenosha.replacements) |>
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
