rm(list = ls())

library(tidyverse)
library(sf)
source("2024-nov/scripts/download-returns.R")

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

################################################################################
# these are the reporting unit polygons
#   derived from WEC and LTSB sources
rep.units.shp <- st_read("2024-nov/rep-unit-polygons/rep-unit-polygons-with-votes-2012-2022.geojson") |>
  select(rep_unit, county, ctv, municipality, MCD_FIPS) |>
  st_drop_geometry() |>
  tibble()

################################################################################
# the processed raw election results

# list of valid countynames
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
      reporting_unit == "CITY OF HARTFORD, DISTRICT 3 WARD 4-7, 13, 16, 18" ~ "CITY OF HARTFORD, DISTRICT 3 WARD 4-7, 13, 16, 18, 19",
      reporting_unit == "CITY OF NEENAH, DIST 2, WARD 16 AND DIST 3, WARDS 17-19& 24" ~ "CITY OF NEENAH WARDS 16-19,24",
      reporting_unit == "T-ARCADIA W1-3" ~ "T-ARCADIA W1-4",
      reporting_unit == "VILLAGE OF HOLMEN WARDS 1-" ~ "VILLAGE OF HOLMEN WARDS 1-5",
      TRUE ~ reporting_unit),
    county = case_when(
      reporting_unit == "CITY OF MILWAUKEE WARD 316" ~ "WASHINGTON",
      reporting_unit == "CITY OF MILWAUKEE WARD 317" ~ "WAUKESHA",
      reporting_unit == "CITY OF HARTFORD, DISTRICT 2 WARD 11 - DODGE" ~ "DODGE",
      TRUE ~ county
    )) |>
  filter(municipality != "CALEDONIA (VOID W6-7)") # seems like a row that should be dropped
write_csv(all.orig, "2024-nov/processed/all-raw-processed.csv")
################################################################################

################################################################################
# reporting units from the raw election results
orig.rep.units <- all.orig |>
  group_by(county, ctv, municipality, reporting_unit) |>
  summarise(.groups = "drop")

# these reporting units consist of a single entire MCD
rep.units.mcd <- orig.rep.units |>
  group_by(county, ctv, municipality) |>
  filter(n() == 1) |>
  ungroup()

# these reporting units are in municipalities with at least 2 reporting units
rep.units.wards <- orig.rep.units |>
  group_by(county, ctv, municipality) |>
  filter(n() > 1) |>
  mutate(wards = word(reporting_unit, 2, sep = "\\bW(?=[0-9])|WARD(?=[0-9])|WARDS(?=[0-9])|\\bW\\b|\\bWARD\\b|\\bWARDS\\b|\\bD\\b|\\bD(?=[0-9])|\\bWDS\\b|\\bWD\\b"),
         wards = str_remove_all(wards, "\n"),
         wards = str_remove_all(wards, " "),
         wards = str_replace_all(wards, "&", ","),
         wards = str_remove(wards, coll(")")),
         wards = str_remove(wards, "^00|^0")) |>
  # munge as needed
  mutate(wards = case_when(
    reporting_unit == "CITY OF ONALASKA DISTRICT 1" ~ "1-5",
    reporting_unit == "CITY OF ONALASKA DISTRICT 2" ~ "6-10",
    reporting_unit == "CITY OF ONALASKA DISTRICT 3" ~ "11-15",
    reporting_unit == "CITY OF MERRILL D1" ~ "1",
    reporting_unit == "CITY OF MERRILL D2" ~ "4",
    reporting_unit == "CITY OF MERRILL D3" ~ "5,14",
    reporting_unit == "CITY OF MERRILL D4" ~ "7,9",
    reporting_unit == "CITY OF MERRILL D5" ~ "8,10",
    reporting_unit == "CITY OF MERRILL D6" ~ "11",
    reporting_unit == "CITY OF MERRILL D7" ~ "6,12,13,16",
    reporting_unit == "CITY OF MERRILL D8" ~ "2,3,15",
    reporting_unit == "CITY OF TOMAHAWK D1" ~ "1,2",
    reporting_unit == "CITY OF TOMAHAWK D2" ~ "3,4",
    reporting_unit == "CITY OF TOMAHAWK D3" ~ "5,6",
    reporting_unit == "VILLAGE OF HOLMEN WARDS 1-5, 12-" ~ "1-5,12-13",
    reporting_unit == "C BEAVER DAM" ~ "8,11,19-20,23,27",
    reporting_unit == "V GREENDALE WARDS 1,2,3,49,10" ~ "1,2,3,4,9,10",
    reporting_unit == "CITY OF HARTFORD, DISTRICT 2 WARD 11 - DODGE" ~ "11",
    TRUE ~ wards
  )) |>
  ungroup() |>
  rowwise() |>
  mutate(wardstring = expand_dash(wards)) |>
  ungroup() |>
  select(-wards)

##############################
# join the complete-MCD reporting units
rep.units.mcd.join <- rep.units.mcd |>
  left_join(rep.units.shp)
rep.units.mcd.join |> filter(is.na(MCD_FIPS))
rep.units.mcd.join |>
  group_by(county, ctv, municipality, reporting_unit) |>
  filter(n() > 1)
##############################
# join the ward-based reporting units
rep.units.shp.wards <- rep.units.shp |>
  anti_join(rep.units.mcd.join) |>
  mutate(wards = word(rep_unit, 2, sep = "\\bWARDS\\b|\\bWARD\\b|\\bWD\\b"),
         wards = str_remove_all(wards, " "),
         wards = case_when(
           wards == "8-7" ~ "7-8",
           rep_unit == "VILLAGE OF HOLMEN WARDS 1-5,12-13" ~ "1-5",
           TRUE ~ wards
         )) |>
  rowwise() |>
  mutate(wardstring = expand_dash(wards)) |>
  ungroup()

rep.units.wards.join <- rep.units.wards |>
  left_join(rep.units.shp.wards)
not.joined <- rep.units.wards.join |>
  filter(is.na(MCD_FIPS))
not.joined # can't find any GIS info on these 4 C Oshkosh wards
################################################################################
# combine matched reporting units

all.matched.reporting.units <- bind_rows(
  rep.units.wards.join |>
    select(county, ctv, municipality, MCD_FIPS, reporting_unit, rep_unit),
  rep.units.mcd.join |>
    select(county, ctv, municipality, MCD_FIPS, reporting_unit, rep_unit)
)

# both should be empty
anti_join(all.matched.reporting.units, all.orig)
anti_join(all.orig, all.matched.reporting.units)

all.matched.reporting.units

polygons <- st_read("2024-nov/rep-unit-polygons/rep-unit-polygons-with-votes-2012-2022.geojson")
polygons.matched <- all.matched.reporting.units |>
  inner_join(polygons) |>
  st_as_sf()
nrow(polygons.matched) == nrow(all.matched.reporting.units)
st_write(polygons.matched, "2024-nov/processed/matched-polygons.geojson", delete_dsn = T)

# complete polygons with NA for those that haven't yet been processed and matched
complete.polygons.match.status <- all.matched.reporting.units |>
  full_join(polygons) |>
  st_as_sf()
nrow(complete.polygons.match.status) == nrow(rep.units.shp)
st_write(complete.polygons.match.status, "2024-nov/processed/all-polygons-with-match-status.geojson", delete_dsn = T)
