rm(list = ls())

library(tidyverse)
library(sf)
library(leaflet)

################################################################################
# the election results
all.results <- read_csv("2024-nov/wec/wec-original-all-races-long-format.csv")


################################################################################
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
# identify the individual wards in each reporting unit
reporting.units.to.wards <- all.results |>
  group_by(county, reporting_unit) |>
  summarise() |>
  separate(reporting_unit, into = c("municipality", "wards"), 
           sep = " WARDS | WARD ", remove = F) |>
  rowwise() |>
  mutate(wards = expand_dash(wards)) |>
  ungroup() |>
  separate(wards, into = paste0("w", 1:21), sep = ",") |>
  pivot_longer(cols = starts_with("w"), names_to = "drop", values_to = "ward") |>
  filter(!is.na(ward)) |>
  select(-drop) |>
  mutate(ctv = str_sub(municipality, 1, 1),
         municipality = str_remove(municipality, "^CITY OF |^VILLAGE OF |^TOWN OF ")) |>
  select(county, ctv, municipality, reporting_unit, ward) |>
  # munge
  mutate(municipality = case_when(
    municipality == "MT. STERLING" ~ "MOUNT STERLING",
    municipality == "LAVALLE" ~ "LA VALLE",
    municipality == "LAND O-LAKES" ~ "LAND O'LAKES",
    municipality == "FONTANA" ~ "FONTANA-ON-GENEVA LAKE",
    municipality == "SAINT LAWRENCE" ~ "ST. LAWRENCE",
    TRUE ~ municipality
  ),
  WARDID = str_pad(ward, width = 4, side = "left", pad = "0"))

################################################################################
# ward shapes: see integrate-ward-vintages.R
ward.shp <- st_read("2024-nov/wec/wards.geojson") |>
  # remove this because it entirely overlaps with ward 4 in a way that interfers with the rep unit polygon creation
  filter(WARD_FIPS != "55079103750007") |>
  # munge ward IDs where needed
  mutate(WARDID = case_when(
    WARD_FIPS == "55105099250001" ~ "0007", # C Broadhead in Rock County wards are misnumbered
    WARD_FIPS == "55019818500001" ~ "0002", # V Unity in Clarke County ward is misnumbered
    TRUE ~ WARDID
  ),
  ctv = case_when(
    WARD_FIPS == "55027059250027" ~ "C", # C Beaver Dam ward 27 is incorrectly labelled T Beaver Dam
    TRUE ~ ctv
  ),
  WARD_FIPS = if_else(WARD_FIPS == "55027059250027", "55027059000027", WARD_FIPS))

matched.to.shp <- left_join(reporting.units.to.wards, ward.shp)
not.matched.to.shp <- anti_join(reporting.units.to.wards, ward.shp)

# visualize the ward polygons not accounted for in the reporting units
#   they are all unpopulated slivers of land
anti_join(ward.shp, reporting.units.to.wards) |>
  leaflet() |>
  addProviderTiles(provider = providers$CartoDB.Positron) |>
  addPolygons(label = ~ward_label)

# these are the reporting units where NONE of the consitutent wards are present in the polygon file
#   when NONE, that means at least some wards in each reporting unit are matched
matched.to.shp |>
  st_drop_geometry() |>
  group_by(county, reporting_unit) |>
  mutate(ward_n = n()) |>
  filter(is.na(ward_label)) |>
  filter(n() == ward_n)
# it is apparently common for 'ghost wards' to exist in the reporting unit titles
#   which have no actual physical existence

# aggregate reporting unit polygons
rep.unit.polygons <- matched.to.shp |>
  mutate(MCD_FIPS = str_sub(WARD_FIPS, 1, 10)) |>
  st_as_sf() |>
  group_by(county, ctv, municipality, MCD_FIPS, reporting_unit) |>
  summarise(geometry = st_union(geometry, is_coverage = TRUE), .groups = "drop") |>
  filter(st_geometry_type(geometry) %in% c("POLYGON","MULTIPOLYGON"))

# make sure each reporting unit is a single row
rep.unit.polygons |>
  group_by(county, reporting_unit) |>
  filter(n() > 1)

# confirm that all election results are matched
anti_join(all.results, rep.unit.polygons)

leaflet(rep.unit.polygons) |>
  addTiles() |>
  addPolygons(label = ~reporting_unit)

st_write(rep.unit.polygons, "2024-nov/wec/rep-units.geojson", delete_dsn = T)
