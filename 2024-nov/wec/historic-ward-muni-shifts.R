rm(list = ls())

library(tidyverse)
library(sf)

muni.orig <- read_csv("2024-nov/analysis/mcd/municipality-presgovsen-2000-2024_NominalIntegration.csv")

muni.margin <- muni.orig |>
  mutate(pct = (votes/total_votes)*100) |>
  select(-votes) |>
  pivot_wider(names_from = party, values_from = pct) |>
  mutate(margin = DEMOCRATIC-REPUBLICAN) |>
  select(-c(DEMOCRATIC, REPUBLICAN)) |>
  group_by(muni_fips, office) |>
  arrange(muni_fips, year, office) |>
  mutate(shift = margin - lag(margin, n = 1, order_by = year)) |>
  ungroup()

avg.shifts <- muni.margin |>
  filter(!is.na(shift)) |>
  mutate(shift = abs(shift)) |>
  group_by(year, office) |>
  summarise(mean_shift = mean(shift),
            median_shift = median(shift),
            mean_shift_wt = weighted.mean(shift, w = total_votes),
            median_shift_wt = Hmisc::wtd.quantile(shift, total_votes, 0.5),
            .groups = "drop")
avg.shifts
write_csv(avg.shifts, "~/Dropbox/Projects/2025/February/wisc-frozen-electorate/data/pres-gov-sen_muni-avg-shifts_wi_2004-2024.csv")

avg.shifts |>
  ggplot(aes(year, median_shift_wt)) +
  geom_point() +
  geom_line(color = "gray") +
  ggrepel::geom_text_repel(aes(label = round(median_shift_wt, 2))) +
  labs(title = "Average shift since the previous election",
       subtitle = paste("Each point shows how much the median municipality",
                        "shifted in its vote since the previous election, to either party.",
                        "Municipalities are weighted by total votes cast.")) +
  facet_wrap(facets = ~office)

################################################################################
rep.unit.orig <- st_read("2024-nov/wec/pres-gov-senate_2024-rep-units_1990-2024.geojson") |>
  st_drop_geometry() |> 
  tibble()

rep.unit.margin <- rep.unit.orig |>
  select(county, reporting_unit, starts_with("PRE"), starts_with("USS"), starts_with("GOV")) |>
  pivot_longer(cols = -c(county, reporting_unit)) |>
  separate(name, into = c("office","party","year"), sep = c(3,6)) |>
  pivot_wider(names_from = party, values_from = value) |>
  mutate(margin = (DEM/TOT - REP/TOT)*100,
         year = if_else(str_sub(year, 1, 1) == 9,
                        as.numeric(paste0("19",year)),
                        as.numeric(paste0("20",year))),
         office = case_when(
           office == "PRE" ~ "PRESIDENT",
           office == "USS" ~ "US SENATE",
           office == "GOV" ~ "GOVERNOR"
         )) |>
  select(-c(DEM, REP)) |>
  group_by(county, reporting_unit, office) |>
  arrange(county, reporting_unit, year, office) |>
  mutate(shift = margin - lag(margin, n = 1, order_by = year)) |>
  ungroup()

avg.shifts.rep.unit <- rep.unit.margin |>
  filter(!is.na(shift)) |>
  mutate(shift = abs(shift)) |>
  group_by(year, office) |>
  summarise(mean_shift = mean(shift),
            median_shift = median(shift),
            mean_shift_wt = weighted.mean(shift, w = TOT),
            median_shift_wt = Hmisc::wtd.quantile(shift, TOT, 0.5),
            .groups = "drop")
write_csv(avg.shifts.rep.unit, "~/Dropbox/Projects/2025/February/wisc-frozen-electorate/data/pres-gov-sen_reporting-unit-avg-shifts_wi_1994-2024.csv")

avg.shifts.rep.unit
avg.shifts.rep.unit |>
  ggplot(aes(year, median_shift_wt)) +
  geom_point() +
  geom_line(color = "gray") +
  ggrepel::geom_text_repel(aes(label = round(median_shift_wt, 2))) +
  scale_x_continuous(breaks = seq(1992, 2024, 4)) +
  labs(title = "Average shift since the previous election",
       subtitle = paste("Each point shows how much the median reporting unit",
                        "shifted in its vote since the previous election, to either party.",
                        "Reporting units are weighted by total votes cast.")) +
  facet_wrap(facets = ~office)
