rm(list = ls())

library(tidyverse)

rep.unit.demographics <- read_csv("2024-nov/demog/reporting-unit-demographics-2020census.csv")
orig.votes <- sf::st_read("2024-nov/analysis/election-results-2012-2024.geojson") |>
  sf::st_drop_geometry() |>
  # add two-party totals
  mutate(USSTOT24 = USSDEM24 + USSREP24,
         USSTOT22 = USSDEM22 + USSREP22,
         USSTOT16 = USSDEM16 + USSREP16,
         USSTOT18 = USSDEM18 + USSREP18,
         PRETOT24 = PREDEM24 + PREREP24,
         PRETOT20 = PREDEM20 + PREREP20,
         PRETOT16 = PREDEM16 + PREREP16)
rep.unit.votes <- orig.votes |>
  filter(level == "reporting unit")
pct.reporting <- round(sum(rep.unit.votes$PRETOT24)/sum(orig.votes$PRETOT24)*100, 1)
completion <- paste("This analysis is based on unofficial ward returns covering",
                    paste0(pct.reporting, "%"), "of voters in 2024.")

################################################################################
# calculate race/ethnic majority per reporting unit
rep.unit.majority <- rep.unit.demographics |>
  select(rep_unit, county, ctv, municipality, MCD_FIPS, rep_unit_2024, starts_with("pop")) |>
  select(-pop_per_sq_mi) |>
  pivot_longer(cols = starts_with("pop_")) |>
  mutate(pct = value/pop*100) |>
  group_by(rep_unit, county, ctv, municipality, MCD_FIPS) |>
  slice_max(order_by = pct, n = 1, with_ties = F) |>
  mutate(majority = if_else(pct > 50, word(name, -1, sep = "_"),
                            "no majority")) |>
  ungroup() |>
  select(rep_unit, county, ctv, municipality, MCD_FIPS, majority, rep_unit_2024) |>
  mutate(MCD_FIPS = as.character(MCD_FIPS))

votes.by.majority <- rep.unit.votes |>
  inner_join(rep.unit.majority) |>
  group_by(majority) |>
  summarise(rep_units = n(),
            across(where(is.numeric), sum)) |>
  mutate(PREMARG24 = (PREDEM24/PRETOT24 - PREREP24/PRETOT24)*100,
         PREMARG20 = (PREDEM20/PRETOT20 - PREREP20/PRETOT20)*100,
         PREMARG16 = (PREDEM16/PRETOT16 - PREREP16/PRETOT16)*100,
         USSMARG16 = (USSDEM16/USSTOT16 - USSREP16/USSTOT16)*100,
         USSMARG18 = (USSDEM18/USSTOT18 - USSREP18/USSTOT18)*100,
         USSMARG22 = (USSDEM22/USSTOT22 - USSREP22/USSTOT22)*100,
         USSMARG24 = (USSDEM24/USSTOT24 - USSREP24/USSTOT24)*100)

majority.caption.text <- paste("Graph and analysis by John D. Johnson (@jdjmke).",
                               "Data sources: Wisconsin Election Commission, unofficial",
                               "2024 election results, & 2020 census data.",
                               "Census blocks are allocated to reporting units based on their centroid.")
gg.majority <- votes.by.majority |>
  select(majority, rep_units, PRETOT24, contains("MARG")) |>
  filter(majority %in% c("white", "hisp","black")) |>
  mutate(majority = case_when(
    majority == "black" ~ "Majority Black wards",
    majority == "hisp" ~ "Majority Hispanic or Latino wards",
    majority == "white" ~ "Majority white wards"
  )) |>
  pivot_longer(cols = contains("MARG"), names_to = "contest", values_to = "margin") |>
  separate(contest, into = c("office","stat","year"), sep = c(3,-2)) |>
  mutate(year = as.numeric(paste0("20", year)),
         office = if_else(office == "PRE", "President", "US Senate"),
         fontcolor = if_else(margin < 0, "red", "blue")) |>
  ggplot(aes(year, margin, linetype = office, shape = office)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(color = fontcolor)) +
  geom_line() +
  ggrepel::geom_text_repel(aes(label = round(margin), color = fontcolor),
                           min.segment.length = 0.01) +
  scale_color_identity() +
  facet_wrap(facets = ~majority, nrow = 1) +
  labs(title = "Wisconsin Presidential & US Senate election results by ward majority",
       subtitle = completion,
       caption = str_wrap(majority.caption.text, 120),
       y = "Dem % minus Rep % (of the 2-party vote) (of the 2-party vote)",
       x = NULL) +
  scale_y_continuous(labels = function(x){case_when(x > 0 ~ paste0("+",x,"D"),
                                                    x == 0 ~ "tie",
                                                    x < 0 ~ paste0("+", abs(x), "R")
  )}) +
  theme_bw() +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(face = "bold"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = -90, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.90, 0.9),
        legend.background = element_rect(fill = "aliceblue"),
        plot.background = element_rect(fill = "aliceblue"),
        panel.background = element_rect(fill = "linen"))
ggsave("2024-nov/graphics/results-by-majority.png", gg.majority, width = 7, height = 5)
################################################################################

################################################################################
# plot vote by density quintiles
density.votes <- rep.unit.votes |>
  inner_join(rep.unit.demographics |>
               mutate(MCD_FIPS = as.character(MCD_FIPS))) |>
  select(county, ctv, municipality, MCD_FIPS, reporting_unit, pop_per_sq_mi,
         starts_with("USS"), starts_with("PRE"))

# density quintiles
density.quintiles <- Hmisc::wtd.quantile(density.votes$pop_per_sq_mi,
                                         weights = density.votes$PRETOT24,
                                         probs = c(0.2,0.4,0.6,0.8))

density.votes.with.quintiles <- density.votes |>
  mutate(
    density_quintile = case_when(
      pop_per_sq_mi < density.quintiles[1] ~ "Least dense",
      pop_per_sq_mi < density.quintiles[2] ~ "Less dense",
      pop_per_sq_mi < density.quintiles[3] ~ "Medium density",
      pop_per_sq_mi < density.quintiles[4] ~ "More dense",
      TRUE ~ "Most dense"),
    density_quintile = fct_reorder(density_quintile, pop_per_sq_mi, .na_rm = T))

df.density.quintiles <- density.votes.with.quintiles |>
  group_by(density_quintile) |>
  summarise(median_density = Hmisc::wtd.quantile(pop_per_sq_mi, PRETOT24, probs = 0.5),
            across(where(is.numeric), sum)) |>
  select(-pop_per_sq_mi) |>
  mutate(PREMARG24 = (PREDEM24/PRETOT24 - PREREP24/PRETOT24)*100,
         PREMARG20 = (PREDEM20/PRETOT20 - PREREP20/PRETOT20)*100,
         PREMARG16 = (PREDEM16/PRETOT16 - PREREP16/PRETOT16)*100,
         USSMARG16 = (USSDEM16/USSTOT16 - USSREP16/USSTOT16)*100,
         USSMARG18 = (USSDEM18/USSTOT18 - USSREP18/USSTOT18)*100,
         USSMARG22 = (USSDEM22/USSTOT22 - USSREP22/USSTOT22)*100,
         USSMARG24 = (USSDEM24/USSTOT24 - USSREP24/USSTOT24)*100) |>
  select(density_quintile, median_density, contains("MARG"))  |>
  pivot_longer(cols = contains("MARG"), names_to = "contest", values_to = "margin") |>
  separate(contest, into = c("office","stat","year"), sep = c(3,-2)) |>
  mutate(year = as.numeric(paste0("20", year)),
         office = if_else(office == "PRE", "President", "US Senate"),
         fontcolor = if_else(margin < 0, "red", "blue"))
density.caption.text <- paste("Graph and analysis by John D. Johnson (@jdjmke).",
                              "Data sources: Wisconsin Election Commission, unofficial",
                              "2024 election results, & 2020 census data.",
                              "Total population for each reporting unit is calculated by",
                              "assigning 2020 census blocks to each unit using block centroids.",
                              "Population density is calculated by dividing the 2020 population",
                              "by the land area of each reporting unit.")

gg.density.quintiles <- df.density.quintiles |>
  ggplot(aes(year, margin, linetype = office, shape = office)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(color = fontcolor)) +
  geom_line() +
  ggrepel::geom_text_repel(aes(label = round(margin), color = fontcolor),
                           min.segment.length = 0.01) +
  scale_color_identity() +
  facet_wrap(facets = ~density_quintile, nrow = 1) +
  labs(title = "Wisconsin Presidential & US Senate election results by population density",
       subtitle = paste("Each column represents one-fifth of voters.", completion),
       caption = str_wrap(density.caption.text, 160),
       y = "Dem % minus Rep % (of the 2-party vote)",
       x = NULL) +
  scale_y_continuous(labels = function(x){case_when(x > 0 ~ paste0("+",x,"D"),
                                                    x == 0 ~ "tie",
                                                    x < 0 ~ paste0("+", abs(x), "R")
  )}) +
  theme_bw() +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(face = "bold"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = -90, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.07, 0.9),
        legend.background = element_rect(fill = "aliceblue"),
        plot.background = element_rect(fill = "aliceblue"),
        panel.background = element_rect(fill = "linen"))
ggsave("2024-nov/graphics/density-quintiles.png", gg.density.quintiles, width = 10, height = 6)
