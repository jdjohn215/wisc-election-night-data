rm(list = ls())

library(tidyverse)
library(tidycensus)
acs.vars <- load_variables("2022", "acs5", T)

orig.votes <- sf::st_read("2024-nov/wec/pres-gov-senate_2024-rep-units_1990-2024.geojson") |>
  sf::st_drop_geometry() |>
  # add two-party totals
  mutate(USSTOT24 = USSDEM24 + USSREP24,
         USSTOT22 = USSDEM22 + USSREP22,
         USSTOT16 = USSDEM16 + USSREP16,
         USSTOT18 = USSDEM18 + USSREP18,
         PRETOT24 = PREDEM24 + PREREP24,
         PRETOT20 = PREDEM20 + PREREP20,
         PRETOT16 = PREDEM16 + PREREP16)
rep.unit.votes <- orig.votes
pct.reporting <- round(sum(rep.unit.votes$PRETOT24)/sum(orig.votes$PRETOT24)*100, 1)
completion <- paste("This analysis is based on official ward returns.")

rep.unit.tract.cw <- read_csv("2024-nov/wec/rep-units-2024_to_tracts-cw.csv")


rep.unit.tract.cw
rep.unit.votes

tract.votes <- rep.unit.votes |>
  mutate(rep_unit_2024 = paste(county, "-", reporting_unit)) |>
  pivot_longer(cols = where(is.numeric)) |>
  inner_join(rep.unit.tract.cw, relationship = "many-to-many") |>
  mutate(allocated_votes = value*prop_of_rep_unit_2024) |>
  group_by(tract_GEOID, name) |>
  summarise(votes = sum(allocated_votes), .groups = "drop") |>
  pivot_wider(names_from = name, values_from = votes) |>
  mutate(tract_GEOID = as.character(tract_GEOID))

# Educational Attainment for the Population 25 Years and Over
tract.education <- get_acs("tract", state = "WI", year = 2022, survey = "acs5",
                           variables = c("B15003_001", "B15003_022", "B15003_023",
                                         "B15003_024", "B15003_025")) |>
  select(tract_GEOID = GEOID, variable, estimate) |>
  pivot_wider(names_from = variable, values_from = estimate) |>
  mutate(pct_ba = (B15003_022+B15003_023+B15003_024+B15003_025)/B15003_001*100) |>
  select(tract_GEOID, pct_ba) |>
  inner_join(tract.votes)

# Median age
tract.age <- get_acs("tract", state = "WI", year = 2022, survey = "acs5",
                     variables = c("B01002_001")) |>
  select(tract_GEOID = GEOID, median_age = estimate) |>
  inner_join(tract.votes)

# Median household income
tract.income <- get_acs("tract", state = "WI", year = 2022, survey = "acs5",
                        variables = c("B19301_001")) |>
  select(tract_GEOID = GEOID, percapita_income = estimate) |>
  inner_join(tract.votes)

# education quintiles
educ.quintiles <- Hmisc::wtd.quantile(tract.education$pct_ba,
                                      weights = tract.education$PRETOT24,
                                      probs = c(0.2,0.4,0.6,0.8))

# age quintiles
age.quintiles <- Hmisc::wtd.quantile(tract.age$median_age,
                                     weights = tract.age$PRETOT24,
                                     probs = c(0.2,0.4,0.6,0.8))

# income quintiles
income.quintiles <- Hmisc::wtd.quantile(tract.income$percapita_income,
                                        weights = tract.income$PRETOT24,
                                        probs = c(0.2,0.4,0.6,0.8))



tract.votes.with.quintiles <- tract.education |>
  inner_join(tract.age) |>
  inner_join(tract.income) |>
  mutate(
    educ_quintile = case_when(
      pct_ba < educ.quintiles[1] ~ "Lowest education",
      pct_ba < educ.quintiles[2] ~ "Lower education",
      pct_ba < educ.quintiles[3] ~ "Medium education",
      pct_ba < educ.quintiles[4] ~ "Higher education",
      TRUE ~ "Highest education"),
    educ_quintile = fct_reorder(educ_quintile, pct_ba, .na_rm = T),
    age_quintile = case_when(
      median_age < age.quintiles[1] ~ "Youngest",
      median_age < age.quintiles[2] ~ "Younger",
      median_age < age.quintiles[3] ~ "Medium",
      median_age < age.quintiles[4] ~ "Older",
      TRUE ~ "Oldest"),
    age_quintile = fct_reorder(age_quintile, median_age, .na_rm = T),
    income_quintile = case_when(
      percapita_income < income.quintiles[1] ~ "Lowest income",
      percapita_income < income.quintiles[2] ~ "Lower income",
      percapita_income < income.quintiles[3] ~ "Medium income",
      percapita_income < income.quintiles[4] ~ "Higher income",
      TRUE ~ "Highest income"),
    income_quintile = fct_reorder(income_quintile, percapita_income, .na_rm = T))

df.educ.quintiles <- tract.votes.with.quintiles |>
  group_by(educ_quintile) |>
  summarise(median_pct_ba = Hmisc::wtd.quantile(pct_ba, PRETOT24, probs = 0.5),
            across(where(is.numeric), sum)) |>
  select(-pct_ba) |>
  mutate(PREMARG24 = (PREDEM24/PRETOT24 - PREREP24/PRETOT24)*100,
         PREMARG20 = (PREDEM20/PRETOT20 - PREREP20/PRETOT20)*100,
         PREMARG16 = (PREDEM16/PRETOT16 - PREREP16/PRETOT16)*100,
         USSMARG16 = (USSDEM16/USSTOT16 - USSREP16/USSTOT16)*100,
         USSMARG18 = (USSDEM18/USSTOT18 - USSREP18/USSTOT18)*100,
         USSMARG22 = (USSDEM22/USSTOT22 - USSREP22/USSTOT22)*100,
         USSMARG24 = (USSDEM24/USSTOT24 - USSREP24/USSTOT24)*100) |>
  select(educ_quintile, median_pct_ba, contains("MARG"))  |>
  pivot_longer(cols = contains("MARG"), names_to = "contest", values_to = "margin") |>
  separate(contest, into = c("office","stat","year"), sep = c(3,-2)) |>
  mutate(year = as.numeric(paste0("20", year)),
         office = if_else(office == "PRE", "President", "US Senate"),
         fontcolor = if_else(margin < 0, "red", "blue"))

df.age.quintiles <- tract.votes.with.quintiles |>
  group_by(age_quintile) |>
  summarise(median_age = Hmisc::wtd.quantile(median_age, PRETOT24, probs = 0.5),
            across(where(is.numeric), sum)) |>
  mutate(PREMARG24 = (PREDEM24/PRETOT24 - PREREP24/PRETOT24)*100,
         PREMARG20 = (PREDEM20/PRETOT20 - PREREP20/PRETOT20)*100,
         PREMARG16 = (PREDEM16/PRETOT16 - PREREP16/PRETOT16)*100,
         USSMARG16 = (USSDEM16/USSTOT16 - USSREP16/USSTOT16)*100,
         USSMARG18 = (USSDEM18/USSTOT18 - USSREP18/USSTOT18)*100,
         USSMARG22 = (USSDEM22/USSTOT22 - USSREP22/USSTOT22)*100,
         USSMARG24 = (USSDEM24/USSTOT24 - USSREP24/USSTOT24)*100) |>
  select(age_quintile, median_age, contains("MARG"))  |>
  pivot_longer(cols = contains("MARG"), names_to = "contest", values_to = "margin") |>
  separate(contest, into = c("office","stat","year"), sep = c(3,-2)) |>
  mutate(year = as.numeric(paste0("20", year)),
         office = if_else(office == "PRE", "President", "US Senate"),
         fontcolor = if_else(margin < 0, "red", "blue"))

df.income.quintiles <- tract.votes.with.quintiles |>
  group_by(income_quintile) |>
  summarise(median_income = Hmisc::wtd.quantile(percapita_income, PRETOT24, probs = 0.5),
            across(where(is.numeric), sum)) |>
  mutate(PREMARG24 = (PREDEM24/PRETOT24 - PREREP24/PRETOT24)*100,
         PREMARG20 = (PREDEM20/PRETOT20 - PREREP20/PRETOT20)*100,
         PREMARG16 = (PREDEM16/PRETOT16 - PREREP16/PRETOT16)*100,
         USSMARG16 = (USSDEM16/USSTOT16 - USSREP16/USSTOT16)*100,
         USSMARG18 = (USSDEM18/USSTOT18 - USSREP18/USSTOT18)*100,
         USSMARG22 = (USSDEM22/USSTOT22 - USSREP22/USSTOT22)*100,
         USSMARG24 = (USSDEM24/USSTOT24 - USSREP24/USSTOT24)*100) |>
  select(income_quintile, median_income, contains("MARG"))  |>
  pivot_longer(cols = contains("MARG"), names_to = "contest", values_to = "margin") |>
  separate(contest, into = c("office","stat","year"), sep = c(3,-2)) |>
  mutate(year = as.numeric(paste0("20", year)),
         office = if_else(office == "PRE", "President", "US Senate"),
         fontcolor = if_else(margin < 0, "red", "blue"))

educ.caption.text <- paste("Graph and analysis by John D. Johnson (@jdjmke).",
                           "Data sources: Wisconsin Election Commission, unofficial",
                           "2024 election results, & 2018-2022 ACS data.",
                           "Reporting units are allocated to census tracts based on",
                           "registered voter addresses.", "Census tracts are then",
                           "assigned to quintiles based on the % of the population with at",
                           "least a bachelor's degree & weighted by # of votes cast in 2024.")
gg.educ.quintiles <- df.educ.quintiles |>
  ggplot(aes(year, margin, linetype = office, shape = office)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(color = fontcolor)) +
  geom_line() +
  ggrepel::geom_text_repel(aes(label = round(margin), color = fontcolor),
                           min.segment.length = 0.01) +
  scale_color_identity() +
  facet_wrap(facets = ~educ_quintile, nrow = 1) +
  labs(title = "Wisconsin Presidential & US Senate election results by share of the population with a bachelor's degree",
       subtitle = paste("Each column represents one-fifth of voters.", completion),
       caption = str_wrap(educ.caption.text, 160),
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
ggsave("2024-nov/wec/graphics/educ-quintiles.png", gg.educ.quintiles, width = 10, height = 6)

age.caption.text <- paste("Graph and analysis by John D. Johnson (@jdjmke).",
                          "Data sources: Wisconsin Election Commission, unofficial",
                          "2024 election results, & 2018-2022 ACS data.",
                          "Reporting units are allocated to census tracts based on",
                          "registered voter addresses.", "Census tracts are then",
                          "assigned to quintiles based on median age",
                          "& weighted by # of votes cast in 2024.")
gg.age.quintiles <- df.age.quintiles |>
  ggplot(aes(year, margin, linetype = office, shape = office)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(color = fontcolor)) +
  geom_line() +
  ggrepel::geom_text_repel(aes(label = round(margin), color = fontcolor),
                           min.segment.length = 0.01) +
  scale_color_identity() +
  facet_wrap(facets = ~age_quintile, nrow = 1) +
  labs(title = "Wisconsin Presidential & US Senate election results by median age",
       subtitle = paste("Each column represents one-fifth of voters.", completion),
       caption = str_wrap(age.caption.text, 160),
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
        legend.position.inside = c(0.93, 0.9),
        legend.background = element_rect(fill = "aliceblue"),
        plot.background = element_rect(fill = "aliceblue"),
        panel.background = element_rect(fill = "linen"))
ggsave("2024-nov/wec/graphics/age-quintiles.png", gg.age.quintiles, width = 10, height = 6)

income.caption.text <- paste("Graph and analysis by John D. Johnson (@jdjmke).",
                             "Data sources: Wisconsin Election Commission, unofficial",
                             "2024 election results, & 2018-2022 ACS data.",
                             "Reporting units are allocated to census tracts based on",
                             "registered voter addresses.", "Census tracts are then",
                             "assigned to quintiles based on per capita income",
                             "& weighted by # of votes cast in 2024.")
gg.income.quintiles <- df.income.quintiles |>
  ggplot(aes(year, margin, linetype = office, shape = office)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(color = fontcolor)) +
  geom_line() +
  ggrepel::geom_text_repel(aes(label = round(margin), color = fontcolor),
                           min.segment.length = 0.01) +
  scale_color_identity() +
  facet_wrap(facets = ~income_quintile, nrow = 1) +
  labs(title = "Wisconsin Presidential & US Senate election results by per capita income",
       subtitle = paste("Each column represents one-fifth of voters.", completion),
       caption = str_wrap(income.caption.text, 160),
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
        legend.position.inside = c(0.93, 0.9),
        legend.background = element_rect(fill = "aliceblue"),
        plot.background = element_rect(fill = "aliceblue"),
        panel.background = element_rect(fill = "linen"))
ggsave("2024-nov/wec/graphics/income-quintiles.png", gg.income.quintiles, width = 10, height = 6)

################################################################################
# explore tract locations
library(leaflet)
tract.shp <- tigris::tracts("WI", cb = T, year = 2022) |> select(tract_GEOID = GEOID) |> inner_join(tract.votes.with.quintiles) |> st_transform(crs = 4326)
quintile_pal <- colorFactor("PRGn", levels = c("Youngest", "Younger", "Medium", "Older", "Oldest"))

# age
tract.votes.with.quintiles |> group_by(age_quintile) |> summarise(count = n(), mean = mean(median_age, na.rm = T))
tract.shp |>
  leaflet() |>
  addTiles() |>
  addPolygons(fillColor = ~quintile_pal(age_quintile),
              color = ~quintile_pal(age_quintile),
              weight = 1, fillOpacity = 0.5,
              label = ~median_age) |>
  addLegend(position = "bottomleft", pal = quintile_pal, values = ~age_quintile)

# education
educ_pal <- colorFactor("PRGn", levels = levels(tract.shp$educ_quintile))
tract.votes.with.quintiles |> group_by(educ_quintile) |> summarise(count = n(), mean = round(mean(pct_ba, na.rm = T)))
tract.shp |>
  leaflet() |>
  addTiles() |>
  addPolygons(fillColor = ~educ_pal(educ_quintile),
              color = ~educ_pal(educ_quintile),
              weight = 1, fillOpacity = 0.5,
              label = ~pct_ba) |>
  addLegend(position = "bottomleft", pal = educ_pal, values = ~educ_quintile)

# income
inc_pal <- colorFactor("PRGn", levels = levels(tract.shp$income_quintile))
tract.votes.with.quintiles |> group_by(educ_quintile) |> summarise(count = n(), mean = mean(percapita_income, na.rm = T))
tract.shp |>
  leaflet() |>
  addTiles() |>
  addPolygons(fillColor = ~inc_pal(income_quintile),
              color = ~inc_pal(income_quintile),
              weight = 1, fillOpacity = 0.5,
              label = ~percapita_income) |>
  addLegend(position = "bottomleft", pal = inc_pal, values = ~income_quintile)
