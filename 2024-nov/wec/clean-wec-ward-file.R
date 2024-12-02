rm(list = ls())

library(tidyverse)

# This script cleans the official Wisconsin Elections Commission reporting unit file
#   for the November 2024 general election.
#   Each contest is on a different sheet of the XLSX file, so this script
#     processes each sheet, and combines them into a single CSV file 
#     with a standardized format
#   The script then adds the state assembly, state senate, & US house district
#     assignments for each reporting unit

# the list of sheets. Sheet 1 is the document map (doesn't contain election results)
orig.sheets <- readxl::excel_sheets("2024-nov/wec/Ward by Ward Report_November 5 2024 General Election_Federal and State Contests.xlsx")

# this function reads each sheet individually and converts it to long format
#   with columns for county, reporting unit, contest, total votes, party,
#   candidate name, and candidate votes
read_sheet <- function(sheetindex){
  # read the original sheet in its entirety
  s1 <- readxl::read_excel("2024-nov/wec/Ward by Ward Report_November 5 2024 General Election_Federal and State Contests.xlsx",
                           col_names = F, .name_repair = "unique_quiet",
                           sheet = sheetindex) |>
    # remove any columns with only NA values
    janitor::remove_empty(which = "cols")
  
  # the rows containing header information
  #   header info is spread across multiple rows, so we must combine them
  header.start <- which(s1$...3 == "Total Votes Cast")
  header.end <- min(which(!is.na(s1$...2))) - 1
  
  # the cell containing the contest name
  contest.name <- s1$...1[header.start-2]
  
  # combine the multi-row headers into a single row
  colnames <- s1 |>
    filter(row_number() %in% header.start:header.end) |>
    mutate(rownum = row_number()) |>
    pivot_longer(cols = -rownum) |>
    pivot_wider(names_from = rownum, values_from = value) |>
    # some munging because the county and reporting unit columns aren't labelled
    #   plus the scattering column lacks party info
    mutate(`1` = case_when(
      name == "...1" ~ "county",
      name == "...2" ~ "reporting_unit",
      name == "...3" ~ "total",
      `2` == "SCATTERING" ~ "NP", # "NP" = "NO PARTY"
      TRUE ~ `1`
    )) |>
    unite("colname", `1`, `2`, na.rm = T, sep = "!!") |>
    mutate(colname = str_remove_all(colname, coll("\r\n")),
           colname = str_squish(colname))
  
  # just keep the parts of the sheet with the actual vote table
  s1 |>
    filter(row_number() > header.end) |> # keep everything after the header
    set_names(colnames$colname) |> # use the new column names (processed above)
    mutate(county = zoo::na.locf(county)) |> # fill the county values
    filter(str_detect(county, "Totals", negate = T), # remove the total rows
           str_detect(reporting_unit, "Totals", negate = T)) |>
    # convert to long format
    pivot_longer(cols = -c(county, reporting_unit, total),
                 names_to = "party!!candidate", values_to = "votes") |>
    # create separate party and candidate fields
    separate(col = `party!!candidate`, into = c("party", "candidate"), sep = "!!") |>
    mutate(contest = contest.name) |> # add the contest name as a field
    # reorder the values
    select(county, reporting_unit, contest, total, party, candidate, votes)
}

# read every sheet and combine the results into a single dataframe
#   the map() function supplied each value in .x to the function indicated in .f
all.sheets <- map(.x = 2:length(orig.sheets),
                  .f = read_sheet,
                  .progress = TRUE) |>
  list_rbind() |>
  type_convert()

################################################################################
# add the district codes for assembly (wsa), state senate (wss), and congress (ush)
wsa.districts <- all.sheets |>
  filter(str_detect(contest, "ASSEMBLY")) |>
  mutate(wsa = as.numeric(word(contest, -1))) |>
  group_by(county, reporting_unit, wsa) |>
  summarise(.groups = "drop") |>
  # add state senate districts (wss) following the nested district assignments
  #   i.e. wsa 1-3 = wss 1, wsa 4-6 = wss 2, etc.
  inner_join(
    tibble(wsa = 1:99,
           wss = rep(1:33, each = 3))
    )

ush.districts <- all.sheets |>
  filter(str_detect(contest, "REPRESENTATIVE IN CONGRESS")) |>
  # extract the word following "DISTRICT" in the contest string
  mutate(ush = as.numeric(str_extract(contest, '(?<=DISTRICT\\s)\\w+'))) |>
  group_by(county, reporting_unit, ush) |>
  summarise(.groups = "drop")

all.sheets.with.districts <- all.sheets |>
  inner_join(wsa.districts) |>
  inner_join(ush.districts) |>
  # some basic string cleaning
  mutate(across(where(is.character), str_to_upper), # upper case
         across(where(is.character), str_squish)) # remove double white spaces

################################################################################
# save the output
write_csv(all.sheets.with.districts, "2024-nov/wec/wec-original-all-races-long-format.csv")
