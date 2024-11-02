library(tidyverse)
library(pdftools)

# pdf reader a, works on Douglas County, etc

pdf_reader_a <- function(pdfpath, save_output = T){
  #################################################
  # read all the data from the PDF
  pagelist <- pdftools::pdf_text(pdfpath)
  
  #################################################
  # this function processes a single page of the pdf
  read_pdf_page <- function(pageindex, pagelist){
    page <- pagelist[pageindex] |> read_delim(col_names = F, delim = "!!",
                                              col_types = cols(.default = "c"))
    
    reporting.unit <- page$X1[which(str_detect(str_to_upper(page$X1),
                                               "AUGUST 13, 2024|AUGUST 13,2024")) + 1]
    race.start <- min(which(str_detect(page$X1, "Vote For")))-1
    
    page |>
      mutate(office = if_else(str_detect(lead(X1, 1), "Vote For"),
                              true = str_squish(X1),
                              false = NA)) |>
      filter(row_number() >= race.start) |>
      mutate(office = zoo::na.locf(office),
             X1 = str_squish(X1)) |>
      group_by(office) |>
      filter(row_number() > 3) |>
      ungroup() |>
      filter(str_detect(X1, "Precinct Summary|Report generated with", negate = T)) |>
      mutate(
        pct = case_when(
          str_detect(word(X1, -1), "%") ~ word(X1, -1),
          TRUE ~ NA),
        votes = case_when(
          str_detect(word(X1, -1), "%") ~ word(X1, -2),
          !is.na(as.numeric(str_remove(word(X1, -1), ","))) ~ word(X1, -1),
          TRUE ~ NA),
        candidate = case_when(
          str_detect(word(X1, -1), "%") ~ word(X1, 1, -3),
          !is.na(as.numeric(str_remove(word(X1, -1), ","))) ~ word(X1, 1, -2),
          TRUE ~ X1)
      ) |>
      select(contest = office, candidate, votes) |>
      mutate(reporting_unit = reporting.unit)
  }
  
  #################################################
  # process each page of the PDF in turn
  all.results.long <- map(.x = 1:length(pagelist), .f = ~read_pdf_page(.x, pagelist),
                          .progress = T) |>
    list_rbind() |>
    filter(!is.na(votes)) |>
    type_convert() |>
    mutate(county = word(word(pdfpath, -1, sep = "/"), 1, sep = " 20")) |>
    mutate(across(where(is.character), str_to_upper),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           ctv = str_sub(reporting_unit, 1, 1),
           municipality = str_remove(reporting_unit, "TOWN OF |VILLAGE OF |CITY OF |^V OF |^C OF |^V OF "),
           municipality = word(municipality, 1, 1, sep = "\\bWARD|\\bWD|\\bWARD|\\bD[0-9]|\\bW[0-9]|\\bW\\b|\\bW[0-9]"),
           municipality = str_remove_all(municipality, coll("-")),
           municipality = str_remove_all(municipality, coll(",")),
           across(where(is.character), str_squish))
  
  #################################################
  # save the output with the timestamped file name
  if(save_output == T){
    write_csv(all.results.long, paste0("2024-nov/raw-processed/",
                                       str_remove(word(pdfpath, -1, sep = "/"), ".pdf"), ".csv"))
  }
  
  #################################################
  # show the results (useful for quickly confirming that votes are numeric)
  all.results.long
}
