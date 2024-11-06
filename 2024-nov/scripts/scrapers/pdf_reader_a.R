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
    
    if(str_detect(pdfpath, "Portage")){
      reporting.unit <- page$X1[which(str_detect(str_to_upper(page$X1),
                                                 "NOVEMBER 5, 2024|NOVEMBER 5,2024")) + 2]
    } else {
      reporting.unit <- page$X1[which(str_detect(str_to_upper(page$X1),
                                                 "NOVEMBER 5, 2024|NOVEMBER 5,2024")) + 1]
    }
    race.start <- min(which(str_detect(page$X1, "Vote For")))-1
    
    page |>
      mutate(office = if_else(str_detect(lead(X1, 1), "Vote For"),
                              true = str_squish(X1),
                              false = NA)) |>
      filter(row_number() >= race.start,
             str_detect(X1, "Precinct Report", negate = T)) |>
      mutate(office = zoo::na.locf(office),
             X1 = str_squish(X1),
             # this repairs situations where RFK is split across multiple lines (as in Columbia county)
             rfk = case_when(
               X1 == "IND Robert F. Kennedy, Jr. / Nicole" &
                 lead(X1, 2) == "Shanahan" ~ "rfk1",
               lag(X1, 1) == "IND Robert F. Kennedy, Jr. / Nicole" &
                 lead(X1, 1) == "Shanahan" ~ "rfk3",
               X1 == "Shanahan" ~ "rfk2",
               TRUE ~ NA
             )) |>
      mutate(X1 = case_when(
        rfk == "rfk1" ~ paste(X1, lead(X1, 2), lead(X1, 1)),
        TRUE ~ X1)) |>
      filter(! rfk %in% c("rfk2","rfk3")) |>
      select(-rfk) |>
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
           across(where(is.character), str_squish)) |>
    filter(! candidate %in% c("UNDERVOTES", "TOTAL VOTES CAST", "OVERVOTES", "CONTEST TOTALS"))
  
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
