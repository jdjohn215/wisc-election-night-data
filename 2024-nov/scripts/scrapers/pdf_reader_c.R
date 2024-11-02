library(tidyverse)
library(readxl)

pdf_reader_c <- function(workbookpath, save_output = T){
  # this function reads an individual sheet
  read_sheet <- function(sheetindex, workbookpath){
    sheet <- read_excel(workbookpath, sheet = sheetindex,
                        col_names = F, .name_repair = "unique_quiet")
    
    # the row which reads "VOTE FOR 1"
    votefor1 <- which(sheet$...2 == "VOTE FOR 1")
    
    # if there is no race on this sheet (e.g. cards cast summary),
    #   then just make empty tibble
    if(length(votefor1) == 0){
      tibble(reporting_unit = character(),
             race_candidate = character(),
             votes = character())
    } else{
      sheet2 <- sheet |> mutate(rownum = row_number()) |> filter(rownum > votefor1)
      startrow <- sheet2 |> filter(str_detect(...1, "^C|^T|^V")) |> pull(rownum) |> min()
      
      race.and.candidates <- sheet |>
        filter(row_number() %in% c(votefor1-1, startrow-1)) |>
        mutate(rownum = row_number()) |>
        pivot_longer(cols = -rownum, names_to = "column") |>
        pivot_wider(names_from = rownum, values_from = value) |>
        filter(column != "...1") |>
        mutate(`1` = zoo::na.locf(`1`),
               colname = paste(`1`, `2`, sep = "!!")) |>
        pull(colname)
      
      sheet |>
        filter(row_number() >= startrow) |>
        set_names(c("reporting_unit", race.and.candidates)) |>
        pivot_longer(cols = -reporting_unit, names_to = "race_candidate",
                     values_to = "votes") |>
        filter(!is.na(votes)) |>
        separate(col = race_candidate, sep = "!!", into = c("contest","candidate"))
    }
  }
  
  all.results <- map(.x = 1:length(excel_sheets(workbookpath)),
                     .f = ~read_sheet(.x, workbookpath),
                     .progress = TRUE) |>
    list_rbind() |>
    type_convert() |>
    select(reporting_unit, contest, candidate, votes) |>
    filter(reporting_unit != "Totals") |>
    mutate(county = word(word(workbookpath, -1, sep = "/"), 1, sep = " 20"),
           across(where(is.character), str_to_upper),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           ctv = str_sub(reporting_unit, 1, 1),
           municipality = str_remove(reporting_unit, "VILLAGE OF |TOWN OF |CITY OF |^T |^C |^V |^T-|^C-|^V-"),
           municipality = word(municipality, 1, 1, sep = "\\bW[0-9]|\\bWARD|\\bWD|\\bD[0-9]|\\bW\\b|\\bDISTRICT"),
           across(where(is.character), str_squish)) |>
    filter(! candidate %in% c("TOTAL VOTES CAST", "OVERVOTES", "UNDERVOTES", "CONTEST TOTAL"))
  
  #################################################
  # save the output with the timestamped file name
  if(save_output == T){
    write_csv(all.results, paste0("2024-nov/raw-processed/",
                                  str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  all.results
}
