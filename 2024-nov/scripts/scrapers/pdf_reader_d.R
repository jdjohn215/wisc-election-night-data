library(tidyverse)

pdf_reader_d <- function(workbookpath, startsheet, endsheet, save_output = T){
  # this function reads an individual sheet
  read_sheet <- function(sheetindex, workbookpath){
    sheet <- read_excel(workbookpath, sheet = sheetindex,
                        col_names = F, .name_repair = "unique_quiet")
    
    startrow <- which(str_detect(sheet$...1, "^Municipality"))
    
    contest <- case_when(
      startrow == 1 ~ NA,
      # if row 1 is an INTERNAL USE ONLY header, get contest name from column 2
      str_detect(unite(sheet[1,], "col", na.rm = T), "FOR INTERNAL USE ONLY") ~ unite(sheet[2,], "col", na.rm = T)[[1]],
      TRUE ~ unite(sheet[1,], "col", na.rm = T)[[1]]
    )
    contest <- str_replace_all(contest, "\n", " ")
    
    # create empty tibble if the table on this page is only a total row
    if(sheet$...1[startrow+1] == "Total"){
      tibble(municipality = character(),
             reporting_unit = character(),
             votes = character(),
             contest = character())
    } else {
      read_excel(workbookpath, sheet = sheetindex, skip = (startrow-1), col_names = T,
                 .name_repair = "unique_quiet", col_types = "text") |>
        janitor::remove_empty("cols") |>
        rename(municipality = 1, reporting_unit = 2) |>
        filter(! municipality %in% c("Total", "TOTAL")) |>
        pivot_longer(cols = -c(municipality, reporting_unit),
                     names_to = "candidate", values_to = "votes") |>
        mutate(contest = contest,
               candidate = str_remove_all(candidate, coll("\n")))
    }
  }
  
  all.results <- map(.x = startsheet:endsheet,
                     .f = ~read_sheet(.x, workbookpath),
                     .progress = TRUE) |>
    list_rbind() |>
    type_convert() |>
    mutate(contest = zoo::na.locf(contest),
           contest = str_replace_all(contest, coll("\n"), " "),
           reporting_unit = paste(municipality, reporting_unit)) |>
    select(reporting_unit, contest, candidate, votes) |>
    mutate(county = word(word(workbookpath, -1, sep = "/"), 1, sep = " 20"),
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           municipality = str_remove(reporting_unit, "VILLAGE OF |TOWN OF |CITY OF |^T |^C |^V "),
           municipality = word(municipality, 1, 1, sep = "\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"))
  
  #################################################
  # save the output with the timestamped file name
  if(save_output == T){
    write_csv(all.results, paste0("2024-nov/raw-processed/",
                                  str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  all.results
}


