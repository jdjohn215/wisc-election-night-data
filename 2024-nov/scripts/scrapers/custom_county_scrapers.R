library(tidyverse)
library(readxl)

################################################################################
read_ashland <- function(workbookpath, save_output = T){
  
  sheet.list <- excel_sheets(workbookpath)
  
  read_sheet <- function(sheetindex){
    sheet <- read_excel(workbookpath, sheet = sheetindex,
                        col_names = F, .name_repair = "unique_quiet")
    
    start.row <- min(which(str_detect(sheet$...1, "^TOWN|^VILLAGE|^CITY")))
    header <- sheet$...2[start.row-2]
    
    colnames <- sheet |>
      filter(row_number() %in% c(start.row-2, start.row-1)) |>
      mutate(rownum = row_number()) |>
      pivot_longer(cols = -rownum, names_to = "column") |>
      mutate(value = str_replace_all(value, coll("\n"), " ")) |>
      pivot_wider(names_from = rownum, values_from = value) |>
      mutate(`1` = zoo::na.locf(`1`, na.rm = F),
             colname = paste(header, `1`, `2`, sep = "_"),
             colname = str_replace_all(colname, "\n", " "))
    
    sheet |>
      filter(row_number() >= start.row) |>
      set_names(colnames$colname) |>
      janitor::remove_empty("cols") |>
      rename(reporting_unit = 1) |>
      select(-ends_with("NA")) |>
      rename(reporting_unit = 1) |>
      filter(str_detect(reporting_unit, "^TOWN|^VILLAGE|^CITY")) |>
      pivot_longer(cols = -reporting_unit, names_to = "contestcandidate", values_to = "votes") |>
      filter(!is.na(votes)) |>
      separate(contestcandidate, sep = "_(?!.*_)", into = c("contest", "candidate"))
  }
  all.output <- map(.x = 1:length(sheet.list),
      .f = ~read_sheet(.x)) |>
    list_rbind() |>
    type_convert() |>
    mutate(county = "Ashland",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|TOWN OF |VILLAGE OF |CITY OF "),
           municipality = word(municipality, 1, sep = "\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"))
  
  if(save_output == TRUE){
    write_csv(all.output, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
}
################################################################################

################################################################################
read_barron <- function(workbookpath, save_output = T){
  sheet <- read_excel(workbookpath, col_names = F,
                      .name_repair = "unique_quiet")
  sheet
  start.row <- min(which(sheet$...1 == "Total"))
  
  colnames <- sheet |>
    filter(row_number() %in% c(start.row-2, start.row-1)) |>
    mutate(rownum = row_number()) |>
    pivot_longer(cols = -rownum, names_to = "column") |>
    mutate(value = str_replace_all(value, coll("\n"), " ")) |>
    pivot_wider(names_from = rownum, values_from = value) |>
    mutate(`1` = zoo::na.locf(`1`, na.rm = F),
           colname = paste(`1`, `2`, sep = "_"))
  
  all.output <- sheet |>
    filter(row_number() > start.row) |> # drops the total row
    set_names(colnames$colname) |>
    janitor::remove_empty("cols") |>
    rename(reporting_unit = 1) |>
    pivot_longer(cols = -reporting_unit, names_to = "contestcandidate", values_to = "votes") |>
    separate(contestcandidate, into = c("contest", "candidate"), sep = "_(?!.*_)") |>
    type_convert() |>
    mutate(county = "Barron",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|TOWN OF |VILLAGE OF |CITY OF "),
           municipality = word(municipality, 1, sep = "\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"))
  
  if(save_output == TRUE){
    write_csv(all.output, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  all.output
}
################################################################################

################################################################################
read_clark <- function(workbookpath, save_output = T){
  sheetvector <- excel_sheets(workbookpath)
  
  process_sheet <- function(sheetindex){
    sheet <- read_excel(workbookpath, col_names = F, sheet = sheetindex,
                        .name_repair = "unique_quiet")
    start.row <- which(sheet$...1 == "Beaver")
    
    colnames <- sheet |>
      filter(row_number() < start.row) |>
      mutate(rownum = row_number()) |>
      pivot_longer(cols = -rownum, names_to = "column") |>
      pivot_wider(names_from = rownum, values_from = value) |>
      # this will likely need editing for the general election
      mutate(`1` = zoo::na.locf(`1`, na.rm = F),
             `2` = zoo::na.locf(`2`, na.rm = F)) |>
      unite(col = "colname", `2`, `3`, na.rm = T)
    
    sheet |>
      filter(row_number() >= start.row) |>
      set_names(colnames$colname) |>
      select(unique(colnames$colname)) |>
      rename(reporting_unit = Municipality) |>
      select(-contains("Municipality")) |>
      select(-any_of(c("Votes Cast", "OutStanding Provisionals", "Outstanding Provisionals"))) |>
      filter(!is.na(reporting_unit),
             reporting_unit != "Totals") |>
      pivot_longer(cols = -reporting_unit, names_to = "contestcandidate", values_to = "votes") |>
      separate(contestcandidate, into = c("contest", "candidate"), sep = "_(?!.*_)")
  }
  
  all.output <- map(.x = sheetvector,
                    .f = ~process_sheet(.x)) |>
    list_rbind() |>
    type_convert() |>
    mutate(county = "Clark",
           across(where(is.character), str_to_upper),
           ctv = if_else(str_sub(reporting_unit, 2, 2) == ".",
                         true = str_sub(reporting_unit, 1, 1),
                         false = "T"),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|TOWN OF |VILLAGE OF |CITY OF |^C |^V |^T "),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\b-W|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           municipality = str_remove(municipality, coll("-")),
           municipality = str_replace(municipality, "VDORCHESTER", "DORCHESTER"))
  
  if(save_output == TRUE){
    write_csv(all.output, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  all.output
}

################################################################################

################################################################################
read_bayfield <- function(workbookpath, sheetvector, save_output = T){
 process_sheet <- function(sheetindex){
   sheet <- read_excel(workbookpath, sheet = sheetindex, col_names = FALSE,
                       .name_repair = "unique_quiet")
   repunit.column <- which(str_detect(sheet, "Barksdale"))
   start.row <- which(sheet[,repunit.column] == "Barksdale")
   colnames <- sheet |>
     filter(row_number() < start.row) |>
     mutate(rownum = row_number()) |>
     pivot_longer(cols = -rownum, names_to = "column") |>
     mutate(value = str_replace_all(value, coll("\n"), " ")) |>
     pivot_wider(names_from = rownum, values_from = value) |>
     mutate(across(.cols = where(is.character), ~zoo::na.locf(.x, na.rm = F))) |>
     select(-column) |>
     unite(col = "colname", na.rm = T)
   sheet |>
     filter(row_number() >= start.row) |>
     set_names(colnames$colname) |>
     select(all_of(repunit.column:ncol(sheet))) |>
     rename(reporting_unit = 1) |>
     filter(reporting_unit != "TOTALS") |>
     pivot_longer(cols = -reporting_unit, names_to = "contestcandidate", values_to = "votes") |>
     separate(contestcandidate, into = c("contest", "candidate"), sep = "_(?!.*_)") |>
     mutate(votes = case_when(
       votes == "-" ~ "0",
       votes == "****" ~ NA,
       TRUE ~ votes
     )) |>
     filter(!is.na(votes))
 }
 
 all.output <- map(.x = sheetvector,
     .f = ~process_sheet(.x)) |>
   list_rbind() |>
   type_convert() |>
   filter(str_detect(reporting_unit, "No Voters", negate = T)) |>
   mutate(county = "Bayfield",
          across(where(is.character), str_to_upper),
          ctv = case_when(
            str_detect(reporting_unit, "CITY OF") ~ "C",
            str_detect(reporting_unit, "VILLAGE OF") ~ "V",
            str_detect(reporting_unit, "TOWN OF") ~ "T",
            TRUE ~ "T"
          ),
          municipality = str_remove(reporting_unit, "\\s*\\([^\\)]+\\)"), # remove parentheticals containing CTV
          municipality = word(municipality, 1, sep = "\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"))
 
 if(save_output == TRUE){
   write_csv(all.output, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
 }
 all.output
}

################################################################################
read_polk <- function(workbookpath, save_output = T){
  # this function might need to be edited based on the number of rows in the column header
  polk.orig <- readxl::read_excel(workbookpath, col_names = F,
                                  col_types = "text", .name_repair = "unique_quiet")
  
  # process multi-row header
  header.start <-  which(str_detect(polk.orig$...2, "Voter Participation"))
  header.end <-  which(str_detect(polk.orig$...2, "Municipality"))
  
  colnames <- polk.orig[header.start:header.end,] |>
    mutate(rownum = row_number()) |>
    pivot_longer(cols = -rownum) |>
    pivot_wider(names_from = rownum, values_from = value) |>
    select(-name) |>
    mutate(`1` = replace(`1`, is.na(`1`) & is.na(`2`) & is.na(`3`), "NA"),
           `2` = replace(`2`, is.na(`1`) & is.na(`2`) & is.na(`3`), "NA"),
           `1` = zoo::na.locf(`1`, na.rm = F),
           `2` = zoo::na.locf(`2`, na.rm = F)) |>
    unite("header", everything(), sep = "_", na.rm = T)
  
  dtemp <- readxl::read_excel(workbookpath, col_types = "text",
                              skip = header.end-1, col_names = colnames$header,
                              .name_repair = "unique_quiet") |>
    janitor::remove_empty("cols") |>
    filter(row_number() > 1) |>
    rename(wardno = 1, reporting_unit = 2, rv = 3, turnout = 4, pturnout = 5, prov_ballots = 6) |>
    filter(!is.na(wardno)) |>
    select(-c(wardno, rv, turnout, pturnout, prov_ballots, contains("Reporting Units"),
              contains("turn out"), contains("Provisional"))) |>
    pivot_longer(cols = -c(reporting_unit),
                 names_to = "contestcandidate", values_to = "votes") |>
    separate(contestcandidate, sep = "_(?!.*_)", into = c("contest", "candidate")) |>
    mutate(contest = if_else(candidate %in% c("Yes","No","v"), str_remove(contest, "_Treasurer"),
                             str_remove(contest, "^NA_"))) |>
    type_convert() |>
    select(reporting_unit, contest, candidate, votes) |>
    mutate(county = "Polk",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|TOWN OF |VILLAGE OF |CITY OF |^T |^V |^C "),
           municipality = word(municipality, 1, sep = "\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_pierce <- function(workbookpath, save_output = T){
  # this function might need to be edited based on the number of rows in the column header
  orig <- readxl::read_excel(workbookpath, col_names = F,
                             col_types = "text", .name_repair = "unique_quiet")
  
  # process multi-row header
  header.start <- 1
  header.end <-  first(which(str_detect(orig$...2, "Town of"))) -1
  
  colnames <- orig[header.start:header.end,] |>
    mutate(rownum = row_number()) |>
    pivot_longer(cols = -rownum) |>
    pivot_wider(names_from = rownum, values_from = value) |>
    select(-name) |>
    mutate(`1` = zoo::na.locf(`1`, na.rm = F),
           `2` = zoo::na.locf(`2`, na.rm = F)) |>
    unite("header", everything(), sep = "_")
  
  dtemp <- readxl::read_excel(workbookpath, col_types = "text",
                              skip = header.end-1, col_names = colnames$header,
                              .name_repair = "unique_quiet") |>
    filter(row_number() > 1) |>
    janitor::remove_empty("cols") |>
    select(-1) |>
    rename(reporting_unit = 1) |>
    filter(reporting_unit != "TOTALS") |>
    pivot_longer(cols = -c(reporting_unit),
                 names_to = "contestcandidate", values_to = "votes") |>
    mutate(votes = na_if(votes, "-")) |>
    mutate(contestcandidate = str_replace_all(contestcandidate, coll("\r\n"), " ")) |>
    separate(contestcandidate, sep = "_(?!.*_)", into = c("contest", "candidate")) |>
    filter(candidate != "NA") |>
    type_convert() |>
    mutate(county = "Pierce",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|TOWN OF |VILLAGE OF |CITY OF "),
           municipality = word(municipality, 1, sep = "\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
# read_oneida <- function(workbookpath, sheetno, save_output = T){
#   # this function might need to be edited based on the number of rows in the column header
#   orig <- readxl::read_excel(workbookpath, col_names = F, sheet = sheetno,
#                              col_types = "text", .name_repair = "unique_quiet")
#   
#   # process multi-row header
#   header.start <- 1
#   header.end <-  first(which(str_detect(orig$...1, "Totals"))) -1
#   
#   colnames <- orig[header.start:header.end,] |>
#     mutate(rownum = row_number()) |>
#     pivot_longer(cols = -rownum) |>
#     pivot_wider(names_from = rownum, values_from = value) |>
#     select(-name) |>
#     mutate(`1` = replace(`1`, is.na(`1`) & is.na(`2`) & is.na(`3`) & is.na(`4`) & is.na(`5`), "NA"),
#            `2` = replace(`2`, is.na(`1`) & is.na(`2`) & is.na(`3`) & is.na(`4`) & is.na(`5`), "NA"),
#            `3` = replace(`3`, is.na(`1`) & is.na(`2`) & is.na(`3`) & is.na(`4`) & is.na(`5`), "NA"),
#            `4` = replace(`4`, is.na(`1`) & is.na(`2`) & is.na(`3`) & is.na(`4`) & is.na(`5`), "NA"),
#            `1` = zoo::na.locf(`1`, na.rm = F),
#            `2` = zoo::na.locf(`2`, na.rm = F),
#            `3` = zoo::na.locf(`3`, na.rm = F),
#            `4` = zoo::na.locf(`4`, na.rm = F)) |>
#     unite("header", everything(), sep = "_")
#   
#   dtemp <- readxl::read_excel(workbookpath, col_types = "text", sheet = sheetno,
#                               skip = header.end-1, col_names = colnames$header,
#                               .name_repair = "unique_quiet") |>
#     filter(row_number() > 1) |>
#     janitor::remove_empty("cols") |>
#     rename(reporting_unit = 1, total_voters = 2, pct_report = 3, prov_ballots = 4) |>
#     filter(reporting_unit != "Totals") |>
#     select(-c(total_voters, pct_report, prov_ballots)) |>
#     pivot_longer(cols = -c(reporting_unit),
#                  names_to = "contestcandidate", values_to = "votes") |>
#     # remove cells with referendum text, Oneida county includes them on the spreadsheet outside the table
#     filter(str_detect(votes, "Question", negate = T)) |>
#     mutate(contestcandidate = str_replace_all(str_squish(contestcandidate), coll("\r\n"), " "),
#            contestcandidate = str_remove(contestcandidate, "_NA$")) |>
#     separate(contestcandidate, sep = "_(?!.*_)", into = c("contest", "candidate")) |>
#     filter(candidate != "NA") |>
#     type_convert() |>
#     mutate(county = "Oneida",
#            across(where(is.character), str_to_upper),
#            ctv = str_sub(reporting_unit, 1, 1),
#            reporting_unit = str_remove_all(reporting_unit, coll(".")),
#            municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|TOWN OF |VILLAGE OF |CITY OF |^T |^C |^V "),
#            municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"))
#   
#   if(save_output == TRUE){
#     write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
#   }
#   dtemp
# }
################################################################################

################################################################################
read_vernon <- function(workbookpath, sheetno = 2, save_output = T){
  # this function might need to be edited based on the number of rows in the column header
  orig <- readxl::read_excel(workbookpath, col_names = F, sheet = sheetno,
                             col_types = "text", .name_repair = "unique_quiet")
  
  # process multi-row header
  header.start <- 1
  header.end <-  first(which(str_detect(orig$...1, "Town of"))) -1
  
  colnames <- orig[header.start:header.end,] |>
    mutate(rownum = row_number()) |>
    pivot_longer(cols = -rownum) |>
    pivot_wider(names_from = rownum, values_from = value) |>
    select(-name) |>
    mutate(`1` = replace(`1`, is.na(`1`) & is.na(`2`) & is.na(`3`), "NA"),
           `2` = replace(`2`, is.na(`1`) & is.na(`2`) & is.na(`3`), "NA"),
           `1` = zoo::na.locf(`1`, na.rm = F),
           `2` = zoo::na.locf(`2`, na.rm = F)) |>
    unite("header", everything(), sep = "_")
  
  dtemp <- readxl::read_excel(workbookpath, col_types = "text", sheet = sheetno,
                              skip = header.end-1, col_names = colnames$header,
                              .name_repair = "unique_quiet") |>
    filter(row_number() > 1) |>
    janitor::remove_empty("cols") |>
    rename(reporting_unit = 1) |>
    filter(reporting_unit != "Total") |>
    pivot_longer(cols = -c(reporting_unit),
                 names_to = "contestcandidate", values_to = "votes") |>
    mutate(contestcandidate = str_replace_all(str_squish(contestcandidate), coll("\r\n"), " ")) |>
    # remove provisional ballots column
    filter(str_detect(contestcandidate, "PROVISIONAL", negate = T)) |>
    separate(contestcandidate, sep = "_(?!.*_)", into = c("contest", "candidate")) |>
    mutate(votes = na_if(votes, "-")) |>
    filter(!is.na(votes)) |>
    type_convert() |>
    mutate(county = "Vernon",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|TOWN OF |VILLAGE OF |CITY OF "),
           municipality = word(municipality, 1, sep = "\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_waushara <- function(workbookpath, sheetvector, save_output = T){
  process_sheet <- function(sheetindex){
    read_excel(workbookpath, sheet = sheetindex, col_names = T, col_types = "text") |>
      rename(reporting_unit = 1) |>
      filter(str_detect(reporting_unit, "TOTAL|Total", negate = T)) |>
      pivot_longer(cols = -reporting_unit, names_to = "contestcandidate", values_to = "votes") |>
      filter(!is.na(votes)) |>
      mutate(contestcandidate = str_replace_all(contestcandidate, coll("\n"), " "),
             # separate by the string of offices in this election
             candidate = word(contestcandidate, -1, sep = "President |Senator |6 |39 |57 |Attorney |Clerk |Treasurer |Deeds |Power |Moneys "),
             contestcandidate = str_remove(contestcandidate, paste0(" ", candidate, "$"))) |>
      rename(contest = contestcandidate)
  }
  
  dtemp <- map(.x = sheetvector,
      .f = ~process_sheet(.x)) |>
    list_rbind() |>
    type_convert() |>
    mutate(county = "Waushara",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           reporting_unit = str_remove_all(reporting_unit, coll("-")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|TOWN OF |VILLAGE OF |CITY OF |^T |^V |^C "),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           municipality = str_remove_all(municipality, coll("-")),
           municipality = str_replace(municipality, "MT MORRIS", "MOUNT MORRIS")) |>
    filter(str_detect(candidate, coll("#"), negate = T),
           ! contest %in% c("PERCENTAGE VOTER TURNOUT"),
           ! candidate %in% c("PERCENTAGE VOTER TURNOUT"))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_oconto <- function(workbookpath, save_output = T){
  # this function requires custom headers
  sheet1.orig <- readxl::read_excel(workbookpath, col_names = F, sheet = 5,
                                    col_types = "text", .name_repair = "unique_quiet")
  
  headers <- c("candidate", "T Abrams W1-3", "T Bagley W1", "T Brazeau W1-3", "T Breed W1", "T Chase W1-5",
    "T Doty W1", "T Gillett W1-2", "T How W1-2", "T Lakewood W1", "T Lena W1",
    "T Lt River W1-2", "T Lt Suamico W1-8", "T Maple Valley W1", "T Morgan W1-2",
    "T Mountain W1", "T Oconto W1-3", "T Oconto Falls W1-2", "T Pensaukee W1-2",
    "T Riverview W1-2", "T Spruce W1-2", "T Stiles W1-2", "T Townsend W1",
    "T Underhill W1", "V Lena W1", "V Pulaski W5", "V Suring W1", "C Gillett W1-3",
    "C Oconto W1-7", "C Oconto Falls W1-6", "TOTALS")
  
  startrow <- min(which(str_detect(sheet1.orig$...1, "President")))
  
  sheet1 <- sheet1.orig |>
    filter(row_number() >= startrow) |>
    set_names(headers) |>
    mutate(contest = if_else(row_number() == 1 | lag(candidate, 1) == "Undervotes",
                             true = candidate, false = NA),
           contest = zoo::na.locf(contest)) |>
    select(contest, everything()) |>
    filter(contest != candidate,
           ! candidate %in% c("Totals", "Overvotes", "Undervotes")) |>
    pivot_longer(cols = -c(contest, candidate), names_to = "reporting_unit",
                 values_to = "votes")
  
  sheet2 <- readxl::read_excel(workbookpath, col_names = headers, sheet = 6,
                               col_types = "text", .name_repair = "unique_quiet") |>
    mutate(contest = if_else(row_number() == 1 | lag(candidate, 1) == "Undervotes",
                             true = candidate, false = NA),
           contest = zoo::na.locf(contest)) |>
    select(contest, everything()) |>
    filter(contest != candidate,
           ! candidate %in% c("Totals", "Overvotes", "Undervotes")) |>
    pivot_longer(cols = -c(contest, candidate), names_to = "reporting_unit",
                 values_to = "votes")
  
  sheet3 <- readxl::read_excel(workbookpath, col_names = headers, sheet = 7,
                               col_types = "text", .name_repair = "unique_quiet") |>
    mutate(contest = if_else(row_number() == 1 | lag(candidate, 1) == "Undervotes",
                             true = candidate, false = NA),
           contest = zoo::na.locf(contest)) |>
    select(contest, everything()) |>
    filter(contest != candidate,
           ! candidate %in% c("Totals", "Overvotes", "Undervotes")) |>
    pivot_longer(cols = -c(contest, candidate), names_to = "reporting_unit",
                 values_to = "votes")
  
  dtemp <- bind_rows(sheet1, sheet2, sheet3) |>
    mutate(across(where(is.character), ~str_remove_all(.x, coll("\n"))),
           county = "Oconto",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|TOWN OF |VILLAGE OF |CITY OF |^T |^V |^C "),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           municipality = str_replace(municipality, "\\bLT\\b", "LITTLE"),
           across(where(is.character), str_squish)) |>
    filter(municipality != "TOTALS")
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_shawano <- function(workbookpath, sheetvector, save_output = T){
  process_sheet <- function(sheetindex){
    sheet <- read_excel(workbookpath, sheet = sheetindex, col_names = FALSE,
                        .name_repair = "unique_quiet")
    
    # identify party of contest, if it is a primary
    party.string <- if_else(word(sheet$...2[1], -1, sep = "\n") %in% c("REPUBLICAN","DEMOCRATIC","LIBERTARIAN","WISCONSIN GREEN"),
                            true = word(sheet$...2[1], -1, sep = "\n"),
                            NA)
    
    start.row <- first(which(sheet$...1 %in% c("TOWNS", "VILLAGES", "CITIES")))
    colnames <- sheet |>
      filter(row_number() < start.row,
             row_number() > 1) |>
      mutate(rownum = row_number()) |>
      pivot_longer(cols = -rownum, names_to = "column") |>
      mutate(value = str_replace_all(value, coll("\n"), " ")) |>
      pivot_wider(names_from = rownum, values_from = value) |>
      mutate(`1` = zoo::na.locf(`1`, na.rm = F),
             `2` = zoo::na.locf(`2`, na.rm = F)) |>
      group_by(`2`) |>
      mutate(`3` = zoo::na.locf(`3`, na.rm = F),
             `1` = if_else(!is.na(party.string) & !is.na(`1`),
                           true = paste(party.string, `1`, sep = "_"),
                           false = `1`)) |>
      ungroup() |>
      select(-column) |>
      unite(col = "colname", na.rm = T) |>
      # reporting unit stub sometimes accidentally gets added by pdf to spreadsheet conversion
      mutate(colname = case_when(
        row_number() == 1 ~ "rep_unit",
        row_number() == 2 & colname == "" ~ "rep_unit_stub",
        TRUE ~ colname
      ))
    sheet |>
      filter(row_number() >= start.row) |>
      set_names(colnames$colname) |>
      filter(rep_unit != "TOTAL VOTES CAST") |>
      # unite reporting unit and reporting unit stub if it exists
      unite("reporting_unit", any_of(c("rep_unit", "rep_unit_stub")), na.rm = T, sep = " ") |>
      mutate(ctv = if_else(reporting_unit %in% c("TOWNS", "VILLAGES", "CITIES"),
                           true = reporting_unit,
                           false = NA),
             ctv = zoo::na.locf(ctv),
             reporting_unit = paste(str_sub(ctv, 1, 1), reporting_unit)) |>
      select(-ctv) |>
      filter(! reporting_unit %in% c("T TOWNS", "C CITIES", "V VILLAGES")) |>
      pivot_longer(cols = -reporting_unit, names_to = "contestcandidate", values_to = "votes") |>
      separate(contestcandidate, into = c("contest", "candidate"), sep = "_(?!.*_)") |>
      filter(!is.na(votes),
             # Shawano prints write-in candidate names in some cells instead of vote totals - remove
             ! (candidate == "Write-in" & str_detect(votes, "[aeiou]")))
  }
  
  dtemp <- map(.x = sheetvector,
      .f = ~process_sheet(.x)) |>
    list_rbind() |>
    type_convert() |>
    mutate(county = "Shawano",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|TOWN OF |VILLAGE OF |CITY OF |^C |^T |^V "),
           municipality = word(municipality, 1, sep = coll(",")))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
# this function reads all sheets in the workbook
read_richland <- function(workbookpath, save_output = T){
  richland.sheets <- readxl::excel_sheets(workbookpath)
  
  read_sheet <- function(sheetindex){
    sheet <- readxl::read_excel(workbookpath, sheet = sheetindex, col_names = F,
                                col_types = "text", .name_repair = "unique_quiet")
    
    office <- sheet$...1[3]
    office
    start.col <- suppressWarnings(which(str_detect(sheet, "Municipality")))
    sheet2 <- sheet[,start.col:ncol(sheet)]
    colnames(sheet2) <- paste0("x",1:ncol(sheet2))
    start.row <- which(str_detect(sheet2$x1, "Akan"))
    start.header.row <- which(sheet2$x1 == "Municipality")
    
    colnames <- sheet2 |>
      filter(row_number() < start.row,
             row_number() >= start.header.row) |>
      janitor::remove_empty("rows") |>
      mutate(rownum = row_number()) |>
      pivot_longer(cols = -rownum) |>
      pivot_wider(names_from = rownum, values_from = value) |>
      select(-name) |>
      mutate(`1` = zoo::na.locf(`1`, na.rm = F),
             `1` = if_else(!is.na(`2`),
                           true = paste(office, `1`, sep = "_"),
                           false = `1`)) |>
      unite("colname", na.rm = T)
    
    sheet2 |>
      filter(row_number() >= start.row) |>
      set_names(colnames$colname) |>
      filter(Municipality != "TOTALS") |>
      rename(reporting_unit = Municipality) |>
      pivot_longer(cols = -c(reporting_unit),
                   names_to = "contestcandidate", values_to = "votes") |>
      mutate(contestcandidate = str_replace_all(str_squish(contestcandidate), coll("\r\n"), " ")) |>
      separate(contestcandidate, sep = "_(?!.*_)", into = c("contest", "candidate")) |>
      filter(!is.na(votes))
  }
  
  suppressWarnings(read_sheet)
  
  dtemp <- map(.x = 1:length(richland.sheets),
               .f = read_sheet) |>
    list_rbind() |>
    type_convert() |>
    mutate(county = "Richland",
           across(where(is.character), str_to_upper),
           ctv = case_when(
             str_detect(reporting_unit, "CITY OF") ~ "C",
             str_detect(reporting_unit, "BOAZ|CAZENOVIA|LONE ROCK|VIOLA|YUBA") ~ "V",
             TRUE ~ "T"
           ),
           municipality = str_remove(reporting_unit, "CITY OF "),
           municipality = word(municipality, 1, sep = "-"))
  
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
# excel converts this as 1 giant table, which is OK
read_sawyer <- function(workbookpath, save_output = T){
  sawyer.sheets <- readxl::excel_sheets(workbookpath)
  
  read_sheet <- function(sheetindex){
    sheet <- read_excel(workbookpath, sheet = sheetindex, col_names = F,
                        col_types = "text", .name_repair = "unique_quiet")
    sheet2 <- sheet |>
      filter(str_detect(sheet$...1, "^Sawyer County Election Results|^Page|^UNOFFICIAL", negate = T)) |>
      janitor::remove_empty("cols")
    start.row <- which(str_detect(sheet2$...1, "TOWN OF BASS LAKE"))
    colnames <- sheet2 |>
      mutate(rownum = row_number()) |>
      filter(rownum < start.row) |>
      pivot_longer(cols = -rownum) |>
      pivot_wider(names_from = rownum, values_from = value) |>
      select(-name) |>
      unite("colname", na.rm = T) |>
      mutate(colname = str_replace_all(colname, coll("\n"), " "),
             colname = if_else(colname == "OFFICE_Description_Candidate",
                               true = "reporting_unit",
                               false = colname),
             colname = str_squish(colname))
    sheet2 |>
      filter(row_number() >= start.row) |>
      setNames(colnames$colname) |>
      select(-which(duplicated(colnames$colname))) |>
      rename(reporting_unit = 1) |>
      filter(str_detect(reporting_unit, "^Totals", negate = T)) |>
      pivot_longer(cols = -c(reporting_unit),
                   names_to = "contestcandidate", values_to = "votes") |>
      mutate(contestcandidate = str_replace_all(str_squish(contestcandidate), coll("\r\n"), " ")) |>
      separate(contestcandidate, sep = "_(?!.*_)", into = c("contest", "candidate")) |>
      filter(votes != "x" | is.na(votes))
  }
  
  dtemp <- map(.x = 1:length(sawyer.sheets),
               .f = read_sheet) |>
    list_rbind() |>
    type_convert() |>
    mutate(county = "Sawyer",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|TOWN OF |VILLAGE OF |CITY OF "),
           municipality = word(municipality, 1, sep = coll(",")))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}

################################################################################

################################################################################
# This function anticipates multiple office tables per sheet
read_taylor <- function(workbookpath, save_output = T){
  sheetlist <- readxl::excel_sheets(workbookpath)
  
  process_sheet <- function(sheetindex){
    sheet <- readxl::read_excel(workbookpath, sheet = sheetindex, col_names = F,
                                .name_repair = "unique_quiet")
    
    contest <- sheet$...1[2]
    colnames <- sheet[2,] |> unlist()
    
    sheet |>
      filter(is.na(...1)) |>
      set_names(colnames) |>
      rename(candidate = 2) |>
      select(-1) |>
      pivot_longer(cols = -candidate, values_to = "votes", names_to = "reporting_unit") |>
      mutate(contest = contest)
  }
  
  
  dtemp <- map(.x = 1:length(sheetlist),
               .f = process_sheet) |>
    list_rbind() |>
    mutate(reporting_unit = case_when(
      reporting_unit == "Cleve land" ~ "Cleveland",
      reporting_unit == "Good rich" ~ "Goodrich",
      reporting_unit == "Green wood" ~ "Greenwood",
      reporting_unit == "Mc Kinley" ~ "McKinley",
      reporting_unit == "Roos evelt" ~ "Roosevelt",
      reporting_unit == "Maple hurst" ~ "Maplehurst",
      TRUE ~ reporting_unit
    )) |>
    type_convert() |>
    mutate(county = "Taylor",
           across(where(is.character), str_to_upper),
           ctv = if_else(str_detect(reporting_unit, "^C[.]|^VILLAGE"),
                         str_sub(reporting_unit, 1, 1),
                         "T"),
           municipality = str_remove(reporting_unit, "^C[.] |^VILLAGE "),
           municipality = word(municipality, 1, sep = " [0-9]")
           )
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
# The Chippewa county format requires specifying the page numbers and column headers manually
#   each table extends to two pages
read_chippewa_contest <- function(pdfpath, pagenumbers, column_names, save_output = T){
  pages <- pdftools::pdf_text(pdfpath)
  
  read_page <- function(page){
    df <- pages[[page]] |>
      read_delim(delim = "!!", col_names = "x1", show_col_types = F)
    
    start.row <- first(which(str_detect(df$x1, "^T |^C |^V "))) # row at which the table starts
    
    df |>
      filter(row_number() >= start.row) |>
      separate(x1, into = c("reporting_unit", "votes"), sep = 30) |>
      mutate_all(str_squish) |>
      filter(!is.na(reporting_unit),
             reporting_unit != "TOTALS") |>
      separate(votes, into = paste0("x", 1:length(column_names)), sep = " ", fill = "right") |>
      setNames(c("reporting_unit", column_names)) |>
      pivot_longer(cols = -reporting_unit, names_to = "contestcandidate", values_to = "votes") |>
      separate(contestcandidate, sep = "_(?!.*_)", into = c("contest", "candidate")) |>
      filter(!is.na(votes),
             candidate != "Reported")
  }
  
  dtemp <- map(.x = pagenumbers,
      .f = read_page) |>
    list_rbind() |>
    type_convert() |>
    mutate(county = "Chippewa",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|^T [/]|^C [/]|^V [/]|TOWN OF |VILLAGE OF |CITY OF "),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"))
  dtemp
}
################################################################################

################################################################################
read_pepin <- function(workbookpath, save_output = T){
  
  sheet <- read_excel(workbookpath, col_names = F, .name_repair = "unique_quiet")
  start.row <- min(which(str_detect(sheet$...1, "^Town")))
  colnames <- sheet |>
    filter(row_number() < start.row) |>
    mutate(rownum = row_number()) |>
    pivot_longer(cols = -rownum, names_to = "column") |>
    mutate(value = str_replace_all(value, coll("\n"), " ")) |>
    pivot_wider(names_from = rownum, values_from = value) |>
    mutate(`1` = zoo::na.locf(`1`, na.rm = F),
           colname = paste(`1`, `2`, sep = "_"),
           colname = str_replace_all(colname, "\n", " ")) |>
    group_by(colname) |>
    mutate(colname = if_else(row_number() > 1, paste(colname, row_number()), colname))
  
  all.output <- sheet |>
    filter(row_number() >= start.row) |>
    set_names(colnames$colname) |>
    rename(reporting_unit = 1, total = 2) |>
    select(-total) |>
    pivot_longer(cols = -reporting_unit, names_to = "contest_candidate", values_to = "votes") |>
    separate(contest_candidate, sep = "_(?!.*_)", into = c("contest", "candidate")) |>
    filter(!is.na(votes)) |>
    type_convert() |>
    filter(reporting_unit != "Total") |>
    mutate(county = "Pepin",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|^T [/]|^C [/]|^V [/]|TOWN OF |VILLAGE OF |CITY OF "),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           across(where(is.character), str_squish))
  
  all.output
  if(save_output == TRUE){
    write_csv(all.output, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
    all.output
  } else{
    all.output
  }
}
################################################################################

################################################################################
read_eauclaire <- function(workbookpath, save_output = T){
  
  sheet1 <- read_excel(workbookpath, sheet = 1, col_names = F, .name_repair = "unique_quiet")
  sheet2 <- read_excel(workbookpath, sheet = 2, col_names = F, .name_repair = "unique_quiet",
                       col_types = "text")
  start.row <- min(which(str_detect(sheet1$...1, "^Town")))
  
  colnames <- sheet1 |>
    filter(row_number() %in% c(start.row-2, start.row-1)) |>
    mutate(rownum = row_number()) |>
    pivot_longer(cols = -rownum, names_to = "column") |>
    mutate(value = str_replace_all(value, coll("\n"), " ")) |>
    pivot_wider(names_from = rownum, values_from = value) |>
    mutate(`1` = zoo::na.locf(`1`, na.rm = F),
           colname = paste(`1`, `2`, sep = "_"),
           colname = str_replace_all(colname, "\n", " "))
  both.sheets <- sheet1 |>
    filter(row_number() >= start.row) |>
    rbind(sheet2) |>
    set_names(colnames$colname) |>
    rename(reporting_unit = 1) |>
    select(-ends_with("NA")) |>
    rename(reporting_unit = 1) |>
    pivot_longer(cols = -reporting_unit, names_to = "contestcandidate", values_to = "votes") |>
    mutate(votes = na_if(votes, "-")) |>
    filter(!is.na(votes)) |>
    separate(contestcandidate, sep = "_(?!.*_)", into = c("contest", "candidate")) |>
    type_convert() |>
    filter(reporting_unit != "Total") |>
    mutate(county = "Eau Claire",
           reporting_unit = str_remove_all(reporting_unit, coll("\n")),
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|^T [/]|^C [/]|^V [/]|TOWN OF |VILLAGE OF |CITY OF "),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           across(where(is.character), str_squish))
  
  if(save_output == TRUE){
    write_csv(both.sheets, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
}
################################################################################

################################################################################
read_marathon <- function(pdfpath, save_output = T){
  #################################################
  # read all the data from the PDF
  pagelist <- pdftools::pdf_text(pdfpath)
  
  #################################################
  # this function processes a single page of the pdf
  read_pdf_page <- function(pageindex, pagelist){
    page <- pagelist[pageindex] |> read_delim(col_names = F, delim = "!!",
                                              col_types = cols(.default = "c"))
    
    reporting.unit <- page$X1[which(str_detect(str_to_upper(page$X1),
                                               "NOVEMBER 5, 2024|NOVEMBER 5,2024")) + 1]
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
           ctv = str_extract(reporting_unit, "\\bC\\b|\\bT\\b|\\bV\\b"),
           municipality = str_remove(reporting_unit, "TOWN OF |VILLAGE OF |CITY OF |\\bV\\b|\\bC\\b|\\bT\\b"),
           municipality = word(municipality, 1, 1, sep = "\\bWARD|\\bWD|\\bWARD|\\bD[0-9]"),
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
################################################################################

################################################################################
read_iowa <- function(workbookpath, save_output = T){
  sheetvector <- excel_sheets(workbookpath)
  
  process_sheet <- function(sheetindex){
    sheet <- read_excel(workbookpath, sheet = sheetindex, col_names = F,
                        .name_repair = "unique_quiet") |>
      janitor::remove_empty("cols")
    
    startrow <- min(which(str_detect(sheet$...1, "Town")))
    
    colnames <- sheet |>
      filter(row_number() < startrow,
             str_detect(...1, "CANVASS RESULTS AS OF", negate = T) |
               is.na(...1)) |>
      mutate(rownum = row_number()) |>
      pivot_longer(cols = -rownum) |>
      pivot_wider(names_from = rownum, values_from = value) |>
      mutate(`1` = zoo::na.locf(`1`, na.rm = F),
             `2` = zoo::na.locf(`2`, na.rm = F)) |>
      unite("colname", any_of(c("1","2","3")), na.rm = T) |>
      pull(2)
    
    sheet |>
      filter(row_number() >= startrow) |>
      set_names(colnames) |>
      rename(reporting_unit = 1) |>
      filter(reporting_unit != "TOTAL") |>
      select(-contains("PROVISIONALS")) |>
      pivot_longer(cols = -reporting_unit, names_to = "contestcandidate", values_to = "votes") |>
      separate(contestcandidate, sep = "_(?!.*_)", into = c("contest", "candidate"))
  }
  
  dtemp <- map(.x = sheetvector,
               .f = ~process_sheet(.x)) |>
    list_rbind() |>
    type_convert() |>
    mutate(county = "Iowa",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           reporting_unit = str_remove_all(reporting_unit, coll("-")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|TOWN OF |VILLAGE OF |CITY OF |^T |^V |^C "),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           municipality = str_remove_all(municipality, coll("-")),
           across(where(is.character), str_squish)) |>
    filter(municipality != "TOTAL")
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_buffalo <- function(workbookpath, save_output = T){
  sheetvector <- excel_sheets(workbookpath)
  
  process_sheet <- function(sheetindex){
    sheet <- read_excel(workbookpath, sheet = sheetindex, col_names = T,
                        .name_repair = "unique_quiet", skip = 1)
    
    sheet |>
      rename(candidate = 1) |>
      filter(!is.na(candidate),
             str_detect(candidate, "Undervotes|Registered Write-in|Provisional", negate = T),
             ! candidate %in% c("FEDERAL","STATE REFERENDUM","COUNTY","LEGISLATIVE AND STATE",
                                "CONGRESSIONAL")) |>
      mutate(contest = if_else(str_detect(candidate, "^County Clerk|^County Treasurer|^Register of Deeds|^Eligible Voting Age|^State Senator Dist. 10|^Representative to the Assembly Dist. 29|^District Attorney|^United States Senator|^Representative in Congress|^President/Vice President"),
                               true = candidate, false = NA),
             contest = zoo::na.locf(contest, na.rm = F)) |>
      select(contest, everything()) |>
      filter(contest != candidate) |>
      mutate(candidate = paste(PARTY, candidate)) |>
      select(-c(PARTY, TOTALS, `PRECINCTS REPORTING`)) |>
      pivot_longer(cols = -c(contest, candidate), names_to = "reporting_unit", values_to = "votes") |>
      mutate(across(where(is.character), str_squish))
  }
  
  dtemp <- map(.x = sheetvector,
               .f = ~process_sheet(.x)) |>
    list_rbind() |>
    type_convert() |>
    mutate(county = "Buffalo",
           across(where(is.character), str_to_upper),
           reporting_unit = str_replace(reporting_unit, "^TW", "TOWN"),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           reporting_unit = str_remove_all(reporting_unit, coll("-")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|TOWN OF |VILLAGE OF |CITY OF |^T |^V |^C "),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           municipality = str_remove_all(municipality, coll("-")),
           across(where(is.character), str_squish)) |>
    filter(municipality != "TOTAL")
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_manitowoc <- function(workbookpath, save_output = T){
  sheet1 <- googlesheets4::read_sheet(workbookpath, sheet = 1)
  sheet2 <- googlesheets4::read_sheet(workbookpath, sheet = 2)
  stopifnot(all.equal(sheet1, sheet2))
  
  dtemp <- sheet1 |>
    pivot_longer(cols = -c(contest, candidate), names_to = "reporting_unit", values_to = "votes") |>
    mutate(county = "Manitowoc",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|TOWN OF |VILLAGE OF |CITY OF |^T |^V |^C "),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           municipality = str_remove_all(municipality, coll("-")),
           across(where(is.character), str_squish))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/Manitowoc ",
                            str_replace_all(Sys.time(), pattern = ":", "-"),
                            ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_sauk <- function(workbookpath, save_output = T){
  sheet1 <- googlesheets4::read_sheet(workbookpath, sheet = 1, col_names = F)
  sheet2 <- googlesheets4::read_sheet(workbookpath, sheet = 2, col_names = F)
  stopifnot(all.equal(sheet1, sheet2))
  
  colnames <- sheet1[1:2,] |>
    mutate(rownum = row_number()) |>
    pivot_longer(cols = -rownum) |>
    pivot_wider(names_from = rownum, values_from = value) |>
    unite("colname", `1`, `2`, na.rm = T) |>
    pull(colname)
  
  dtemp <- sheet1 |>
    filter(row_number() > 2) |>
    set_names(colnames) |>
    pivot_longer(cols = -reporting_unit, values_to = "votes", names_to = "contestcandidate") |>
    separate(contestcandidate, into = c("contest", "candidate"), sep = "_") |>
    mutate(county = "Sauk",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|TOWN OF |VILLAGE OF |CITY OF |^T |^V |^C "),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           municipality = str_remove_all(municipality, coll("-")),
           across(where(is.character), str_squish))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/Sauk ",
                            str_replace_all(Sys.time(), pattern = ":", "-"),
                            ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_monroe <- function(workbookpath, save_output = T){
  sheet1 <- googlesheets4::read_sheet(workbookpath, sheet = 1, col_names = F)
  sheet2 <- googlesheets4::read_sheet(workbookpath, sheet = 2, col_names = F)
  stopifnot(all.equal(sheet1, sheet2))
  
  colnames <- sheet1[1:2,] |>
    mutate(rownum = row_number()) |>
    pivot_longer(cols = -rownum) |>
    pivot_wider(names_from = rownum, values_from = value) |>
    unite("colname", `1`, `2`, na.rm = T) |>
    pull(colname)
  
  dtemp <- sheet1 |>
    filter(row_number() > 2) |>
    set_names(colnames) |>
    pivot_longer(cols = -reporting_unit, values_to = "votes", names_to = "contestcandidate") |>
    separate(contestcandidate, into = c("contest", "candidate"), sep = "_") |>
    mutate(county = "Monroe",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|TOWN OF |VILLAGE OF |CITY OF |^T |^V |^C "),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           municipality = str_remove_all(municipality, coll("-")),
           across(where(is.character), str_squish))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/Monroe ",
                            str_replace_all(Sys.time(), pattern = ":", "-"),
                            ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_trempealeau <- function(workbookpath, save_output = T){
  sheetvector <- excel_sheets(workbookpath)
  
  process_sheet <- function(sheetindex){
    sheet <- read_excel(workbookpath, sheet = sheetindex, col_names = F,
                        .name_repair = "unique_quiet") |>
      janitor::remove_empty(which = "cols")
    sheet
    startrow <- min(which(str_detect(sheet$...1, "MUNICIPALITY")))+1
    
    header1 <- sheet$...1[1]
    colnames <- sheet |>
      filter(row_number() < startrow,
             row_number() > 1) |>
      mutate(rownum = row_number()) |>
      pivot_longer(cols = -rownum) |>
      pivot_wider(names_from = rownum, values_from = value) |>
      mutate(across(where(is.character), ~zoo::na.locf(.x, na.rm = F)),
             header = header1) |>
      select(-name) |>
      select(header, everything()) |>
      unite("colname", na.rm = T) |>
      pull(colname)
    sheet |>
      filter(row_number() >= startrow) |>
      set_names(colnames) |>
      rename(reporting_unit = 1) |>
      filter(reporting_unit != "TOTALS") |>
      pivot_longer(cols = -reporting_unit, names_to = "contest", values_to = "votes") |>
      mutate(candidate = word(contest, -2, -1, sep = "_"),
             contest = str_remove(contest, candidate),
             candidate = str_replace(candidate, "_", " "),
             contest = str_remove(contest, "_$"),
             votes = str_remove_all(votes, "X"),
             votes = na_if(votes, "")) |>
      filter(!is.na(votes))
  }
  
  dtemp <- map(.x = sheetvector,
               .f = process_sheet) |>
    list_rbind() |>
    # remove referendums because they are differently formatted
    filter(str_detect(contest, "STATE_QUESTION", negate = T)) |>
    type_convert() |>
    mutate(county = "Trempealeau",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|^T [/]|^C [/]|^V [/]|TOWN OF |VILLAGE OF |CITY OF |^T-|^C-|^V-"),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           across(where(is.character), str_squish))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_crawford <- function(workbookpath, save_output = T){
  sheet <- read_excel(workbookpath, sheet = 1, col_names = F,
                      .name_repair = "unique_quiet")
  
  startrow <- min(which(str_detect(sheet$...1, "^TOWN")))
  colnames <- sheet |>
    filter(row_number() < startrow) |>
    mutate(rownum = row_number()) |>
    pivot_longer(cols = -rownum) |>
    pivot_wider(names_from = rownum, values_from = value) |>
    mutate(across(where(is.character), ~zoo::na.locf(.x, na.rm = F))) |>
    select(-name) |>
    unite("colname", na.rm = T) |>
    group_by(colname) |>
    mutate(colname = if_else(row_number() > 1, paste(colname, row_number(), sep = "_"),
                             colname),
           colname = str_replace_all(colname, coll("\n"), " ")) |>
    pull(colname)
  dtemp <- sheet |>
    filter(row_number() >= startrow,
           str_detect(...1, "Total Votes", negate = T)) |>
    set_names(colnames) |>
    rename(reporting_unit = 1) |>
    mutate(ctvassign = case_when(
      str_detect(reporting_unit, "^TOWN") ~ "TOWN OF",
      str_detect(reporting_unit, "^VILLAGE") ~ "VILLAGE OF",
      str_detect(reporting_unit, "^CITY") ~ "CITY OF PRAIRIE DU CHIEN",
      TRUE ~ NA
    ),
    ctvassign = zoo::na.locf(ctvassign),
    reporting_unit = paste(ctvassign, reporting_unit)) |>
    select(-ctvassign) |>
    filter(!is.na(pick(2))) |>
    pivot_longer(cols = -reporting_unit, names_to = "contestcandidate", values_to = "votes") |>
    filter(str_detect(votes, "[aeiou]", negate = T)) |> # drop extra rep unit column
    separate(contestcandidate, sep = "_(?!.*_)", into = c("contest", "candidate")) |>
    type_convert() |>
    mutate(county = "Crawford",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_replace(reporting_unit, "-W(?=[0-9])", " W"),
           reporting_unit = str_replace(reporting_unit, "PRA DU CHIEN", "PRAIRIE DU CHIEN"),
           reporting_unit = case_when(
             reporting_unit == "CITY OF PRAIRIE DU CHIEN 1ST WARD" ~ "CITY OF PRAIRIE DU CHIEN WARD 1",
             reporting_unit == "CITY OF PRAIRIE DU CHIEN 2ND WARD" ~ "CITY OF PRAIRIE DU CHIEN WARD 2",
             reporting_unit == "CITY OF PRAIRIE DU CHIEN 3RD WARD" ~ "CITY OF PRAIRIE DU CHIEN WARD 3",
             reporting_unit == "CITY OF PRAIRIE DU CHIEN 4TH WARD" ~ "CITY OF PRAIRIE DU CHIEN WARD 4",
             reporting_unit == "CITY OF PRAIRIE DU CHIEN 5TH WARD" ~ "CITY OF PRAIRIE DU CHIEN WARD 5",
             reporting_unit == "CITY OF PRAIRIE DU CHIEN 6TH WARD" ~ "CITY OF PRAIRIE DU CHIEN WARD 6",
             reporting_unit == "VILLAGE OF DESOTO W2" ~ "VILLAGE OF DE SOTO W2",
             reporting_unit == "VILLAGE OF MT. STERLING" ~ "VILLAGE OF MOUNT STERLING",
             TRUE ~ reporting_unit
           ),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|TOWN OF |VILLAGE OF |CITY OF "),
           municipality = word(municipality, 1, sep = "\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           across(where(is.character), str_squish))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_greenlake <- function(workbookpath, save_output = T){
  sheetvector <- excel_sheets(workbookpath)
  
  process_sheet <- function(sheetindex){
    sheet <- read_excel(workbookpath, sheet = sheetindex, col_names = F,
                        .name_repair = "unique_quiet") |>
      janitor::remove_empty(which = "cols")
    sheet
    headerstart <- if_else(!is.na(sheet$...1[1]),
                           true = 4, false = 1)
    startrow <- min(which(str_detect(sheet$...1, "^TOWN")))
    
    colnames <- sheet[headerstart:(startrow-1),] |>
      mutate(rownum = row_number()) |>
      pivot_longer(cols = -rownum) |>
      pivot_wider(names_from = rownum, values_from = value) |>
      mutate(across(where(is.character), ~zoo::na.locf(.x, na.rm = F))) |>
      select(-name) |>
      unite("colname", na.rm = T) |>
      pull(colname)
    sheet |>
      filter(row_number() >= startrow) |>
      set_names(colnames) |>
      rename(reporting_unit = 1) |>
      filter(reporting_unit != "Total|TOTAL|totals|Totals|TOTALS") |>
      pivot_longer(cols = -reporting_unit, names_to = "contestcandidate", values_to = "votes") |>
      separate(contestcandidate, sep = "_(?!.*_)", into = c("contest", "candidate"))
  }
  
  dtemp <- map(.x = sheetvector,
               .f = process_sheet) |>
    list_rbind() |>
    type_convert() |>
    mutate(county = "Green Lake",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|^T [/]|^C [/]|^V [/]|TOWN OF |VILLAGE OF |CITY OF |^T-|^C-|^V-"),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           across(where(is.character), str_squish))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_marquette <- function(workbookpath, sheetvector = c(2,3,4), save_output = T){
  sheet1 <- read_excel(workbookpath, sheet = sheetvector[1], col_names = F,
                       .name_repair = "unique_quiet", skip = 24)
  colnames.1 <- sheet1 |>
    filter(row_number() < 4) |>
    mutate(rownum = row_number()) |>
    pivot_longer(cols = -rownum) |>
    pivot_wider(names_from = rownum, values_from = value) |>
    mutate(across(.cols = any_of(c("1","2","3")),
                  .f = ~zoo::na.locf(.x, na.rm = F))) |>
    select(-name) |>
    unite("colname", na.rm = T) |>
    mutate(colname = str_replace_all(colname, coll("\n"), " "))
  
  sheet1.1 <- sheet1 |>
    filter(row_number() > 3) |>
    set_names(colnames.1$colname) |>
    rename(reporting_unit = 1) |>
    filter(reporting_unit != "TOTAL VOTES") |>
    pivot_longer(cols = -reporting_unit, names_to = "candidate", values_to = "votes") |>
    mutate(contest = "PRESIDENT")
  
  ##############
  sheet2.1 <- read_excel(workbookpath, sheet = sheetvector[2], col_names = F,
                       .name_repair = "unique_quiet", n_max = 26)
  colnames.2.1 <- sheet2.1 |>
    filter(row_number() < 4) |>
    mutate(rownum = row_number()) |>
    pivot_longer(cols = -rownum) |>
    pivot_wider(names_from = rownum, values_from = value) |>
    mutate(across(.cols = any_of(c("1","2","3")),
                  .f = ~zoo::na.locf(.x, na.rm = F))) |>
    select(-name) |>
    unite("colname", na.rm = T) |>
    mutate(colname = str_replace_all(colname, coll("\n"), " "))
  sheet2.1.2 <- sheet2.1 |>
    filter(row_number() > 3) |>
    set_names(colnames.2.1$colname) |>
    rename(reporting_unit = 1) |>
    filter(reporting_unit != "TOTAL VOTES") |>
    pivot_longer(cols = -reporting_unit, names_to = "candidate", values_to = "votes") |>
    mutate(contest = word(candidate, 1, sep = "_"),
           candidate = str_remove(candidate, contest))
  
  ###########################
  sheet2.2 <- read_excel(workbookpath, sheet = sheetvector[2], col_names = F,
                         .name_repair = "unique_quiet", skip = 27)
  colnames.2.2 <- sheet2.2 |>
    filter(row_number() < 4) |>
    mutate(rownum = row_number()) |>
    pivot_longer(cols = -rownum) |>
    pivot_wider(names_from = rownum, values_from = value) |>
    mutate(across(.cols = any_of(c("1","2","3")),
                  .f = ~zoo::na.locf(.x, na.rm = F))) |>
    select(-name) |>
    unite("colname", na.rm = T) |>
    mutate(colname = str_replace_all(colname, coll("\n"), " "))
  sheet2.2.2 <- sheet2.2 |>
    filter(row_number() > 3) |>
    set_names(colnames.2.2$colname) |>
    rename(reporting_unit = 1) |>
    filter(reporting_unit != "TOTAL VOTES") |>
    pivot_longer(cols = -reporting_unit, names_to = "candidate", values_to = "votes") |>
    mutate(contest = word(candidate, 1, sep = "_"),
           candidate = str_remove(candidate, contest))
  
  dtemp <- bind_rows(sheet1.1, sheet2.1.2, sheet2.2.2) |>
    type_convert() |>
    mutate(county = "Marquette",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|^T [/]|^C [/]|^V [/]|TOWN OF |VILLAGE OF |CITY OF |^T-|^C-|^V-"),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           municipality = str_remove(municipality, coll(",")),
           across(where(is.character), str_squish)) |>
    filter(municipality != "SCROLL DOWN")
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_kewaunee <- function(workbookpath, save_output = T){
  sheet1 <- read_excel(workbookpath, sheet = 1, col_names = F,
                       .name_repair = "unique_quiet", col_types = "text") |>
    janitor::remove_empty(which = "cols")
  
  sheet2 <- read_excel(workbookpath, sheet = 2, col_names = F,
                       .name_repair = "unique_quiet", col_types = "text") |>
    janitor::remove_empty(which = "cols")
  
  startrow <- min(which(str_detect(sheet1$...1, "^OFFICE")))
  
  dtemp <- sheet1 |>
    filter(row_number() > startrow) |>
    bind_rows(sheet2) |>
    set_names(unlist(sheet1[startrow,])) |>
    rename(contest = 1, candidate = 2) |>
    janitor::remove_empty("rows") |>
    pivot_longer(cols = -c(contest, candidate), names_to = "reporting_unit", values_to = "votes") |>
    filter(reporting_unit != "TOTALS") |>
    type_convert() |>
    mutate(county = "Kewaunee",
           across(where(is.character), str_to_upper),
           ctv = case_when(
             word(reporting_unit, 1) == "VILLAGE" ~ "V",
             word(reporting_unit, 1) == "CITY" ~ "C",
             TRUE ~ "T"
           ),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|^T [/]|^C [/]|^V [/]|TOWN OF |VILLAGE OF |CITY OF |^T-|^C-|^V-|^CITY |^VILLAGE "),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           municipality = str_remove(municipality, coll(",")),
           across(where(is.character), str_squish))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_burnett <- function(workbookpath, save_output = T){
  sheet1 <- read_excel(workbookpath, sheet = 1, col_names = F,
                       .name_repair = "unique_quiet", col_types = "text") |>
    janitor::remove_empty(which = "cols")
  colname.startrow <- min(which(sheet1$...1 == "Offices"))
  sheet1.1 <- sheet1 |> filter(row_number() >= colname.startrow) |>
    janitor::remove_empty("cols")
  
  startrow <- min(which(str_detect(sheet1.1$...1, "^TOWN")))
  
  colnames <- sheet1.1 |>
    filter(row_number() < startrow) |>
    mutate(rownum = row_number()) |>
    pivot_longer(cols = -rownum) |>
    pivot_wider(names_from = rownum, values_from = value) |>
    mutate(`1` = zoo::na.locf(`1`),
           across(where(is.character), ~str_replace(.x, coll("\n"), " "))) |>
    unite("colname", `1`, `2`, na.rm = T)
  
  dtemp <- sheet1.1 |>
    filter(row_number() >= startrow) |>
    set_names(colnames$colname) |>
    select(-which(duplicated(colnames$colname))) |>
    rename(reporting_unit = 1) |>
    filter(str_detect(reporting_unit, "^Total|^NUMBER", negate = T)) |>
    pivot_longer(cols = -c(reporting_unit), names_to = "contestcandidate",
                 values_to = "votes") |>
    separate(contestcandidate, sep = "_(?!.*_)", into = c("contest", "candidate")) |>
    filter(!is.na(candidate)) |>
    type_convert() |>
    mutate(county = "Burnett",
           across(where(is.character), str_to_upper),
           ctv = case_when(
             word(reporting_unit, 1) == "VILLAGE" ~ "V",
             word(reporting_unit, 1) == "CITY" ~ "C",
             TRUE ~ "T"
           ),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|^T [/]|^C [/]|^V [/]|TOWN OF |VILLAGE OF |CITY OF |^T-|^C-|^V-|^CITY |^VILLAGE "),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           municipality = str_remove(municipality, coll(",")),
           across(where(is.character), str_squish))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_washburn <- function(workbookpath, save_output = T){
  sheet1 <- read_excel(workbookpath, sheet = 1, col_names = F,
                       .name_repair = "unique_quiet", col_types = "text") |>
    janitor::remove_empty(which = "cols")
  
  sheet1.1 <- sheet1 |> filter(row_number() > 1) |>
    janitor::remove_empty("cols")
  
  startrow <- min(which(str_detect(sheet1.1$...1, "^TOWN")))
  
  colnames <- sheet1.1 |>
    filter(row_number() < startrow) |>
    mutate(rownum = row_number()) |>
    pivot_longer(cols = -rownum) |>
    pivot_wider(names_from = rownum, values_from = value) |>
    mutate(`1` = zoo::na.locf(`1`),
           `2` = zoo::na.locf(`2`, na.rm = F),
           `3` = zoo::na.locf(`3`),
           `4` = zoo::na.locf(`4`),
           across(where(is.character), ~str_replace(.x, coll("\n"), " "))) |>
    unite("colname", `1`, `2`, `3`, `4`, na.rm = T)
  
  dtemp <- sheet1.1 |>
    filter(row_number() >= startrow) |>
    filter(row_number() < 31) |>
    set_names(colnames$colname) |>
    rename(reporting_unit = 1) |>
    filter(str_detect(reporting_unit, "^TOTAL|^VOTER", negate = T)) |>
    mutate(ctv = if_else(str_detect(reporting_unit, coll(":")),
                         true = str_sub(reporting_unit, 1, 1),
                         false = NA),
           ctv = zoo::na.locf(ctv)) |>
    select(ctv, everything()) |>
    filter(str_detect(reporting_unit, coll(":"), negate = T)) |>
    pivot_longer(cols = -c(ctv, reporting_unit), names_to = "contestcandidate",
                 values_to = "votes") |>
    separate(contestcandidate, sep = "_(?!.*_)", into = c("contest", "candidate")) |>
    filter(contest != "VOTER DATA_CANDIDATE:") |>
    type_convert() |>
    mutate(county = "Washburn",
           across(where(is.character), str_to_upper),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|^T [/]|^C [/]|^V [/]|TOWN OF |VILLAGE OF |CITY OF |^T-|^C-|^V-|^CITY |^VILLAGE "),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           municipality = str_remove(municipality, coll(",")),
           across(where(is.character), str_squish),
           municipality = str_replace(municipality, "BEAVERBROOK", "BEAVER BROOK"))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_kenosha <- function(csvurl, save_output = T){
  kenosha <- read_delim(csvurl, delim = "|")
  
  dtemp <- kenosha |>
    rename(reporting_unit = Ward_Name, municipality = Municipality, contest = Contest,
           candidate = Candidate, votes = Votes) |>
    mutate(reporting_unit = paste(municipality, reporting_unit),
           candidate = paste(Party, candidate)) |>
    select(municipality, reporting_unit, contest, candidate, votes) |>
    type_convert() |>
    mutate(county = "KENOSHA",
           across(where(is.character), str_to_upper),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove_all(municipality, coll(".")),
           ctv = str_sub(municipality, 1, 1),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|^T [/]|^C [/]|^V [/]|TOWN OF |VILLAGE OF |CITY OF |^T-|^C-|^V-|^CITY |^VILLAGE |^T |^C |^V "),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           municipality = str_remove(municipality, coll(",")),
           across(where(is.character), str_squish),
           municipality = str_replace(municipality, "BEAVERBROOK", "BEAVER BROOK"))
  
  # reformat timestamp
  timestamp <- str_remove(word(csvurl, -2, -1, sep = "_"), ".csv")
  
  reformated.timestamp <- paste0(str_sub(timestamp, 1, 4), "-", str_sub(timestamp, 5,6), "-",
                                 str_sub(timestamp, 7,8), " ", str_sub(timestamp,-6,-5), "-",
                                 str_sub(timestamp, -4,-3), "-", str_sub(timestamp, -2, -1))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/Kenosha ", reformated.timestamp, ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_marinette <- function(workbookpath, sheetvector = 2:4, save_output = T){
  firstsheet <- read_excel(workbookpath, sheet = sheetvector[1], col_names = F,
                           .name_repair = "unique_quiet") |>
    janitor::remove_empty(which = "cols")
  colnames <- firstsheet |>
    select(-1) |>
    filter(row_number() < 9) |>
    mutate(rownum = row_number()) |>
    pivot_longer(cols = -rownum) |>
    pivot_wider(names_from = rownum, values_from = value) |>
    mutate(across(.cols = any_of(c("1","2","3")),
                  .f = ~zoo::na.locf(.x, na.rm = F))) |>
    select(-name) |>
    unite("colname", na.rm = T) |>
    mutate(colname = str_replace_all(colname, coll("\n"), " ")) |>
    pull(colname)
  
  firstcol <- firstsheet[,1]
  
  firstsheet.clean <- firstsheet |>
    filter(row_number() >= 12) |>
    set_names(c("reporting_unit", colnames)) |>
    filter(! reporting_unit %in% c("Total","TOTAL","totals","Totals","TOTALS","Reporting Unit Count"),
           ! reporting_unit %in% c("TOWNS", "CITIES", "VILLAGES"),
           !is.na(reporting_unit)) |>
    pivot_longer(cols = -reporting_unit, names_to = "contestcandidate", values_to = "votes") |>
    mutate(candidate = word(contestcandidate, 2, sep = "12_|36_|25_|27_|PRESIDENT_VICE PRESIDENT_"),
           contest = str_remove(contestcandidate, candidate)) |>
    select(-contestcandidate)
  
  process_sheet <- function(sheetindex){
    sheet <- read_excel(workbookpath, sheet = sheetindex, col_names = F,
                        .name_repair = "unique_quiet") |>
      janitor::remove_empty(which = "cols")
    
    colnames <- sheet |>
      filter(row_number() < 9) |>
      mutate(rownum = row_number()) |>
      pivot_longer(cols = -rownum) |>
      pivot_wider(names_from = rownum, values_from = value) |>
      mutate(across(.cols = any_of(c("1","2","3")),
                    .f = ~zoo::na.locf(.x, na.rm = F))) |>
      select(-name) |>
      unite("colname", na.rm = T) |>
      mutate(colname = str_replace_all(colname, coll("\n"), " ")) |>
      pull(colname)
    
    bind_cols(firstcol, sheet) |>
      filter(row_number() >= 12) |>
      set_names(c("reporting_unit", colnames)) |>
      filter(! reporting_unit %in% c("Total","TOTAL","totals","Totals","TOTALS","Reporting Unit Count"),
             ! reporting_unit %in% c("TOWNS", "CITIES", "VILLAGES"),
             !is.na(reporting_unit)) |>
      pivot_longer(cols = -reporting_unit, names_to = "contestcandidate", values_to = "votes") |>
      mutate(candidate = word(contestcandidate, 2, sep = "12_|36_|25_|27_|PRESIDENT_VICE PRESIDENT_"),
             contest = str_remove(contestcandidate, candidate)) |>
      select(-contestcandidate)
  }
  
  dtemp <- map(.x = sheetvector[2:length(sheetvector)],
               .f = process_sheet) |>
    list_rbind() |>
    rbind(firstsheet.clean) |>
    type_convert() |>
    mutate(county = "Marinette",
           across(where(is.character), str_to_upper),
           ctv = case_when(
             str_detect(reporting_unit, "^MARINETTE|^NIAGARA|^PESHTIGO") ~ "C",
             TRUE ~ str_sub(reporting_unit, 1, 1)
           ),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|^T [/]|^C [/]|^V [/]|TOWN OF |VILLAGE OF |CITY OF |^T-|^C-|^V-"),
           municipality = word(municipality, 1, sep = " - |\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           municipality = str_remove(municipality, coll(",")),
           across(where(is.character), str_squish))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_langlade <- function(workbookpath, save_output = T){
  # this function requires custom headers
  sheet1.orig <- readxl::read_excel(workbookpath, col_names = T, sheet = 1,
                                    col_types = "text", .name_repair = "unique_quiet")
  
  sheet1 <- sheet1.orig |>
    rename(candidate = 1) |>
    mutate(contest = if_else(is.na(`TOWN ACKLEY`) & !is.na(candidate),
                             true = candidate, false = NA),
           contest = zoo::na.locf(contest)) |>
    filter(contest != candidate) |>
    select(contest, everything()) |>
    select(-Total) |>
    pivot_longer(cols = -c(contest, candidate), names_to = "reporting_unit", values_to = "votes") |>
    mutate(across(where(is.character), ~str_replace(.x, coll("\n"), " ")))
  
  sheet2 <- readxl::read_excel(workbookpath, col_names = colnames(sheet1.orig), sheet = 2,
                               col_types = "text", .name_repair = "unique_quiet") |>
    rename(candidate = 1) |>
    mutate(contest = if_else(is.na(`TOWN ACKLEY`) & !is.na(candidate),
                             true = candidate, false = NA),
           contest = zoo::na.locf(contest, na.rm = F)) |>
    filter(contest != candidate) |>
    select(contest, everything()) |>
    # the zeroes show a value of 5 in this column, so remove that here
    mutate(`TOWN ACKLEY` = if_else(`TOWN ACKLEY` == Total, "0", `TOWN ACKLEY`)) |>
    select(-Total) |>
    pivot_longer(cols = -c(contest, candidate), names_to = "reporting_unit", values_to = "votes") |>
    mutate(across(where(is.character), ~str_replace(.x, coll("\n"), " ")))
  
  
  dtemp <- bind_rows(sheet1, sheet2) |>
    mutate(across(where(is.character), ~str_remove_all(.x, coll("\n"))),
           county = "Langlade",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           reporting_unit = str_replace(reporting_unit, "CITY OF ANTIGO", "CITY OF ANTIGO WARD"),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|TOWN OF |VILLAGE OF |CITY OF |^T |^V |^C |^TOWN |^VILLAGE"),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           across(where(is.character), str_squish)) |>
    filter(municipality != "TOTALS") |>
    type_convert()
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_dunn <- function(workbookpath, save_output = T){
  sheet1 <- read_excel(workbookpath, sheet = 1, col_names = F,
                       .name_repair = "unique_quiet", col_types = "text") |>
    janitor::remove_empty(which = "cols")
  
  startrow <- min(which(str_detect(sheet1$...1, "^Precinct")))+1
  
  colnames <- sheet1 |>
    filter(row_number() < startrow,
           row_number() > 1) |>
    mutate(rownum = row_number()) |>
    pivot_longer(cols = -rownum) |>
    pivot_wider(names_from = rownum, values_from = value) |>
    mutate(`1` = zoo::na.locf(`1`, na.rm = F),
           `2` = zoo::na.locf(`2`, na.rm = F),
           across(where(is.character), ~str_replace(.x, coll("\n"), " "))) |>
    unite("colname", `1`, `2`, `3`, na.rm = T)
  
  dtemp <- sheet1 |>
    filter(row_number() >= startrow) |>
    set_names(colnames$colname) |>
    rename(reporting_unit = 2) |>
    select(-c(Precinct, contains("Voter Participation"))) |>
    filter(str_detect(reporting_unit, "^Total|Voters", negate = T),
           !is.na(reporting_unit)) |>
    pivot_longer(cols = -c(reporting_unit), names_to = "contestcandidate",
                 values_to = "votes") |>
    separate(contestcandidate, sep = "_(?!.*_)", into = c("contest", "candidate")) |>
    filter(!is.na(candidate)) |>
    type_convert() |>
    mutate(county = "Dunn",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           reporting_unit = str_replace(reporting_unit, "MENOMOINE", "MENOMONIE"),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|^T [/]|^C [/]|^V [/]|TOWN OF |VILLAGE OF |CITY OF |^T-|^C-|^V-|^CITY |^VILLAGE |^T |^V |^C "),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           municipality = str_remove(municipality, coll(",")),
           across(where(is.character), str_squish))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_grant <- function(workbookpath, save_output = T){
  sheet1 <- read_excel(workbookpath, sheet = 1, col_names = T,
                       .name_repair = "unique_quiet", col_types = "text")
  
  colnames <- tibble(candidate = colnames(sheet1)) |>
    mutate(contest = if_else(str_detect(candidate, "SCATTERING"),
                             str_squish(str_remove(candidate, "SCATTERING")), NA)) |>
    fill(contest, .direction = "up") |>
    mutate(contest = if_else(row_number() < 4, NA, contest)) |>
    unite("colname", contest, candidate, na.rm = T) |>
    mutate(colname = str_replace(colname, coll(":"), "_"),
           colname = str_replace_all(colname, coll("\n"), " "),
           colname = str_replace(colname, "Potosi School District General Obligation Bonds ", "Potosi School District General Obligation Bonds_"))
  
  dtemp <- sheet1 |>
    set_names(colnames$colname) |>
    select(-1) |>
    rename(reporting_unit = GE2024Jurisdiction, municipality = Municipality) |>
    pivot_longer(cols = -c(reporting_unit, municipality), names_to = "contestcandidate",
                 values_to = "votes") |>
    filter(! contestcandidate %in% c("GlobalID", "created_user", "created_date", "last_edited_user", "last_edited_date")) |>
    separate(contestcandidate, sep = "_(?!.*_)", into = c("contest", "candidate")) |>
    mutate(county = "Grant",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           reporting_unit = str_replace(reporting_unit, "MENOMOINE", "MENOMONIE"),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|^T [/]|^C [/]|^V [/]|TOWN OF |VILLAGE OF |CITY OF |^T-|^C-|^V-|^CITY |^VILLAGE |^T |^V |^C "),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           municipality = str_remove(municipality, coll(",")),
           across(where(is.character), str_squish))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################
read_vilas <- function(workbookpath, save_output = T){
  sheet1 <- read_excel(workbookpath, sheet = 1, col_names = F,
                       .name_repair = "unique_quiet", col_types = "text")
  
  colnames <- sheet1 |>
    filter(row_number() < 5,
           str_detect(...1, "^Vilas", negate = T) | is.na(...1)) |>
    mutate(rownum = row_number()) |>
    pivot_longer(cols = -rownum) |>
    pivot_wider(names_from = rownum, values_from = value) |>
    mutate(`1` = zoo::na.locf(`1`, na.rm = F),
           `2` = zoo::na.locf(`2`, na.rm = F),
           across(where(is.character), ~str_replace(.x, coll("\n"), " "))) |>
    unite("colname", `1`, `2`, `3`, na.rm = T)
  
  dtemp <- sheet1 |>
    filter(row_number() > 5,
           !is.na(...1)) |>
    set_names(colnames$colname) |>
    janitor::remove_empty("cols") |>
    rename(reporting_unit = 1) |>
    select(-34) |>
    pivot_longer(cols = -c(reporting_unit), names_to = "contestcandidate",
                 values_to = "votes") |>
    separate(contestcandidate, sep = "_(?!.*_)", into = c("contest", "candidate")) |>
    mutate(county = "Vilas",
           across(where(is.character), str_to_upper),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|^T [/]|^C [/]|^V [/]|TOWN OF |VILLAGE OF |CITY OF |^T-|^C-|^V-|^CITY |^VILLAGE |^T |^V |^C "),
           municipality = word(municipality, 1, sep = "\\bW\\b|\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           municipality = str_remove(municipality, coll("-")),
           municipality = str_remove(municipality, coll(",")),
           across(where(is.character), str_squish),
           ctv = if_else(municipality == "EAGLE RIVER", "C", "T"))
  
  if(save_output == TRUE){
    write_csv(dtemp, paste0("2024-nov/raw-processed/", str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  dtemp
}
################################################################################

################################################################################