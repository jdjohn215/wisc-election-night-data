pdf_reader_b2 <- function(workbookpath, firstsheet, lastsheet, save_output = T){
  # the list of sheets in the workbook
  sheet.names <- excel_sheets(workbookpath)
  
  # this function reads an individual sheet
  read_election_sheet <- function(sheetno, workbookpath){
    sheet.full <- read_excel(workbookpath, sheet = sheetno,
                             col_names = F, .name_repair = "unique_quiet")
    table.start <- which(sheet.full$...1 == "Precinct")
    contest.name <- ifelse(table.start > 1, str_remove(sheet.full$...1[1],
                                                       coll("**** - Insufficient Turnout to Protect Voter Privacy")),
                           NA)
    df <- read_excel(workbookpath, sheet = sheetno,
                     .name_repair = "unique_quiet", col_types = "text",
                     skip = (table.start-1)) %>% # must use dplyr pipe because of column index reference in next line
      filter(! .[[1]] %in% c("Electionwide",
                             "Cumulative",
                             "Cumulative - Total",
                             "Electionwide - Total",
                             "Countywide",
                             "All Ballots",
                             "All Ballots - Total")) |>
      mutate(contest = str_squish(contest.name)) |> 
      rename(Precinct = 1) |>
      # reduce to 1 row per reporting unit
      mutate(wardstart = str_detect(Precinct, "^Town|^City|^Village")) |>
      group_by(wardstart) |>
      mutate(wardno = row_number(),
             wardno = if_else(wardstart == F, NA, wardno)) |>
      ungroup() |>
      mutate(wardno = zoo::na.locf(wardno)) |>
      type_convert() |>
      group_by(wardno) |>
      mutate(across(where(is.numeric), .f = ~max(.x, na.rm = T))) |>
      ungroup() |>
      filter(! Precinct %in% c("Total", "Election Day")) |>
      mutate_all(as.character) |>
      select(-c(wardstart, wardno))
    
    # remove duplicate precinct column, if it exists
    if(length(which(str_detect(names(df), "Precinct"))) == 2){
      df <- df |>
        select(-which(str_detect(names(df), "Precinct"))[2]) |>
        rename(Precinct = 1)
    }
    
    # remove empty and percentage columns
    df |>
      select(-starts_with("..."))
  }
  
  # process all the sheets individually
  all.sheets <- map(.x = firstsheet:lastsheet,
                    .f = ~read_election_sheet(.x, 
                                              workbookpath = workbookpath), 
                    .progress = T)
  
  # what range of sheets correspond to each race?
  contest.sheets <- map(.x = 1:length(all.sheets),
                        .f = function(x){
                          tibble(sheetnumber = x,
                                 contestname = unique(all.sheets[[x]]$contest))
                        }) |>
    list_rbind() |>
    mutate(contestname = if_else(sheetnumber == 1, "Turnout summary", contestname),
           contestname = zoo::na.locf(contestname)) |>
    group_by(contestname) |>
    summarise(first_sheet = min(sheetnumber),
              last_sheet = max(sheetnumber))
  
  # combine all the results from each race into a long-format tibble
  all.results <- map(.x = 1:nrow(contest.sheets),
                     .f = function(x){
                       list_rbind(all.sheets[contest.sheets$first_sheet[x]:contest.sheets$last_sheet[x]]) |>
                         select(-contest) |>
                         pivot_longer(cols = -Precinct) |>
                         filter(!is.na(value)) |>
                         mutate(contest = contest.sheets$contestname[x])
                     }) |>
    list_rbind() |>
    select(reporting_unit = Precinct, contest, candidate = name, votes = value) |>
    filter(contest != "Turnout summary") |>
    type_convert() |>
    mutate(county = word(word(workbookpath, -1, sep = "/"), 1, sep = " 20"),
           across(where(is.character), str_to_upper),
           across(where(is.character), ~str_remove_all(.x, coll("\n"))),
           across(where(is.character), ~str_remove_all(.x, coll("."))),
           ctv = str_sub(reporting_unit, 1, 1),
           municipality = str_remove(reporting_unit, "TOWN OF |VILLAGE OF |CITY OF "),
           municipality = word(municipality, 1, 1, sep = coll(","))) |>
    filter(municipality != "ALL BALLOTS - TOTAL",
           municipality != "COUNTYWIDE - TOTAL",
           municipality != "TOTAL",
           municipality != "DOOR COUNTY - TOTAL",
           municipality != "COUNTY - TOTAL",
           municipality != "ELECTION DAY",
           ! candidate %in% c("TOTAL VOTES", "TIMES CAST", "REGISTERED VOTERS"))
  
  #################################################
  # save the output with the timestamped file name
  if(save_output == T){
    write_csv(all.results, paste0("2024-nov/raw-processed/",
                                  str_remove(word(workbookpath, -1, sep = "/"), ".pdf|.xlsx"), ".csv"))
  }
  
  all.results
}

