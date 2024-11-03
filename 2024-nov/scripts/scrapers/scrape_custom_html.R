library(tidyverse)
library(rvest)

# THIS SCRIPT CONTAINS FUNCTIONS THAT SCRAPE THE HTML ELECTION RESULTS FROM
#   SELECTED COUNTIES

################################################################################
# This function scrapes the results at the given Milwaukee County Election Results URL
scrape_milwaukee <- function(electionurl, save_output = T){
  returns <- read_html(electionurl)
  all.td <- returns |>
    html_elements("td") |>
    html_text()
  all.td.df <- tibble(td = all.td) |>
    mutate(rownumber = row_number())
  table.names <- all.td.df |>
    filter(lead(td, n = 3) == "Votes")
  
  # just the election result ward tables
  results <- returns |>
    html_elements(".precinctTable") |>
    html_table()
  
  # this function processes the table from a single race
  read_race <- function(index){
    this.df <- results[[index]] |>
      select(-1)
    name.it <- this.df |> filter(row_number() == 1) |> mutate_all(as.character) |> pivot_longer(cols = everything()) |> pull(value)
    this.df <- this.df |> filter(row_number() > 1)
    names(this.df) <- name.it
    this.df |>
      mutate(race = table.names$td[index]) |>
      select(race, everything()) |>
      pivot_longer(cols = -c(race, Ward), values_to = "votes")
  }
  
  all.results <- map(.x = 1:length(results),
      .f = ~read_race(.x), .progress = T) |>
    list_rbind() |>
    select(contest = race, reporting_unit = Ward, candidate = name, votes) |>
    filter(reporting_unit != "Total") |>
    mutate(county = "Milwaukee",
           across(where(is.character), str_to_upper),
           reporting_unit = str_remove_all(reporting_unit, coll(".")),
           ctv = str_sub(reporting_unit, 1, 1),
           municipality = str_remove(reporting_unit, "VILLAGE OF |TOWN OF |CITY OF |^T |^C |^V "),
           municipality = word(municipality, 1, 1, sep = "\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"))
  
  #################################################
  # save the output with the timestamped file name
  if(save_output == T){
    write_csv(all.results, paste0("2024-nov/raw-processed/",
                                  "Milwaukee ", str_replace_all(Sys.time(), 
                                                                pattern = ":", "-"), ".csv"))
  }
  all.results
}
################################################################################

################################################################################
# This function scrapes the results at the given Jefferson County Election Results URL
scrape_jefferson <- function(electionurl, save_output = T){
  returns <- read_html(electionurl) # path to the Jefferson election page
  
  # all the text on the page
  all.txt <- returns |>
    html_elements(".container") |>
    html_text() |>
    read_delim(col_names = F, delim = "!!")
  
  # identify the contest names which have an associate precinct table
  contests.with.precinct.table <- all.txt |>
    mutate(contestname = if_else(str_detect(lead(X1, 1), "\\bVotes\\b"), str_squish(X1), NA)) |>
    mutate(contestname = zoo::na.locf(contestname, na.rm = F)) |>
    filter(str_detect(X1, "\\bPrecinct Detail\\b"))
  
  # a list of all the precinct tables
  precinct.tables <- returns |>
    html_elements(".table-bordered") |>
    html_table()
  
  # this function processes each individual precinct table
  #   pivots to long format
  #   adds contest name
  #   creates a tibble with NA values if the precinct table is empty
  process_precinct_table <- function(tableindex){
    this.table <- precinct.tables[[tableindex]] |>
      janitor::row_to_names(row_number = 1) |>
      rename("reporting_unit" = 1) |>
      mutate(contest = contests.with.precinct.table$contestname[tableindex])
    
    # create NA tibble if the precinct table is empty
    if(nrow(this.table) == 1){
      tibble(reporting_unit = NA,
             contest = contests.with.precinct.table$contestname[tableindex],
             candidate = NA,
             stat = NA)
    } else{
      this.table |>
        pivot_longer(cols = -c(reporting_unit, contest), names_to = "candidate",
                     values_to = "stat")
    }
  }
  
  # process each precinct table and bind the 
  # results into a single long format tibble
  all.results <- map(.x = 1:length(precinct.tables),
                     .f = ~process_precinct_table(.x),
                     .progress = TRUE) |>
    list_rbind() |>
    filter(str_detect(stat, coll("%"), negate = T)) |>
    select(reporting_unit, contest, candidate, votes = stat) |>
    filter(reporting_unit != "CANDIDATE TOTALS") |>
    mutate(county = "Jefferson",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           municipality = str_remove(reporting_unit, "VILLAGE OF |TOWN OF |CITY OF |^T |^C |^V "),
           municipality = word(municipality, 1, sep = coll("(")))
  
  #################################################
  # save the output with the timestamped file name
  if(save_output == T){
    write_csv(all.results, paste0("2024-nov/raw-processed/",
                                  "Jefferson ", str_replace_all(Sys.time(), 
                                                                pattern = ":", "-"), ".csv"))
  }
  all.results
}


# This function scrapes the results at the given Rock County Election Results URL
#   the webpage you first find will include the election results as an IFrame
#   Open the browser's "inspect source" tool to find the link the iframe is pointing to
#   e.g. https://elections.co.rock.wi.us/20240813.html
# https://www.co.rock.wi.us/departments/county-clerk/election-information/election-results-august-13-2024

scrape_rock <- function(electionurl, save_output = T){
  returns <- read_html(electionurl)
  
  # all of the HTML tables on the page. Some of them are precinct tables and some aren't
  all.tables <- returns |>
    html_nodes("table") |>
    html_table()
  
  # this function determines if a table is a precinct table
  #   if so, it processes it into long format
  #   if not, it returns an empty tibble
  process_precinct_table <- function(tableindex){
    this.table <- all.tables[[tableindex]]
    
    # remove tables that are empty or aren't precinct tables
    if(all(!is.na(this.table$X1)) | all(is.na(this.table))){
      tibble(reporting_unit = character(),
             contest = character(),
             candidate = character(),
             votes = character())
    } else{
      contestname <- this.table$X2[1]
      this.table |>
        janitor::remove_empty("cols") |>
        janitor::row_to_names(row_number = 1) |>
        rename(reporting_unit = 1) |>
        mutate(contest = if_else(contestname == "Precinct",
                                 true = "Voters and Ballots",
                                 false = contestname)) |>
        pivot_longer(cols = -c(reporting_unit, contest), names_to = "candidate",
                     values_to = "votes")
    }
  }
  
  # process each precinct table and bind the 
  # results into a single long format tibble
  all.results <- map(.x = 1:length(all.tables),
                     .f = ~process_precinct_table(.x)) |>
    list_rbind() |>
    mutate(county = "Rock",
           votes = str_remove_all(votes, coll(","))) |>
    type_convert() |>
    filter(reporting_unit != "Total") |>
    mutate(across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           municipality = str_remove(reporting_unit, "VILLAGE OF |TOWN OF |CITY OF |^T |^C |^V "),
           municipality = word(municipality, 1, 1, sep = "\\bW[0-9]|\\bWD|\\bWARD|\\bD[0-9]"),
           municipality = str_replace(municipality, "BELIOT", "BELOIT"))
  
  #################################################
  # save the output with the timestamped file name
  if(save_output == T){
    write_csv(all.results, paste0("2024-nov/raw-processed/",
                                  "Rock ", str_replace_all(Sys.time(), 
                                                           pattern = ":", "-"), ".csv"))
  }
  all.results
}
################################################################################

################################################################################
# Waukesha
#   e.g. https://electionresults.waukeshacounty.gov/contests.aspx?contest=-1

scrape_waukesha <- function(electionurl, save_output = T){
  results <- read_html(electionurl)
  
  # all the contest names
  contestnames <- results |>
    html_nodes(".summaryCaption") |>
    html_text() |>
    str_squish()
  
  # list of precinct table tibbles
  precinct.tables <- results |>
    html_nodes("#tblRU") |>
    html_table()
  
  # for each precinct table tibble:
  #   add the contest name
  #   pivot to long format
  all.results <- map(.x = 1:length(precinct.tables),
                     .f = function(x){
                       precinct.tables[[x]] |>
                         mutate_all(as.character) |>
                         mutate(contest = contestnames[x]) |>
                         pivot_longer(cols = -c(`Reporting Unit`, contest),
                                      names_to = "candidate", values_to = "votes")
                     }) |>
    list_rbind() |>
    select(reporting_unit = ward, contest, candidate, votes) |>
    mutate(county = "Waukesha",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           municipality = str_remove(reporting_unit, "^CITY |^VILLAGE |^TOWN "),
           municipality = word(municipality, 1, sep = "\\bW[0-9]"),
           across(where(is.character), str_squish))
  
  #################################################
  # save the output with the timestamped file name
  if(save_output == T){
    write_csv(all.results, paste0("2024-nov/raw-processed/",
                                  "Waukesha ", str_replace_all(Sys.time(), 
                                                               pattern = ":", "-"), ".csv"))
  }
  all.results
}
################################################################################

################################################################################
# This function grabs all the ward level results for a given election from the
#   Dane County elections API. It returns them in a single long format table
get_all_dane <- function(election_name, save_output = T){
  list.of.elections <- fromJSON("https://api.danecounty.gov/api/v1/elections/list")
  
  # e.g. "2024 Partisan Primary"
  this.election.id <- list.of.elections$ElectionId[list.of.elections$ElectionName == election_name]
  all.races <- fromJSON(paste0("https://api.danecounty.gov/api/v1/elections/races/",
                               this.election.id))
  all.results <- map(.x = all.races$RaceNumber,
                     .f = ~fromJSON(paste0("https://api.danecounty.gov/api/v1/elections/precinctresults/",
                                           this.election.id, "/", .x))$PrecinctVotes,
                     .progress = T) |>
    list_rbind() |>
    tibble() |>
    select(contest = RaceName, candidate = CandidateName, reporting_unit = PrecinctName,
           votes = TotalVotes) |>
    mutate(county = "Dane",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           municipality = str_remove(reporting_unit, "^T |^C |^V "),
           municipality = word(municipality, 1, sep = " WD"))
  
  #################################################
  # save the output with the timestamped file name
  if(save_output == T){
    write_csv(all.results, paste0("2024-nov/raw-processed/",
                                  "Dane ", str_replace_all(Sys.time(), 
                                                           pattern = ":", "-"), ".csv"))
  }
  all.results
}
################################################################################

################################################################################
# This function grabs all the ward level results for Wood County
#   It needs a link to the page containing further links to each precinct's page
#   like this: https://elections.woodcountywi.gov/Results/PrecinctList?ElectionId=9
read_wood <- function(precinct_page_url, save_output = T){
  precinct.list.page <- read_html(precinct_page_url)
  
  precinct.urls <- tibble(precinct_name = precinct.list.page |> html_node(".col-xl-8") |> html_nodes("td") |> html_text(),
                          precinct_url = precinct.list.page |> html_node(".col-xl-8") |> html_nodes("td") |> html_elements("a") |> html_attr("href")) |>
    mutate(precinct_url = paste0("https://elections.woodcountywi.gov", precinct_url))
  
  # this function reads an individual precinct page
  read_page <- function(precinctindex){
    page.html <- read_html(precinct.urls$precinct_url[precinctindex])
    
    # the list of race titles on the page
    race.title <- page.html |>
      html_nodes("h4") |>
      html_text()
    
    # the list of content on the page
    tlist <- page.html |>
      html_node(".col-xl-8") |>
      html_nodes("div")
    
    # this step removes the header info. It might need edited in Nov, but probably not
    tlist <- tlist[4:length(tlist)]
    
    # this function turns each chunk of content into a results table
    process_div <- function(divindex){
      t <- tlist[divindex] |>
        html_text() |>
        read_delim(delim = "!!", col_names = F, show_col_types = F) |> 
        mutate(X1 = str_squish(X1))
      
      if(nrow(t) == 2){
        tibble(candidates = character(),
               votes = character(),
               contest = character())
      } else if(nrow(t) > 2){
        start.row <- which(t$X1 == "Votes") + 1
        t |>
          filter(row_number() >= start.row) |>
          mutate(value = case_when(
            str_detect(X1, "^[:digit:]+$") ~ "votes",
            TRUE ~ "candidates"
          )) |>
          mutate(rownum = rep(1:(n()/2), each = 2)) |>
          pivot_wider(names_from = value, values_from = X1) |>
          select(-rownum) |>
          mutate(contest = race.title[divindex])
      }
    }
    
    # collect all tables for this reporting unit
    map(.x = 1:length(tlist),
        .f = ~process_div(.x)) |>
      list_rbind() |>
      mutate(reporting_unit = precinct.urls$precinct_name[precinctindex])
  }
  
  # read all precinct pages
  all.results <- map(.x = 1:nrow(precinct.urls),
                     .f = ~read_page(.x),
                     .progress = T) |>
    list_rbind() |>
    select(reporting_unit, contest, candidate = candidates, votes) |>
    mutate(county = "Wood",
           across(where(is.character), str_to_upper),
           ctv = str_sub(reporting_unit, 1, 1),
           municipality = str_remove(reporting_unit, "^T[/]|^C[/]|^V[/]|TOWN OF |VILLAGE OF |CITY OF "),
           municipality = word(municipality, 1, sep = "\\bWARD\\b|\\bWARDS\\b|\\bW[0-9]"),
           municipality = str_replace(municipality, "WISC RAPIDS", "WISCONSIN RAPIDS"))
  
  #################################################
  # save the output with the timestamped file name
  if(save_output == T){
    write_csv(all.results, paste0("2024-nov/raw-processed/",
                                  "Wood ", str_replace_all(Sys.time(), 
                                                           pattern = ":", "-"), ".csv"))
  }
  all.results
}
################################################################################
