
# the most recent file
#   read the file for a given county in a given folder with the most recent timestamp
#   - optionally, include the full path or just the bare filename (without extension)
latest_file <- function(county, folder, fullpath = F){
  # list of valid countynames
  county.names <- readRDS("county-names.rds")
  stopifnot(county %in% county.names)
  
  # all of this county's files
  all.files <- county_files(county, folder)
  if(fullpath == F){
    all.files$filename[all.files$downloaded_at == suppressWarnings(max(all.files$downloaded_at))]
  } else if(fullpath == T){
    all.files$fullpath[all.files$downloaded_at == suppressWarnings(max(all.files$downloaded_at))]
  }
}

# this function returns the list of files downloaded for the given county
#   the output is a tibble with columns for filename, update datetime, and
#     whether or not the PDF is different from the previously downloaded PDF
county_files <- function(county, folder){
  # list of valid countynames
  county.names <- readRDS("county-names.rds")
  stopifnot(county %in% county.names)
  
  # all of this county's files
  tibble(fullpath = list.files(paste0("2024-nov/", folder),
                               full.names = T,
                               pattern = paste0(county, " 2024"))) |>
    mutate(filename = word(str_remove(fullpath, ".pdf|.xlsx|.csv"), -1, sep = "/"),
           downloaded_at = lubridate::as_datetime(word(filename, -2, -1))) |>
    arrange(downloaded_at) |>
    mutate(identical_to_latest = tools::md5sum(fullpath) == tools::md5sum(fullpath[downloaded_at == suppressWarnings(max(downloaded_at))]))
}

# this function downloads the file for a given county
#   checks if the file is identical to previous files
#   - if yes, rename raw-export and raw-processed files to the latest timestamp
#   - if no, recommend next steps (i.e. export and run scraper function)
#   - finally, delete old files identical to the current file
download_returns <- function(countyname){
  stopifnot(countyname %in% file.directory$county)
  county.values <- file.directory |> filter(county == countyname)
  path <- county.values$file_url
  filetype <- county.values$file_type
  
  stopifnot(filetype %in% c("googledrive pdf", "pdf", "xlsx"))
  
  # use the appropriate download function based on filetype
  switch(filetype,
         "googledrive pdf" = googledrive::drive_download(file = county.values$file_url,
                                                         path = paste0("2024-nov/raw/",
                                                                       countyname, " ", 
                                                                       str_replace_all(Sys.time(), 
                                                                                       pattern = ":", "-"),
                                                                       ".pdf")),
         "xlsx" = download.file(url = county.values$file_url,
           destfile = paste0("2024-nov/raw/", countyname, " ",
                             str_replace_all(Sys.time(), pattern = ":", "-"), ".xlsx")),
         "pdf" = download.file(url = county.values$file_url,
                               destfile = paste0("2024-nov/raw/", countyname, " ",
                                                 str_replace_all(Sys.time(), pattern = ":", "-"),
                                                 ".pdf"))
         )
  
  # evaluate if the new file is different from the previous file
  identical.files <- county_files(countyname, "raw") |> 
    filter(identical_to_latest == T)
  
  # if an identical raw-export file exists, update timestamp
  if(!is_empty(latest_file(countyname, "raw-export"))){
    if(latest_file(countyname, "raw-export") %in% identical.files$filename){
      print("Identical: updating timestamp of raw-export file")
      file.copy(from = latest_file(countyname, "raw-export", fullpath = T),
                to = paste0("2024-nov/raw-export/", latest_file(countyname, "raw"), ".xlsx"))
    }
  }
  
  # if an identical raw-processed file exists, update timestamp
  if(!is_empty(latest_file(countyname, "raw-processed"))){
    if(latest_file(countyname, "raw-processed") %in% identical.files$filename){
      print("Identical: updating timestamp of raw-processed file")
      file.copy(from = latest_file(countyname, "raw-processed", fullpath = T),
                to = paste0("2024-nov/raw-processed/", latest_file(countyname, "raw"), ".csv"))
    }
  } else if(county.values$needs_xlsx_export == "yes"){
    print(paste("export", latest_file(countyname, "raw", fullpath = T), 
                "to XLSX. Then run",
                paste0(county.values$processing_function, "(2024-nov/raw-export/",
                       latest_file(countyname, "raw"), ".xlsx)")))
  } else {
    print(paste0("run ", county.values$processing_function, "(",
                 latest_file(countyname, "raw", fullpath = T), ")"))
  }
  
  # identify files to remove. Only keep the most recent identical file
  files.to.remove <- identical.files |> arrange(desc(downloaded_at)) |> filter(row_number() > 1) |> pull(filename)
  county_files(countyname, "raw-export") |> filter(filename %in% files.to.remove) |> pull(fullpath) |> unlink()
  county_files(countyname, "raw-processed") |> filter(filename %in% files.to.remove) |> pull(fullpath) |> unlink()
  county_files(countyname, "raw") |> filter(filename %in% files.to.remove) |> pull(fullpath) |> unlink()
}

# download_returns("Ashland")
# download_returns("Barron")
# download_returns("Bayfield")
# download_returns("Adams")

