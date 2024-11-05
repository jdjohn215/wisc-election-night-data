rm(list = ls())

library(tidyverse)
library(pdftools)
library(rvest)
library(jsonlite)

################################################################################
# county directory
file.directory <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hqq-_DTu8EmwC_0apYy2IjQL5SAry82kKlXYgBU2Yvs/edit?gid=0#gid=0") |>
  mutate(needs_xlsx_export = if_else(is.na(needs_xlsx_export), "no", needs_xlsx_export))
2
################################################################################
# Load all the source files
source("2024-nov/scripts/scrapers/pdf_reader_a.R")
source("2024-nov/scripts/scrapers/pdf_reader_b.R")
source("2024-nov/scripts/scrapers/pdf_reader_b2.R")
source("2024-nov/scripts/scrapers/pdf_reader_c.R")
source("2024-nov/scripts/scrapers/pdf_reader_d.R")
source("2024-nov/scripts/scrapers/custom_county_scrapers.R")
source("2024-nov/scripts/scrapers/scrape_custom_html.R")
source("2024-nov/scripts/download-returns.R")

################################################################################

#download_returns("Adams")
pdf_reader_a(latest_file("Adams", "raw", T))

#download_returns("Ashland")
read_ashland(latest_file("Ashland", "raw-export", T))

#download_returns("Barron")
read_barron(latest_file("Barron", "raw", T))

#download_returns("Bayfield")
read_bayfield(latest_file("Bayfield", "raw-export", T), c(2:7))

#download_returns("Brown")
pdf_reader_c(latest_file("Brown", "raw-export", T))

#download_returns("Buffalo")
read_buffalo(latest_file("Buffalo", "raw-export", T))

#download_returns("Burnett")
read_burnett(latest_file("Burnett", "raw-export", T))

#download_returns("Calumet")
pdf_reader_d(latest_file("Calumet", "raw-export", T), 1, 75)

# download_returns("Chippewa") # might have to manually download and timestamp this one
#   this won't work with zeroes
chippewa <- bind_rows(
  read_chippewa_contest(latest_file("Chippewa", "raw", fullpath = T), pagenumbers = 9:10,
                        column_names = c("US Senate - Reported",
                                         "US Senate - Baldwin",
                                         "US Senate - Hovde",
                                         "US Senate - Anderson",
                                         "US Senate - Leager",
                                         "US Senate - write-in")),
  read_chippewa_contest(latest_file("Chippewa", "raw", fullpath = T), page = 12:13,
                        column_names = c("Rep in Congress D7 - Dem_Reported",
                                         "Rep in Congress D7 - Dem_Kilbourn",
                                         "Rep in Congress D7 - Dem_Duranceau",
                                         "Rep in Congress D7 - Dem_write-in")),
  read_chippewa_contest(latest_file("Chippewa", "raw", fullpath = T), page = 62:63,
                        column_names = c("State Referendum Question 1_Yes",
                                         "State Referendum Question 1_No",
                                         "State Referendum Question 2_Yes",
                                         "State Referendum Question 2_No"))
)
write_csv(chippewa, paste0("2024-nov/raw-processed/", latest_file("Chippewa", "raw"), ".csv"))

# has to be manually downloaded
#download_returns("Clark")
read_clark(latest_file("Clark", "raw-export", T))

#download_returns("Columbia")
pdf_reader_a(latest_file("Columbia", "raw", T))

#download_returns("Crawford")
read_crawford(latest_file("Crawford", "raw-export", T))

#get_all_dane(file.directory$file_url[file.directory$county == "Dane"])

#download_returns("Dodge") # might have to manually download
pdf_reader_c(latest_file("Dodge", "raw-export", T))

#download_returns("Door")
pdf_reader_b(latest_file("Door", "raw-export", T), 1, 102)

#download_returns("Douglas")
pdf_reader_a(latest_file("Douglas", "raw", T))

# Dunn haven't processed this yet
#   bad primary file format, should be easier to process the election file

#download_returns("Eau Claire") # might have to manually download
read_eauclaire(latest_file("Eau Claire", "raw-export", T))

# Florence - not yet processed

#download_returns("Fond du Lac") # likely must download manually
pdf_reader_b2(latest_file("Fond du Lac", "raw-export", T), 1, 158)

# Forest - not yet processed

#download_returns("Grant")
pdf_reader_d(latest_file("Grant", "raw-export", T), 1, 80)

#download_returns("Green")
pdf_reader_d(latest_file("Green", "raw-export", T), 1, 22)

# Green Lake - not yet processed
#download_returns("Green Lake")
read_greenlake(latest_file("Green Lake", "raw-export", T))

# Iowa - download one drive XLSX files and manually combine into a single workbook, each file on its own sheet
read_iowa(latest_file("Iowa", "raw", T))

# Iron - reporting unit data probably not available

#download_returns("Jackson")
pdf_reader_d(latest_file("Jackson", "raw-export", T), 1, 82)

#scrape_jefferson(file.directory$file_url[file.directory$county == "Jefferson"])

# Juneau - reporting unit data likely unavailable

#download_returns("Kenosha") # this step not needed when downloading directly from Kevin's repo
read_kenosha(file.directory$file_url[file.directory$county == "Kenosha"])

#download_returns("Kewaunee")
read_kewaunee(latest_file("Kewaunee", "raw-export", T))

#download_returns("La Crosse")
pdf_reader_c(latest_file("La Crosse", "raw-export", T))

#download_returns("Lafayette")
pdf_reader_d(latest_file("Lafayette", "raw-export", T), 1, 37)

# Langlade - not yet processed

#download_returns("Lincoln")
pdf_reader_a(latest_file("Lincoln", "raw", T))

read_manitowoc(file.directory$file_type[file.directory$county == "Manitowoc"])

#download_returns("Marathon") # likely must download manually
read_marathon(latest_file("Marathon", "raw", T))

# Marinette - not yet processed

# Marquette - not yet processed
# download_returns("Marquette") # might have to download and timestamp manually
read_marquette(latest_file("Marquette", "raw-export", T), sheetvector = c(2,3,7))

# Menominee - reporting unit data probably not available

#scrape_milwaukee(file.directory$file_url[file.directory$county == "Milwaukee"])

read_monroe(file.directory$file_type[file.directory$county == "Monroe"])

#download_returns("Oconto")
read_oconto(latest_file("Oconto", "raw-export", T))

#download_returns("Oneida")
read_oneida(latest_file("Oneida", "raw", T), sheetno = 7)

#download_returns("Outagamie") # likely must download manually
pdf_reader_a(latest_file("Outagamie", "raw", T))

#download_returns("Ozaukee")
pdf_reader_b(latest_file("Ozaukee", "raw-export", T), 1, 60)

#download_returns("Pepin")
read_pepin(latest_file("Pepin", "raw-export", T))

#download_returns("Pierce")
read_pierce(latest_file("Pierce", "raw", T))

#download_returns("Polk")
read_polk(latest_file("Polk", "raw", T))

#download_returns("Portage")
pdf_reader_a(latest_file("Portage", "raw", T))

# Price - reporting unit data probably not available

#download_returns("Racine") # likely must download manually
pdf_reader_b(latest_file("Racine", "raw-export", T), 1, 100)

#download_returns("Richland")
read_richland(latest_file("Richland", "raw-export", T))

#scrape_rock(file.directory$file_url[file.directory$county == "Rock"])

read_sauk(file.directory$file_type[file.directory$county == "Sauk"])

#download_returns("Sawyer")
read_sawyer(latest_file("Sawyer", "raw-export", T))

#download_returns("Shawano")
read_shawano(latest_file("Shawano", "raw-export", T), sheetvector = 1:5)

#download_returns("Sheboygan") # likely must download manually
pdf_reader_d(latest_file("Sheboygan", "raw-export", T), 5, 89)

#download_returns("St. Croix")
pdf_reader_a(latest_file("St. Croix", "raw", T))

#download_returns("Taylor")
read_taylor(latest_file("Taylor", "raw-export", T))

# Trempeleau - not yet processed
#download_returns("Trempealeau")
read_trempealeau(latest_file("Trempealeau", "raw-export", T))

#download_returns("Vernon") # download sharepoint file manually
read_vernon(latest_file("Vernon", "raw", T))

# Vilas - not yet processed

#download_returns("Walworth")
pdf_reader_b(latest_file("Walworth", "raw-export", T), 1, 145)

#download_returns("Washburn")
read_washburn(latest_file("Washburn", "raw-export", T))

#download_returns("Washington")
pdf_reader_b2(latest_file("Washington", "raw-export", T), 2, 222)

# Waukesha - use list of URLs, one URL to each contest
scrape_waukesha(file.directory$file_url[file.directory$county == "Waukesha"])

# waupaca file has been taken down, so I copy over previously downloaded version
#download_returns("Waupaca")
pdf_reader_d(latest_file("Waupaca", "raw-export", T), 1, 99)

#download_returns("Waushara")
read_waushara(latest_file("Waushara", "raw-export", T), 1:6)

#download_returns("Winnebago")
pdf_reader_b2(latest_file("Winnebago", "raw-export", T), 1, 112)

read_wood(file.directory$file_url[file.directory$county == "Wood"])

################################################################################
