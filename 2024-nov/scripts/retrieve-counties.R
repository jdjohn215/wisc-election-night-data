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

#download_returns("Barron") # manually copy/pasted file
read_barron(latest_file("Barron", "raw", T))

#download_returns("Bayfield")
read_bayfield(latest_file("Bayfield", "raw-export", T), c(1:3))

#download_returns("Brown")
pdf_reader_c(latest_file("Brown", "raw-export", T))

#download_returns("Buffalo")
read_buffalo(latest_file("Buffalo", "raw-export", T))

#download_returns("Burnett")
read_burnett(latest_file("Burnett", "raw-export", T))

#download_returns("Calumet")
pdf_reader_a(latest_file("Calumet", "raw", T))

# download_returns("Chippewa") # might have to manually download and timestamp this one
#   manually edited
read_chippewa(latest_file("Chippewa", "raw-export", T))

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
pdf_reader_b(latest_file("Door", "raw-export", T), sheetvector = c(1:4,6:7,9:44))

#download_returns("Douglas")
pdf_reader_a(latest_file("Douglas", "raw", T))

#download_returns("Dunn")
read_dunn(latest_file("Dunn", "raw-export", T))

#download_returns("Eau Claire") # might have to manually download
read_eauclaire(latest_file("Eau Claire", "raw-export", T))

#download_returns("Florence")
read_florence(latest_file("Florence", "raw-export", T))

#download_returns("Fond du Lac") # likely must download manually
pdf_reader_b2(latest_file("Fond du Lac", "raw-export", T), c(1:5,7:8,10:11,13:76))

#download_returns("Forest")
read_forest(latest_file("Forest", "raw-export", T))

#download_returns("Grant")
read_grant(latest_file("Grant", "raw", T))

#download_returns("Green")
#pdf_reader_d(latest_file("Green", "raw-export", T), 1, 5) # Lacks reporting units

#download_returns("Green Lake")
read_greenlake(latest_file("Green Lake", "raw-export", T), sheetvector = 1:4)

# Iowa - download one drive XLSX files and manually combine into a single workbook, each file on its own sheet
read_iowa(latest_file("Iowa", "raw", T))

# Iron - reporting unit data probably not available

#download_returns("Jackson")
read_jackson(latest_file("Jackson", "raw-export", T)) # had to manually edit

#scrape_jefferson(file.directory$file_url[file.directory$county == "Jefferson"])

#download_returns("Juneau")
read_juneau(latest_file("Juneau", "raw-export", T))

#download_returns("Kenosha")
#read_kenosha(file.directory$file_url[file.directory$county == "Kenosha"])
pdf_reader_c(latest_file("Kenosha", "raw-export", T))

#download_returns("Kewaunee")
read_kewaunee(latest_file("Kewaunee", "raw-export", T))

#download_returns("La Crosse")
pdf_reader_c(latest_file("La Crosse", "raw-export", T))

#download_returns("Lafayette")
# manually edited XLSX format
read_lafayette(latest_file("Lafayette", "raw-export", T))

#download_returns("Langlade")
read_langlade(latest_file("Langlade", "raw-export", T))

#download_returns("Lincoln")
pdf_reader_a(latest_file("Lincoln", "raw", T))

read_manitowoc(file.directory$file_type[file.directory$county == "Manitowoc"])

#download_returns("Marathon") # likely must download manually
read_marathon(latest_file("Marathon", "raw", T))

#download_returns("Marinette")
read_marinette(latest_file("Marinette", "raw-export", T), sheetvector = 2:4)

# download_returns("Marquette") # might have to download and timestamp manually
read_marquette(latest_file("Marquette", "raw-export", T), sheetvector = c(2,3,4))

#download_returns("Menominee")
# hand-entered
read_menominee(latest_file("Menominee", "raw-export", T))

#scrape_milwaukee(file.directory$file_url[file.directory$county == "Milwaukee"])

#download_returns("Monroe")
read_monroe(latest_file("Monroe", "raw-export", T))

#download_returns("Oconto")
read_oconto(latest_file("Oconto", "raw-export", T))

#download_returns("Oneida")
read_oneida(file.directory$file_url[file.directory$county == "Oneida"])

#download_returns("Outagamie") # likely must download manually
pdf_reader_a(latest_file("Outagamie", "raw", T))

#download_returns("Ozaukee")
pdf_reader_b(latest_file("Ozaukee", "raw-export", T), c(1:60))

#download_returns("Pepin")
read_pepin(latest_file("Pepin", "raw-export", T))

#download_returns("Pierce")
read_pierce(latest_file("Pierce", "raw", T))

#download_returns("Polk")
read_polk(latest_file("Polk", "raw", T))

#download_returns("Portage")
pdf_reader_a(latest_file("Portage", "raw", T))

#download_returns("Price")
read_price(latest_file("Price", "raw-export", T), 1)

#download_returns("Racine") # likely must download manually
pdf_reader_b(latest_file("Racine", "raw-export", T), sheetvector = c(1:100))

#download_returns("Richland")
read_richland(latest_file("Richland", "raw-export", T))

#scrape_rock(file.directory$file_url[file.directory$county == "Rock"])

#download_returns("Rusk")
read_rusk(latest_file("Rusk", "raw-export", T))

read_sauk(file.directory$file_type[file.directory$county == "Sauk"])

#download_returns("Sawyer")
read_sawyer(latest_file("Sawyer", "raw-export", T))

#download_returns("Shawano")
read_shawano(latest_file("Shawano", "raw-export", T), sheetvector = 1:5)

#download_returns("Sheboygan") # likely must download manually
#pdf_reader_d(latest_file("Sheboygan", "raw-export", T), 5, 25) # needs some work

#download_returns("St. Croix")
pdf_reader_a(latest_file("St. Croix", "raw", T))

#download_returns("Taylor")
read_taylor(latest_file("Taylor", "raw-export", T))

# Trempeleau - not yet processed
#download_returns("Trempealeau")
read_trempealeau(latest_file("Trempealeau", "raw-export", T), sheetvector = 1:3)

#download_returns("Vernon") # download sharepoint file manually
read_vernon(latest_file("Vernon", "raw", T))

#download_returns("Vilas")
read_vilas(latest_file("Vilas", "raw-export", T))

#download_returns("Walworth")
pdf_reader_b(latest_file("Walworth", "raw-export", T), c(1:4,6:7,9:67))

#download_returns("Washburn")
read_washburn(latest_file("Washburn", "raw-export", T))

#download_returns("Washington")
pdf_reader_b2(latest_file("Washington", "raw-export", T), sheetvector = c(2:37,40:93))

# Waukesha - use list of URLs, one URL to each contest
scrape_waukesha(file.directory$file_url[file.directory$county == "Waukesha"])

#download_returns("Waupaca")
pdf_reader_d(latest_file("Waupaca", "raw-export", T), 1, 99)

#download_returns("Waushara")
read_waushara(latest_file("Waushara", "raw-export", T), 1:4)

#download_returns("Winnebago")
pdf_reader_b2(latest_file("Winnebago", "raw-export", T), sheetvector = c(1:112))

read_wood(file.directory$file_url[file.directory$county == "Wood"])

################################################################################
