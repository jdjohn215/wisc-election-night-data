rm(list = ls())

library(tidyverse)
library(pdftools)
library(rvest)
library(jsonlite)

################################################################################
# Scrape PDFs (including PDFs converted to XLSX using Adobe Acrobat's native convertor)
source("statewide/scrapers/pdf_reader_a.R")
source("statewide/scrapers/pdf_reader_b.R")
source("statewide/scrapers/pdf_reader_c.R")
source("statewide/scrapers/pdf_reader_d.R")


# Counties with format A
adams <- pdf_reader_a("statewide/raw/adams.pdf")
columbia <- pdf_reader_a("statewide/raw/columbia.pdf")
lincoln <- pdf_reader_a("statewide/raw/lincoln.pdf")
marathon <- pdf_reader_a("statewide/raw/marathon.pdf")
outagamie <- pdf_reader_a("statewide/raw/outagamie.pdf")
portage <- pdf_reader_a("statewide/raw/portage.pdf")
stcroix <- pdf_reader_a("statewide/raw/stcroix.pdf")

# Counties with format B
washington <- pdf_reader_b("statewide/raw-export/Washington.xlsx", 2, 224)
ozaukee <- pdf_reader_b("statewide/raw-export/Ozaukee.xlsx", 1, 149)
door <- pdf_reader_b("statewide/raw-export/Door.xlsx", 1, 102)
fonddulac <- pdf_reader_b("statewide/raw-export/FondDuLac.xlsx", 1, 158)
racine <- pdf_reader_b("statewide/raw-export/Racine.xlsx", 1, 249)
walworth <- pdf_reader_b("statewide/raw-export/Walworth.xlsx", 1, 145)
winnebago <- pdf_reader_b("statewide/raw-export/Winnebago.xlsx", 1, 217)

# Counties with format C
brown <- pdf_reader_c("statewide/raw-export/brown.xlsx")
dodge <- pdf_reader_c("statewide/raw-export/dodge.xlsx")
eauclaire <- pdf_reader_c("statewide/raw-export/EauClaire.xlsx")
kenosha <- pdf_reader_c("statewide/raw-export/Kenosha.xlsx")
lacrosse <- pdf_reader_c("statewide/raw-export/LaCrosse.xlsx")

# Counties with format D
grant <- pdf_reader_d("statewide/raw-export/grant.xlsx", 1, 80)
jackson <- pdf_reader_d("statewide/raw-export/jackson.xlsx", 1, 82)
lafayette <- pdf_reader_d("statewide/raw-export/lafayette.xlsx", 1, 37)
sheboygan <- pdf_reader_d("statewide/raw-export/sheboygan.xlsx", 5, 89)
waupaca <- pdf_reader_d("statewide/raw-export/waupaca.xlsx", 1, 99)
calumet <- pdf_reader_d("statewide/raw-export/calumet.xlsx", 1, 52)
green <- pdf_reader_d("statewide/raw-export/green.xlsx", 1, 22)
################################################################################

################################################################################
# Counties with custom HTML scrapers
source("statewide/scrapers/scrape_custom_html.R")

milwaukee <- scrape_milwaukee("https://county.milwaukee.gov/EN/County-Clerk/Off-Nav/Election-Results/8-13-24-Partisan-Primary---Unofficial-Results")
jefferson <- scrape_jefferson("https://apps.jeffersoncountywi.gov/jc/election/results/8132024")
rock <- scrape_rock("https://elections.co.rock.wi.us/20240813.html")
waukesha <- scrape_waukesha("https://electionresults.waukeshacounty.gov/contests.aspx?contest=-1")
dane <- get_all_dane("2024 Partisan Primary")
wood <- read_wood("https://elections.woodcountywi.gov/Results/PrecinctList?ElectionId=9")
################################################################################

################################################################################
# Counties with custom PDF or excel scrapers
source("statewide/scrapers/custom_county_scrapers.R")

ashland <- read_ashland("statewide/raw-export/ashland.xlsx")
barron <- read_barron("statewide/raw/barron.xlsx")
bayfield <- read_bayfield("statewide/raw-export/bayfield.xlsx", c(2:7))
clark <- read_clark("statewide/raw-export/clark.xlsx")
polk <- read_polk("statewide/raw/polk.xlsx")
pierce <- read_pierce("statewide/raw/pierce.xlsx")
oneida <- read_oneida("statewide/raw/oneida.xlsx", sheetno = 7)
vernon <- read_vernon("statewide/raw/vernon.xlsx", sheetno = 2)
waushara <- read_waushara("statewide/raw-export/waushara.xlsx", sheetvector = 1:6)
oconto <- read_oconto("statewide/raw-export/oconto.xlsx", sheetno = 2)
shawano <- read_shawano("statewide/raw-export/shawano.xlsx", sheetvector = 1:5)
richland <- read_richland("statewide/raw-export/richland.xlsx")
sawyer <- read_sawyer("statewide/raw-export/sawyer.xlsx")
taylor <- read_taylor("statewide/raw-export/taylor.xlsx")

# chippewa is more complicated
chippewa <- bind_rows(
  read_chippewa_contest("statewide/raw/chippewa.pdf", pagenumbers = 10:11,
                        column_names = c("US Senate - Dem_Reported",
                                         "US Senate - Dem_Baldwin",
                                         "US Senate - Dem_write-in",
                                         "Rep in Congress D3 - Dem_Reported",
                                         "Rep in Congress D3 - Dem_Cooke",
                                         "Rep in Congress D3 - Dem_Shankland",
                                         "Rep in Congress D3 - Dem_Wilson",
                                         "Rep in Congress D3 - Dem_write-in")),
  read_chippewa_contest("statewide/raw/chippewa.pdf", page = 12:13,
                        column_names = c("Rep in Congress D7 - Dem_Reported",
                                         "Rep in Congress D7 - Dem_Kilbourn",
                                         "Rep in Congress D7 - Dem_Duranceau",
                                         "Rep in Congress D7 - Dem_write-in")),
  read_chippewa_contest("statewide/raw/chippewa.pdf", page = 62:63,
                        column_names = c("State Referendum Question 1_Yes",
                                         "State Referendum Question 1_No",
                                         "State Referendum Question 2_Yes",
                                         "State Referendum Question 2_No"))
)
