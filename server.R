####### SETUP  ####### 

# Libraries
library(shinydashboard)
library(rangeBuilder)
library(tidyverse)
library(plyr)
library(lubridate)
library(rvest)
library(rworldmap)
library(clinUtils)
library(DT)


# Source scripts
source("./azure_functions.R")

# Load data
locations <- getTable("Locations")
collectclosecontacts <- getTable("CollectContactsCalls")
cases <- getTable("cases")

function(input, output, session) {
  
  # Web scraping and defining green countries}
  
  ## Scrape green country list from NI Direct website as of 18/08/21
  green.url <- "https://www.nidirect.gov.uk/node/14416#toc-1"
  green.countries <- read_html(green.url) %>% 
    html_nodes("main") %>% 
    html_nodes("article") %>% 
    html_nodes('ul')
  green.countries <- html_text(green.countries[2])
  
  ## Tidy Scraped Data
  green.countries <- str_split(green.countries, pattern = "\n\t") #Split single string into each country 
  green.countries <- as.data.frame(green.countries)
  colnames(green.countries) <- "country"
  green.countries <- gsub("\\s*\\([^\\)]+\\)","", as.character(green.countries$country)) #Remove brackets and everything in them
  green.countries <- str_remove(green.countries, "[\n]")
  green.countries <- str_remove(green.countries, " and Jerusalem")
  green.countries <- as.data.frame(green.countries)
  colnames(green.countries) <- "country"
  green.countries$status <- "Green"
  green.countries <- lapply(green.countries, str_trim) #Trim whitespace
  green.countries <- as.data.frame(green.countries)
  
  # Web scraping and defining red countries
  
  ## Scrape red country list from NI Direct website
  red.url <- "https://www.nidirect.gov.uk/articles/coronavirus-covid-19-travelling-red-list-country#toc-0"
  red.countries <- read_html(red.url) %>% 
    html_nodes("main") %>% 
    html_nodes("article") %>% 
    html_nodes('ul')
  red.countries <- html_text(red.countries[2])
  
  ## Tidy Scraped Data
  red.countries <- str_split(red.countries, pattern = "\n\t") #Split single string into each country 
  red.countries <- as.data.frame(red.countries)
  colnames(red.countries) <- "country"
  red.countries <- gsub("\\s*\\([^\\)]+\\)", " ", as.character(red.countries$country)) #Remove brackets and everything in them
  red.countries <- str_remove(red.countries, "[\n]")
  red.countries <- as.data.frame(red.countries)
  colnames(red.countries) <- "country"
  red.countries$status <- "Red"
  red.countries <- lapply(red.countries, str_trim) #Trim whitespace 
  red.countries <- as.data.frame(red.countries)
  
  # Define Common Travel Area
  CTA <- as.data.frame(c("England", "Scotland", "Wales", "Isle of Man", "Ireland", "Guernsey", "Jersey")) 
  CTA$status <- "Green"
  colnames(CTA) <- c("country", "status")
  
  ## Define all countries in world
  all.countries <- countryExData[ , 2] #rworldmap package which lists all countries 
  all.countries <- gsub("Dem. Rep. Congo","Democratic Republic of Congo", all.countries, fixed = T) 
  all.countries <- gsub("Rep.","Republic", all.countries, fixed = T) 
  all.countries <- gsub("Viet Nam","Vietnam", all.countries, fixed = T) 
  all.countries <- as.data.frame(all.countries)
  all.countries$status <- NA
  colnames(all.countries) <- c("country", "status")
  
  ## Amber countries are countries not on red or green lists or CTA
  non.amber.countries <- rbind(red.countries, green.countries, CTA)
  
  ## Remove countries that appear in green and red lists and CTA
  amber.countries <- anti_join(all.countries, non.amber.countries, by = "country")
  amber.countries$status <- "Amber"
  
  # Country Status data frame
  
  all.countries.status <- rbind(amber.countries, non.amber.countries)
  all.countries.status <- all.countries.status[order(all.countries.status$country), ] 
  current.country.status <- all.countries.status$status
  
  
  country.status <- data.frame(matrix(NA, nrow = 189, ncol = 52))
  
  epiweek <- (paste0("Epiweek", 1:52))
  colnames(country.status) <- epiweek
  old.epiweeks <- (paste0("Epiweek", 1:31))
  
  current.epiweek <- paste0("Epiweek", strftime(Sys.Date(), format = "%V"))
  
  # Assign this weeks data
  status.assignment <- country.status %>% 
    dplyr::select(current.epiweek) 
  status.assignment[,1] <- current.country.status
  country.status <- replace(country.status, current.epiweek, status.assignment[,1])
  country.status$Epiweek32 <- country.status$Epiweek34
  country.status$Epiweek33 <- country.status$Epiweek34
  country.status$country <- all.countries.status$country
  
  write.csv(country.status, "country.status.backup.csv")
  
  
  
  # Old Country Status lists (Prior to Changes Implemented August 8th 2021)
  
  ## Green
  green.changes <- data.frame("country" = c("Austria", "Germany", "Latvia", "Norway", "Romania", "Slovenia", "Slovakia")) #These countries were added to green list August 8th 21
  old.green.countries <- anti_join(x = green.countries, y = green.changes, by = "country")
  
  ## Red
  red.changes <- data.frame("country" = c("Georgia", "La Reunion", "Mayotte", "Mexico")) #These countries were added to red list August 8th 21
  old.red.countries <- anti_join(x = red.countries, y = red.changes, by = "country")
  
  ## Amber
  old.non.amber.countries <- rbind(old.red.countries, old.green.countries, CTA)
  old.amber.countries <- anti_join(all.countries, old.non.amber.countries, by = "country")

  
  ## Old Country Status data frame
  old.all.countries.status <- rbind(old.amber.countries, old.non.amber.countries)
  missed.countries <- anti_join(all.countries.status, old.all.countries.status, by = "country")
  old.amber.countries <- rbind(old.amber.countries, missed.countries)
  old.amber.countries$status <- "Amber"
  
  old.all.countries.status <- rbind(old.amber.countries, old.non.amber.countries)
  old.all.countries.status <- old.all.countries.status[order(old.all.countries.status$country), ] 
  old.country.status <- old.all.countries.status$status
  
  # Add old cases (pre-august 8th, pre epiweek 32) to country.status data frame
  country.status <- replace(country.status, old.epiweeks, old.country.status) 
  
  ## Pivot Countries
  pivot.countries <- pivot_longer(country.status, !country, names_to = "Epiweek", values_to = "Status")
  
  # Define travellers
  
  ## Make data frame
  travellers <- locations %>%
    filter(TypeOfPlace == "Travel outside Northern Ireland") %>%
    left_join(collectclosecontacts, by = c("CollectCallId" = "Id")) %>%
    left_join(cases, by = "CaseNumber") %>%
    dplyr::select(CountriesVisited.x, WhenDidYouLeaveNorthernIreland, WhenDidYouReturnToNorthernIreland, CaseNumber, FirstName, LastName, Gender.x, CreatedOn) %>% 
    filter(WhenDidYouReturnToNorthernIreland >= "2021-01-01") %>% 
    mutate(Epiweek = paste0("Epiweek", strftime(WhenDidYouReturnToNorthernIreland, format = "%V"))) 

  ## Tidy pre drop down data
  travellers$CountriesVisited.x <- lapply(travellers$CountriesVisited.x, str_trim)
  
  ### START ####
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ENGLAND"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "england"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England - Liverpool"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England - Manchester"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Liverpool"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "London"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "R.O.I"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "R.O.I."] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "R O I"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic Of Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "REPUBLIC OF IRELAND"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "RoI"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "republic of Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Edinburgh"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Glasgow"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "edinburgh"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "glasgow"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Glasgow Scotland"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "scotland"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Dublin"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Engand"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England (Liverpool)"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England, Manchester"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England."] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Reading"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "RoI - Bundoran Donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "tenerife- Spain"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Blackpool england"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Cairnryan"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "roi"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England,"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England (London)"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England Yorkshire"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI Quigley's Point , Co Donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Dublin Airport Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Asda Lutterwoth"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "At university in Edinburgh"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "belfast-london gatwick on 03/01/2021"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Belfast to Leeds Bradford and on to Harrogate"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Birmimgham , England"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Birmingham , England"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Birmingham england"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Birmingham England"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Birmingham UK"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Blackpool"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Bridge End,Co Donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "BULGARIA"] <- "Bulgaria"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Bridgeend service station Co. Donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Buncrana"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Caffery International LTD Coolfore, Ashbourne Co Meath, Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Cairnryan Ferry terminal Scotland"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Canada and Republic of Ireland"] <- "Canada"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Car journey to Dundalk county Louth Ireland for two hours shopping"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "case flew in from England on Friday 25th June"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Case went to stay with his sister Margaret McGrory in Cavan town, Co Cavan. exact address and phone number not available"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Caseys Caravan and Camping Park,Donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Central England for work purposes travel postcode LE 671ER and NG31 9SP"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Centreparcs Whinfell Old Sawmill Cottages 2 Whinfell, Whinfell CA10 2DW"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Co Kildare, Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Co Tipperary, Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Cork, RoI"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "cornwall England"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "CORNWALL ENGLAND"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "County Donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "County Louth"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Cyprus & England"] <- "Cyprus"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Cyprus Athens"] <- "Athens, Cyprus"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Day trip to Leeds to visit son for first time this year.I believe I caught COVID from him as his household has tested positive and he’s awaiting results"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Doha , Qatar into Dublin Airport"] <- "Qatar"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Donegal - ROI"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Donegal ROI"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Donegal,ROI"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Donegall. South of Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Dubai Dublin"] <- "United Arab Emirate"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Dublin airport"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Dublin Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Dublin ROI"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Dublin, Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Dublin, Republic of Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Dundalk"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Dundalk ROI"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Dungloe Site with work. Travelled each day to the work"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Edinburgh, Scotland"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ENgland"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England- I live and work in Liverpool and I came home to Northern Ireland for Christmas"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "england- leeds"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England- Liverpool"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England- Stevenage"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England-Liverpool"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England-London. Did not leave Gatwick airport as stayed in airport hotel-the Hilton"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England - 4 Hall Farm Close SK7 6PJ"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "england - leeds"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England - Leeds"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England - London"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England - United Kingdom"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England Travelled from Gatwick to Dublin"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England (150 Scar Lane, Huddersfield HD3 4PY)"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England (Blackpool and Cromer in Norfolk)"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England and also Ibiza"] <- "Balearic Islands, England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England and Scotland"] <- "England, Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England and Wales"] <- "England, Wales"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England to NI"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England to NI then onwards to Co. Leitrim ROI"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "england to NI trip"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "england to Northern Ireland"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England uk"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England Cornwall St Ives"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England Scotland"] <- "England, Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England Working Hindhead Tunnell, London"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England, Cornwall"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England, Huddersfield."] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England, London"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England. Scotland"] <- "England, Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Farnham in England for work."] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Flew from Liverpool to Belfast"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Flight from England to Northern Ireland"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Flights from Liverpool to Belfast"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Flying from Manchester to Dublin. The. Transitioning to Northern Ireland."] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "France and England	"] <- "England, France"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "France Monte Carlo, Monacco"] <- "France, Monacco"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "From Glasgow to Belfast"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "G B"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Galway clinic Galway Clinic Doughiska Co. Galway H91HHT0 Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Galway clinic Galway Clinic Doughiska Co. Galway H91HHT0 Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "gatwick- belfast return journey on same day , 03/01/2021"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Gatwick airport to Blackbushe"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Glasgow, UK"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Home from England to Belfast"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "I came from London, England"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "I came from the Ireland on Friday for Christmas."] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "I came home from Liverpool"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "I come back from Poland through Ireland."] <- "Poland, Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "I flew from London Gatwick on 20/12/2020"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "I flew to Scotland in 13/7 with my husband . I have tested positive in Scotland and an isolating there. I have given all details to Scotland track and trace over the phone"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "I flew with BA from Sao Paulo in Brazil to London Heathrow and then to Belfast City."] <- "Brazil, England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "I live in republic of ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "I traveled from England to my home in NI"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "I travelled from Liverpool to Derry airport"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "I travelled home from Hungary on Friday 4th December. From work. Ryanair 0720 I think from Budapest."] <- "Hungary"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "I travelled home from university at Cambridge to Belfast via Birmingham Airport"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "I visited England for work, I got a flight home on Thursday 04 March 2021."] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "I work in an accounting practice in the Republic of Ireland, but the work page would not accept that address."] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "India (New Delhi) to Heathrow then Heathrow to Belfast."] <- "England, India"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "India Southern Ireland"] <- "India"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Iraq - Turkey - London - Belfast City"] <- "England, Iraq, Turkey"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "IRELAND"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Ireland - AK Fuels, Dundalk Co Louth"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Ireland - Dundalk -County Louth	"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Ireland - I work in the ROI"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Ireland -Cavan"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Ireland, Tayto Park"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Ireland, Wales and England"] <- "Wales"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Isle of Wight Southampton, England	"] <- "Isle of Wight, England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Ivory Coast -Abidjan France-Paris London to Belfast"] <- "England, France, Ivory Coast"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Kerala"] <- "India"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Kilbrew, Ashbourne A84"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Kildare"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "kildare ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Kyrgyzstan Return Journey connecting flights.-Moscow, Amsterdam,Belfast"] <- "Amsterdam, Kyrgyzstan, Moscow"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Lanzarote,Canary Isles"] <- "Canary Islands"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Lebanon Turkey Republic of Ireland"] <- "Turkey"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Letterkenny in Donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Live in ROI"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Lived in Canada and moved back to North of Ireland	"] <- "Canada"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Liveepool"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "LIVERPOOL"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Liverpool , England"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "liverpool england"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Liverpool England"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Liverpool in England"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Liverpool UK"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Liverpool, England"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Liverpool, England (12/07/2021 - 15/07/2021)	"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Liverpool, north Wales"] <- "England, Wales"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Liverpool, UK"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Lives in Donegal, works in NI"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Lives in Republic of Ireland and travelled to Warsaw, Poland via Dublin"] <- "Poland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Lives in ROI (Donegal) but works in Londonderry Aghilly Buncrana BT99 5IV"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "london"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "London -GB"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "London GB"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "London, England"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "London, GB"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "London."] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Luton"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Mainland UK"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Mainland UK - London"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Majorca"] <- "Balearic Islands"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Majorca, spain"] <- "Balearic Islands"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Manchester"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "McBurneys Balyymena"] <- "NI"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "McBurneys to drop off cab."] <- "NI"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Netherlands and England"] <- "England, Netherlands"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "New Delhi to London Heathrow then connecting flight to Belfast after quarantine."] <- "England, India"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Newcastle"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Newcastle - UK"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Nigeria United Kingdom (London)"] <- "England, Nigeria"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "North Tipperary Nenagh"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Northern Ireland to Scotland England Scotland to Northern Ireland"] <- "England, Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Playa De Las Americas. Tenerife"] <- "Canary Islands"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "poland"] <- "Poland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "POLAND"] <- "Poland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Poland ROI"] <- "Poland, Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Portsalon co. Donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Portsmouth, England"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "portugal"] <- "Portugal"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "PORTUGAL"] <- "Portugal"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Portugal R.O.I"] <- "Portugal"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Portugal Rep of Ireland"] <- "Portugal"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Prague to Dublin then to Belfast"] <- "Czech Republic"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "qutar to dublin"] <- "Qatar"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "R.O.I Ann Braden Mullanbuoy Castlefinn"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Rep of Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "REp of Ireland. Day trip for Ice-cream and playpark"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Rep. Ireland - Dublin"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of iIeland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "republic of ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland - Cork City"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland - Letterkenny, Co. Donegal."] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland Dublin"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland (Dublin)"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland (Dublin). For Work	"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland and Dubai"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland Donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "republic of Ireland Dublin"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland Dublin"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland Monaghan"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "REPUBLIC OF IRELAND TO WORKPLACE"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland Dundalk"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland, Ballyshannon."] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland, England"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland."] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Irelandi"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI- Monaghan"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI-Donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI - between Emyvale Co Monaghan and Aughnacloy Co Tyrone"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI - Dublin"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI -Drogheda for work daily"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI Belgium Holland Germany Poland	"] <- "Belgium, Germany, Holland, Poland, Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI dundalk"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI Galway"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI Ballyshannon Co Donegal	"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI Donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI North Dublin - Baldoyle"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI On holiday in River Valley Caravan Park, Redcross Co. Wicklow."] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI Quigley's Point , Co Donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI, England"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI, ENGLAND"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "RoI, France, Germany, Bulgaria"] <- "Bulgaria, France, Germany, Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI, Germany"] <- "Germany"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI: works in the ROI. Last visited work station on 12-14/04/2021"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI10"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Romania and ROI"] <- "Romania"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Roscommon, County Roscommon	"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Sagres, Portugal"] <- "Portugal"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "SCOTLAND"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Scotland England"] <- "England, Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Scotland & England"] <- "England, Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Scotland (Cairnryan Ferry port) England Blackpool 18/06/21 - 21st/06.21 Cromer (Norfolk) 21/06/21 - 25/06/21 Blackpool 25/06/21 - 30/06/21 30/06/21 travelled directly up to Cairnryan to return home."] <- "England, Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Scotland (Edinburgh) Case is a University student in Edinburgh	"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Scotland and England"] <- "England, Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Scotland Cairnryan to Belfast"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Scotland on the stenaline boat . I remained in my lorry while in Scotland"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Scotland England"] <- "England, Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Scotland England"] <- "England, Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Scotland, England"] <- "England, Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Scotland, England, Wales"] <- "England, Scotland, Wales"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Scotland, Newcastle under lyme, Bournemouth. Carlisle Ferry back from Cairnryan."] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Scotland. Glasgow city	"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Scotland. ROI."] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Shanghai London"] <- "China"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "South Africa Durban Connections to Johannesburg and Doha Final destination London Heathrow"] <- "England, Qatar, South Africa"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "South of ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Southern Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "SOUTHERN IRELAND"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "SPAIN"] <- "Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Spain-Balearic island"] <- "Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Spain - Balearic Isle - Ibiza"] <- "Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Spain Marbella"] <- "Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Spain via ROI"] <- "Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Spain Mallorca"] <- "Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Spain Murcia"] <- "Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Spain ROI"] <- "Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Spent one month in Spain with work Flew home on 06/07/2021"] <- "Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Stayed in my holiday home in Donegal - Republic of Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Stobarts Lutterworth Hunter Boulevard Lutterworth LE17 4XN England"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "STRETFORD MANCHESTER"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Sudan via Istanbul"] <- "Sudan, Turkey"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Tenerife"] <- "Canary Islands"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Traveled from Liverpool to northern Ireland and returned to Liverpool within 24hours"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Traveled from Wales to Northern Ireland"] <- "Wales"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Travelled from Alicante To Bristol airport From Bristol to Belfast"] <- "England, Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "travelled from dublin airport direct to istanbul airport on 25th March - no detail of flight but was at 16.15 or 16.45 hrs. Travelled from Istanbul sabiha gokcen airport on 4th April to london Standsted. Travelled from London stansted to Belfast international on 5th april"] <- "England, Turkey"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Travelled from England (London) to Belfast Northern Ireland"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Travelled from Greece to Dublin, Ireland 14th July"] <- "Greece"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Travelled from London - works in London."] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Travelled from London, England to Belfast, Northern Ireland	"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Travelled from Oslo Norway on Sunday 20th Dec 2020 to Dublin airport via Germany (Frankfurt)"] <- "Germany, Norway, Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Travelled to Majorca on Monday 12/07/2021 and returned on Friday 16/07/2021."] <- "Balearic Islands"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Travelling home from Ibiza"] <- "Balearic Islands"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Uganda-Holland-England"] <- "England, Holland, Uganda"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "UK (LONDON)"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Visit to family home in Donegal for Christmas: address: Gortmacoll Milford Donegal F92 K163"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Visit to London from 10th December to 16th December inclusive"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Warrington Cheshire England"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Warsaw, Poland, via Dublin. Case lives in the Republic of Ireland"] <- "Poland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Was in Qatar. Travelled from Doha to Dublin on 25/03/21. Has been escalated to CL. Aware of flight-all travellers being followed up due to new variant detected on flight."] <- "Qatar"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Waterford - Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Wolverhampton Leeds Selby Doncaster"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Worked in Boots Pharmacy Letterkenny Retail Park, Co. Donegal on 22/12/2020. Currently a locum pharmacist with Clarity Locums."] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Working in Dublin Have used HSE track and trace app"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Working in Industrial estate in Norwich"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Works in ROI. Worked Monday to Friday last week and Monday to Wednesday this week."] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Zimbabwe Zambia Ethopia London Heathrow Liverpool London Liverpool Belfast"] <- "England, Ethopia, Zambia, Zimbabwe"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "150 Scar Lane, Huddersfield HD3 4PY, England"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Amsterdam Singapore Jakata Bali"] <- "Amsterdam, Indonesia, Singapore"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Ashbourne ROI"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Azerbaijan Turkey England"] <- "Azerbaijan, England, Turkey"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Belarus England"] <- "Belarus, England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "belfast-london gatwick on 03/01/2021."] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Benidorm"] <- "Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Bermuda. England (Gatwick )"] <- "Bermuda"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Bridgeend service station Co. Donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Brussels Belgium"] <- "Belgium"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Caffery International LTD Coolfore, Ashbourne Co Meath, Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Canada (Toronto) Mexico (Cancun)"] <- "Canada, Mexico"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Case lives in ROI ( Donegal) and works in Londonderry"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Central England for work purposes travel postcode LE 671ER and NG31 9SP"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Centreparcs Whinfell Old Sawmill Cottages 2 Whinfell, Whinfell CA10 2DW"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Cyprus Athens"] <- "Cyprus, Greece"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Dubai Dublin"] <- "United Arab Emirate"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "dublin ireland bristol england wales"] <- "England, ROI, Wales"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Egypt (Cairo) France (Paris CDG) Holland (Amsterdam Schippol)"] <- "Egypt, France, Holland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England - 4 Hall Farm Close SK7 6PJ"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England Travelled from Gatwick to Dublin	"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England Cornwall St Ives"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England Working Hindhead Tunnell, London"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "France Monte Carlo, Monacco"] <- "France, Monacco"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Galway clinic Galway Clinic Doughiska Co. Galway H91HHT0 Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Galway clinic Galway Clinic Doughiska Co. Galway H91HHT0 Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Gran Canaria"] <- "Grand Canary Island"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "I visited England for work, I got a flight home on Thursday 04 March 2021."] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Ibiza"] <- "Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "India Southern Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Ireland - Dundalk -County Louth"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Isle of Wight Southampton, England"] <- "Isle of Wight"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England / Scotland"] <- "England, Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England for Euro football final on Sunday 11 July"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Flew back from London"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Flew from London City Airport to Belfast City Airport on Friday 16th July (symptoms developed later that evening)."] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Haven caravan resort in Fleetwood and into blackpool also Blackpool pleasure beach on Tuesday"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "I visited my mobile home in Donegal rep of ireland with my household"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland ,Greenore Golf Club,Dundalk,A91 RY10,County Louth."] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == " Scotland"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England- 12/7/2021-to Manchester. Returned on 15/7/2021"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England Travelled from Gatwick to Dublin"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England Cornwall St Ives"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England Working Hindhead Tunnell, London"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "France and England"] <- "England, France"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "I visited England for work, I got a flight home on Thursday 04 March 2021."] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Liverpool, England (12/07/2021 - 15/07/2021)"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Works in ROI. Worked Monday to Friday last week and Monday to Wednesday this week."] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Wales/England"] <- "England, Wales"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Wolverhampton Leeds Selby Doncaster"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "United Kingdom Skegness"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Sweden germany ROI"] <- "Germany, Ireland, Sweden"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Spain , Ibiza"] <- "Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Donegal, Republic of Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Donegal, ROI"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Edinburgh in Scotland"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England - Manchester and Cornwall"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England Scotland"] <- "England, Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England, Denmark, England, Wales, Scotland, N Ireland"] <- "Denmark, England, Scotland, Wales"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Fleetwood haven caravan park Blackpool Stayed in premier Inn in dumfries on Thursday 8th July"] <- "England, Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "I was in Edinburgh."] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Ireland - Donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Lived in Canada and moved back to North of Ireland"] <- "Canada"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Majorca Balearic Islands"] <- "Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of  Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland  Dublin"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland  (Dublin)"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland (Dublin). For Work"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland Dundalk"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI - Donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI  -Drogheda for work daily"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI. Counties Galway and Silgo"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Rossnowlagh, Co. Donegal since Friday 16th July"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Scotland (Edinburgh) Case is a University student in Edinburgh"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "South Wales (Cardiff) 3/7 to 9/7 Cornwall (Padstow) 9/7 to 16/7 North Wales (Llandudno) 16/7 to 18/7"] <- "England, Wales"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "travelled from dublin airport direct to istanbul airport on 25th March - no detail of flight but was at 16.15 or 16.45 hrs. Travelled from Istanbul sabiha gokcen airport on 4th April to london Standsted. Travelled from London stansted to Belfast international on 5th april"] <- "England, Turkey"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Travelled from Alicante To Bristol airport From Bristol to Belfast"] <- "England, Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Travelled from London, England to Belfast, Northern Ireland"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Travelled from Zakynthos to london Gatwick and then london Gatwick to Northern Ireland"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Travelled to England with work."] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Wolverhampton Leeds Selby Doncaster"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Working in Dublin Have used HSE track and trace app"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI North Dublin - Baldoyle"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI On holiday in River Valley Caravan Park, Redcross Co. Wicklow."] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Roscommon, County Roscommon	"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Scotland. Glasgow city"] <- "Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Spain Ibiza "] <- "Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == " England"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == " USA"] <- "USA"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Balearic Islands Majorica"] <- "Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Balearic Islands Majorca"] <- "Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Balearics Ibiza"] <- "Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Bundoran in Ireland."] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Bundoran, co donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Dublin Wales Blackpool"] <- "England, Ireland, Wales"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Emgland"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England - Cornwall"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England   cornwell stayed at self-catering cottage "] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England ( Burgh by Sands just outside Carlisle )"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England (Manchester)"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England Scotland"] <- "England, Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "England. From Friday 9 July to Friday 16 July."] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Flight home from Heathrow - Belfast"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "I visited Porsteward myself 2 weeks ago by car."] <- "NI"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Kilkenny, Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Liverpool 2nd to 16th July"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "London Luton"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "London Brighton"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Mallorca"] <- "Balearic Islands"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Mallorca to Belfast city airport on 21st July"] <- "Balearic Islands"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Menorca"] <- "Balearic Islands"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Mullaghmore Beach, Sligo"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland (Dublin)"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland,"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland, Co Donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of ireland, donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Republic of Ireland, in county Waterford"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "RO1"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Roi"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI (Donegal)"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI Ballyshannon Co Donegal"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI Belgium Holland Germany Poland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Roscommon, County Roscommon"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Rosguill Holiday Park, Melmore Road, Gortnalughoge, Letterkenny, Co. Donegal, F92 W965, Ireland"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Scotland, england"] <- "England, Scotland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "South of Ireland for a staycation"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Spain  magluf jet 2 hoildays hotel and transfer to hotel package"] <- "Balearic Islands"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Spain Ibiza"] <- "Balearic Islands, Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Spain islands"] <- "Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "travelled from dublin airport direct to istanbul airport on 25th March - no detail of flight but was at 16.15 or 16.45 hrs. Travelled from Istanbul sabiha gokcen airport on  4th April to london Standsted. Travelled from London stansted to Belfast international on 5th april"] <- "England, Turkey"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Travelled to Majorca departed Monday July 12th and returned Friday July 16th"] <- "Balearic Islands"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Travelled to Waterford, Dungarvan on Saturday 17th July. Stayed in rented accommodation, no one else present other than the family."] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "United Kingdom"] <- "UK"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Visited Flushing and Falmouth Cornwall"] <- "England"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Wales. Haven - Hafan Y Mor holiday Park. 9th to 19th July 2021 Visited the entertainment settings including: The Cove, The Boardwalk, Swimming Pool, pottery class, outdoor activities."] <- "Wales"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Working in Dublin Have used HSE track and trace app"] <- "Ireland"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "Majorca"] <- "Spain"
  travellers$CountriesVisited.x[travellers$CountriesVisited.x == "ROI"] <- "Ireland"
  travellers$CountriesVisited.x <- gsub("ROI", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Balearic Islands", "Spain", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("USA", "United States", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("GB", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("UK", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Dubai", "United Arab Emirates", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("dubai", "United Arab Emirates", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("ROI", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("RoI", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Roi", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("RIO", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("RO1", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("roi", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("R.O.I.", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("R.O.I", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Republic of Ireland", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("republic of ireland", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("REPUBLIC OF IRELAND", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Dublin", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("County Donegal Southern Ireland", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Ireland - Donegal", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Co Louth, 15mins away to visit my parents and family on sunday 27th", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Car journey to Dundalk county Louth Ireland for two hours shopping", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Ireland - Letterkenny, Co. Donegal.", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Visit to family home in Donegal for Christmas: address: Gortmacoll Milford Donegal F92 K163", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Flight from India to LHR\nLHR to BHD", "India", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("India, England, N. Ireland", "India", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("India, LHR, N. Ireland", "India", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Ibiza", "Spain", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("United Kingdom Skegness", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Uganda-Holland-England", "Uganda", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Teneriffe", "Spain", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("New Delhi to London Heathrow then connecting flight to Belfast after quarantine.", "India", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Galway clinic Galway Clinic Doughiska Co. Galway H91HHT0 Ireland", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("travelled from dublin airport direct to istanbul airport on 25th March - no detail of flight but was at 16.15 or 16.45 hrs. Travelled from Istanbul sabiha gokcen airport on  4th April to london Standsted. Travelled from London stansted to Belfast international on 5th april", "Turkey", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Mallorca - Balearic Islands", "Spain", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("England  - Liverpool", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Liverpool", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("London  - England", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("London, England", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("London", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("ENGLAND", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("england", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Portsmouth, England", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Manchester, England", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Birmingham UK", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("England-London. Did not leave Gatwick airport as stayed in airport hotel-the Hilton", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Netherlands and England", "Netherlands", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("150 Scar Lane, Huddersfield HD3 4PY, England", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Altan Loch Hotel Donegal", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("America and london", "United States", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Ardmore\nCounty Waterford", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Ashbourne Ireland", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("SPAIN", "Spain", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Balearic Island  Majorca.", "Spain", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Balearic Islands  Majorca.", "Spain", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Balearic Islands Majorca", "Spain", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Balearic Islands Majorica", "Spain", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Balearic Mallorca!", "Spain", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Balearics Spain", "Spain", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Bath England", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Belaric islands Santa ponsa", "Spain", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Belarus England", "Belarus", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("belfast-london gatwick on 03/01/2021.", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Belfast city airport to Edinburgh \n& return", "Scotland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("belfast city airport to Exeter", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Belfast City Airport to Exeter , England\n\n& return", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Belfast to Glascow and return", "Scotland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Benidorm", "Spain", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Berlin, Amsterdam and Dublin-all connecting flights commencing 28/5/21. Employed via German Government and currently residing in Bangor. Self isolating in Bangor", "Germany", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Bermuda and Gatwick England", "Bermuda", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Bermuda. England (Gatwick )", "Bermuda", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Birmingham United Kingdom", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Blackpool", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Blackpool \nflew via manchester", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Blackpool england", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Bosnia", "Bosnia and Herzegovina", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("BRAY CO wicklow", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Brazil\r\nPortugal\r\nIreland", "Brazil", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Bridge End,Co Donegal", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Bridgeend service station Co. Donegal", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Brussels Belgium", "Belgium", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Buncrana", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Bermuda. England (Gatwick )", "Bermuda", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Bundoran in Ireland.", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Bundoran\nIreland", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Bundoran, co donegal", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Bundoran. Co. Donegal Ireland", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Caffery International LTD Coolfore, Ashbourne Co Meath, Ireland", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Canada and Ireland", "Canada", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("case flew in from England on Friday 25th June", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("case has travelled form Hydrabad in India and has travelled through bangalore to London . He then travelled onto Belfast City Airport", "India", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Case lives in Ireland ( Donegal) and works in Londonderry", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Caseys Caravan and Camping Park,Donegal", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Central England for work purposes travel postcode LE 671ER and NG31 9SP", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Centreparcs Whinfell Old Sawmill Cottages 2 Whinfell, Whinfell CA10 2DW", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Claas uk by ferry Belfast to England then home holly head to Dublin.o", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Co Cork Ireland", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Co Cork, Ireland", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Co Donegal\n1.. moville - flat no address at Rigney Birthday party\n\n2 ... 4 Cnocglass Fanad family holiday house", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Co Kildare, Ireland", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Co. Leitrim Ireland", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Co.Donegal R.Ireland in Fahan", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Cork, Ireland", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("cornwall England", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("CORNWALL ENGLAND", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("County Donegal", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("County Donegall", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("County Kerry\nIreland", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("County Louth", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Creeslough\n Donegal\nIreland", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Crete", "Greece", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Crete,", "Greece", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Cyprus & England", "Cyprus", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Cyprus Athens", "Cyprus", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Day trip to Leeds to visit son for first time this year.I believe I caught COVID from him as his household has tested positive and he’s awaiting results", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Derry", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("England, Scotland", "Scotland", travellers$CountriesVisited.x)
  #### END ####
  
  country.count <- as.data.frame(table(travellers$CountriesVisited.x))
  country.count <- arrange(country.count, desc(Freq))
  topten <- head(country.count, 10)
  colnames(topten) <- c("country", "count")
  topten <- left_join(topten, all.countries.status, by = "country")
  colnames(topten) <- c("Country", "Count", "Status")
  
  
  #travellers$CountriesVisited.x <- standardizeCountry(travellers$CountriesVisited.x, fuzzyDist = 10)
  #travellers$CountriesVisited.x <- tolower(travellers$CountriesVisited.x)
  #travellers$CountriesVisited.x <- simpleCap(travellers$CountriesVisited.x, onlyFirst = F, rev = F)
  #travellers$CountriesVisited.x <- gsub("Republic of the Congo", "Democratic Republic of Congo", travellers$CountriesVisited.x, fixed = T)
  #this still needs tweaking (str_contains)

  ## Link data to country status 
  travellers.status <- left_join(travellers, pivot.countries, by = c("Epiweek" = "Epiweek", "CountriesVisited.x" = "country"))
  
  ####### HOME #######
  
  # InfoBoxes
  output$total_cases <- renderInfoBox({
    infoBox(
      "Total Reported Cases from International Travellers", 
      paste0(nrow(filter(travellers.status))), 
      icon = icon("globe-europe"), 
      color ="light-blue")
  })
  
  output$cases_this_week <- renderInfoBox({
    infoBox(
      "Reported Cases This Week from International Travellers", 
      paste0(nrow(filter(travellers.status,
                         Epiweek == current.epiweek))), 
      icon = icon("globe-europe"), 
      color = "light-blue")
  })
  
  output$total_green_cases <- renderInfoBox({
    infoBox(
      "Total Reported Cases from Green Countries", 
      paste0(nrow(filter(travellers.status,
                         Status == "Green"))), 
      icon = icon("chart-bar"), 
      color ="green")
  })
  
  output$green_cases_this_week <- renderInfoBox({
    infoBox(
      "Cases This Week from Green Countries", 
      paste0(nrow(filter(travellers.status,
                         Status == "Green",
                         Epiweek == current.epiweek))), 
      icon = icon("plane-arrival"), 
      color = "green")
  })
  
  output$total_amber_cases <- renderInfoBox({
    infoBox(
      "Total Reported Cases from Amber Countries", 
      paste0(nrow(filter(travellers.status,
                         Status == "Amber"))), 
      icon = icon("chart-bar"), 
      color ="yellow")
  })
  
  output$amber_cases_this_week <- renderInfoBox({
    infoBox(
      "Cases This Week from Amber Countries", 
      paste0(nrow(filter(travellers.status,
                         Status == "Amber",
                         Epiweek == current.epiweek))), 
      icon = icon("plane-arrival"), 
      color = "yellow")
  })
  
  output$total_red_cases <- renderInfoBox({
    infoBox(
      "Total Reported Cases from Red Countries", 
      paste0(nrow(filter(travellers.status,
                         Status == "Red"))), 
      icon = icon("chart-bar"), 
      color ="red")
  })
  
  output$red_cases_this_week <- renderInfoBox({
    infoBox(
      "Cases This Week from Red Countries", 
      paste0(nrow(filter(travellers.status,
                         Status == "Red",
                         Epiweek == current.epiweek))), 
      icon = icon("plane-arrival"), 
      color = "red")
  })
  
  # Table
  output$top_ten = DT::renderDataTable({
    DT::datatable(topten)
  })
  
  ####### TABLES  ####### 
  
  output$all_countries = DT::renderDataTable({
    DT::datatable(all.countries.status, options = list(pageLength = 50))
  })
  
}