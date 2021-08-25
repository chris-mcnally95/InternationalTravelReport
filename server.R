####### SETUP  ####### 

# Libraries
library(tidyverse)
library(plyr)
library(lubridate)
library(rvest)
library(rworldmap)

# Source scripts
source("./azure_functions.R")

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
  red.countries <- gsub("\\s*\\([^\\)]+\\)","", as.character(red.countries$country)) #Remove brackets and everything in them
  red.countries <- str_remove(red.countries, "[\n]")
  red.countries <- as.data.frame(red.countries)
  colnames(red.countries) <- "country"
  red.countries$status <- "Red"
  
  # Defining amber countries
  CTA <- as.data.frame(c("United Kingdom", "Isle of Man", "Ireland", "Guernsey", "Jersey")) #Define Common Travel Area
  CTA$status <- "Green"
  colnames(CTA) <- c("country", "status")
  
  ## Define all countries in world
  amber.countries <- countryExData[ , 2] #rworldmap package which lists all countries 
  amber.countries <- gsub("Dem. Rep. Congo","Democratic Republic of Congo", amber.countries, fixed = T) 
  amber.countries <- gsub("Rep.","Republic", amber.countries, fixed = T) 
  amber.countries <- gsub("Viet Nam","Vietnam", amber.countries, fixed = T) 
  amber.countries <- as.data.frame(amber.countries)
  colnames(amber.countries) <- "country"
  
  ## Amber countries are countries not on red or green lists or CTA
  non.amber.countries <- rbind(red.countries, green.countries, CTA)
  
  ## Remove countries that appear in green and red lists and CTA
  amber.countries <- anti_join(amber.countries, non.amber.countries)
  amber.countries$status <- "Amber"
  
  
  
  # Country Status data frame
  
  all.countries <- rbind(amber.countries, non.amber.countries)
  all.countries <- all.countries[order(all.countries$country), ] 
  present.country.status <- all.countries$status
  
  
  country.status <- data.frame(matrix(NA, nrow = 200, ncol = 52))
  row.names(country.status) <- all.countries$country
  
  
  epiweek <- (paste0("Epiweek", 1:52))
  colnames(country.status) <- epiweek
  
  current.epiweek <- paste0("Epiweek", strftime(Sys.Date(), format = "%V"))
  status.assignment <- country.status %>% 
    select(current.epiweek) 
  status.assignment[,1] <- present.country.status
  country.status <- replace(country.status, current.epiweek, status.assignment[,1])
  country.status$Epiweek33 <- country.status$Epiweek34
  country.status$country <- all.countries$country
  
  
  write.csv(country.status, "country.status.backup.csv")
  
  pivot.countries <- pivot_longer(country.status, !country, names_to = "Epiweek", values_to = "Status")
  
  # Load data
  locations <- getTable("Locations")
  collectclosecontacts <- getTable("CollectContactsCalls")
  cases <- getTable("cases")
  
  
  # Define travellers
  
  ## Make data frame
  travellers <- locations %>%
    filter(TypeOfPlace == "Travel outside Northern Ireland") %>%
    left_join(collectclosecontacts, by = c("CollectCallId" = "Id")) %>%
    left_join(cases, by = "CaseNumber") %>%
    select(CountriesVisited.x, WhenDidYouLeaveNorthernIreland, WhenDidYouReturnToNorthernIreland, CaseNumber, FirstName, LastName, Gender.x, CreatedOn) %>% 
    mutate(Epiweek = paste0("Epiweek", strftime(WhenDidYouReturnToNorthernIreland, format = "%V")))
  
  ## Tidy old data
  travellers$CountriesVisited.x <- gsub("ROI", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("RoI", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("roi", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("R.O.I.", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Republic of Ireland", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("REPUBLIC OF IRELAND", "Ireland", travellers$CountriesVisited.x)
  
  time.travellers.check <- travellers %>% 
    filter(WhenDidYouReturnToNorthernIreland >= Sys.Date())


  ## Link data to country status 
  travellers <- left_join(travellers, pivot.countries, by = c("Epiweek" = "Epiweek", "CountriesVisited.x" = "country"))
  
  startdate <- as.Date("2021-08-16")
    
    ####### HOME ####### 
}