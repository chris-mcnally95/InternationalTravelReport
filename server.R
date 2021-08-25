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
    filter(CreatedOn >= "2021-01-01") %>% 
    mutate(Epiweek = paste0("Epiweek", strftime(WhenDidYouReturnToNorthernIreland, format = "%V"))) 

  ## Tidy old data
  travellers$CountriesVisited.x <- gsub("ROI", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("RoI", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("roi", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("R.O.I.", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("R.O.I", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Republic of Ireland", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("REPUBLIC OF IRELAND", "Ireland", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("County Donegal Southern Ireland", "Ireland", travellers$CountriesVisited.x)
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
  travellers$CountriesVisited.x <- gsub("Portsmouth, England", "England", travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- gsub("Manchester, England", "England", travellers$CountriesVisited.x)
  
  travellers$CountriesVisited.x <- standardizeCountry(travellers$CountriesVisited.x, fuzzyDist = 10)
  travellers$CountriesVisited.x <- tolower(travellers$CountriesVisited.x)
  travellers$CountriesVisited.x <- simpleCap(travellers$CountriesVisited.x, onlyFirst = F, rev = F)
  travellers$CountriesVisited.x <- gsub("Republic of the Congo", "Democratic Republic of Congo", travellers$CountriesVisited.x, fixed = T)
  #this still needs tweaking (str_contains)#

  ## Link data to country status 
  travellers.status <- left_join(travellers, pivot.countries, by = c("Epiweek" = "Epiweek", "CountriesVisited.x" = "country"))
  
  
  # Make Graphs
  
    
  ####### HOME #######
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
}