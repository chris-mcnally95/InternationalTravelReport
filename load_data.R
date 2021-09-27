library(shinydashboard)
library(rangeBuilder)
library(tidyverse)
library(plyr)
library(lubridate)
library(rvest)
library(rworldmap)
library(clinUtils)
library(DT)
library(ggplot2)
library(plotly)
library(DBI)


synapse_server <- "swhscphipprduks01.sql.azuresynapse.net"
synapse_database <- "exploratorydb"
connection_driver <- "{ODBC Driver 17 for SQL Server}"

con <- dbConnect(odbc::odbc(),
                 driver = connection_driver,
                 database = synapse_database,
                 Authentication="ActiveDirectoryMSI",
                 server = synapse_server)

sqlQuery <- "SELECT TOP (5) id as 'Case ID' from Cases"
pingdb <- function() {
  initdata <- dbGetQuery(con, sqlQuery)
  return(initdata)
}

getTable <- function(table) {
  query <- paste("SELECT * FROM", table)
  data <- dbGetQuery(con, query)
  message(paste0("Data retrieved from ", table))
  return(data)
}

# Load data
locations <- getTable("Locations")
collectclosecontacts <- getTable("CollectContactsCalls")
cases <- getTable("cases") #%>%
#dplyr::select(DateOfOnset, DateOfSample, CaseNumber, Gender, AgeAtPositiveResult,
#CreatedOn)

# Issue Connection Stop --- This should be moved to the bottom of global.R
shiny::onStop(function(){dbDisconnect(con)})