library(DBI)
library(dplyr)
library(dbplyr)


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


# Standard SQL Language
# getTableFiltered <- function(table) {
#   query <- paste("SELECT * FROM", table, "WHERE ('CreatedOn' >= '20210910')")
#   data <- dbGetQuery(con, query)
#   message(paste0("Data retrieved from ", table))
#   return(data)
# }

# Using dplyr
# q1 <- tbl(con, "Cases") %>%
#   filter(CreatedOn >= "20210910")
# show_query(q1)
# q1 <- as.data.frame(q1)

# Using dplyr function
getTableFiltered <- function(table, date) {
  query <- tbl(con, table) %>%
    filter(CreatedOn >= date)
  show_query(query)
  data <- as.data.frame(query)
  message(paste0("Data retrieved from ", table, ". Filtered from ", date))
  return(data)
}

