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



