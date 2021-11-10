#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # Load in Dynamic Data Sets
  
  source("./data_prep.R")
  
  # Your application server logic 
  
  #--------------HOME--------------
  
  mod_home_server("home_ui_1")
  
  #--------------COUNTRY FREQUENCY--------------
  
  mod_country_frequency_server("country_frequency_ui_1")
  
  #--------------COUNTRY CASES CHART--------------
  
  mod_country_cases_chart_server("country_cases_chart_ui_1")
  
  #--------------REPORTS--------------
  
  mod_reports_server("reports_ui_1")
  
}
