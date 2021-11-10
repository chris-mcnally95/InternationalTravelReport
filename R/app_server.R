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
  
  selected_dates1 <- shiny::reactive ({
    input$week_selection1
  })
  
  selected_dates2 <- shiny::reactive ({
    input$week_selection2
  })
  
  mod_country_cases_chart_server("country_cases_chart_ui_1", 
                                 date_range1 = selected_dates1,
                                 date_range2 = selected_dates2)
  
  #--------------REPORTS--------------
  
  mod_reports_server("reports_ui_1")
  
}
