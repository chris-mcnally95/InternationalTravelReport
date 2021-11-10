#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  # Your application UI logic 
  shinydashboard::dashboardPage(
    
    shinydashboard::dashboardHeader(title = "International Travel Dashboard"),
    
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard:: menuItem("Home", tabName = "home", icon = icon("home")),
        shinydashboard::menuItem("Country Frequencies", tabName = "country_frequencies", icon = icon("globe")),
        shinydashboard::menuItem("Cases by Country", tabName = "cases_by_country", icon = icon("globe-europe")),
        shinydashboard::menuItem("Reports", tabName = "reports", icon = icon("list"))
      )
    ),
    
    shinydashboard::dashboardBody(
      
      shinydashboard::tabItems(
        
        #--------------HOME--------------
        
        mod_home_ui("home_ui_1"),
        
        #--------------COUNTRY FREQUENCY--------------
        
        mod_country_frequency_ui("country_frequency_ui_1"),
        
        #--------------COUNTRY CASES CHART--------------
        
        mod_country_cases_chart_ui("country_cases_chart_ui_1"),
        
        #--------------REPORTS--------------
        
        mod_reports_ui("reports_ui_1")
        
      )
    )
  )
}
