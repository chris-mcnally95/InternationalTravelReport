#' country_frequency UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_country_frequency_ui <- function(id){
  
  shinydashboard::tabItem(
    tabName = "country_frequencies",
    
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title ="Countries by Frequency in 2021",
        p("This table displays the top countries associated with the most COVID-19 cases in 2021"),
        p(strong("Please Note:")," The free text inputs of country entries (as well as cases with multiple countries visited) are currently being addressed, therefore, some cases will be omitted and/or be erroneous temporarily"),
        hr(),
        shinycssloaders::withSpinner(DT::dataTableOutput(shiny::NS(id, "top_ten")))
      )
    )
  )
}
    
#' country_frequency Server Functions
#'
#' @noRd 
mod_country_frequency_server <- function(id){
  moduleServer( id, function(input, output, session){
    output$top_ten = DT::renderDataTable({
      DT::datatable(topten, options = list(pageLength = 10))
    })
  })
}
    
## To be copied in the UI
# mod_country_frequency_ui("country_frequency_ui_1")
    
## To be copied in the server
# mod_country_frequency_server("country_frequency_ui_1")
