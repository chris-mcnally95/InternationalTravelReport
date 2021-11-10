#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id){
  
  shinydashboard::tabItem(
    tabName = "home",
    
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title ="Welcome to the International Travel Dashboard.",
        p("This dashboard has been built to aid the CTC Data Management Team / Surveillance Team to montior COVID-19 cases related to international travel in 2021"),
        p(strong("Please Note:")," The free text inputs of country entries are currently being addressed, therefore, some cases will be omitted temporarily."),
        p(strong("Additional Note:")," As of 04/10/21 the Red, Amber, Green status of countries has been depractated and replaced with Red and Non-Red. This dashboard has 
              been updated to reflect that."),
        p(strong("Additional Note:")," As of 04/11/21, zero countries are listed on the Red List for Northern Ireland.")
      )
    ),
    
    shiny::fluidRow(
      shinycssloaders::withSpinner(shinydashboard::infoBoxOutput(shiny::NS(id, "total_cases"), width = 6)),
      shinydashboard::infoBoxOutput(shiny::NS(id, "cases_this_week"), width = 6)
    ),
    
    shiny::fluidRow(
      shinydashboard::infoBoxOutput(shiny::NS(id, "total_trav_cases"), width = 6),
      shinydashboard::infoBoxOutput(shiny::NS(id, "trav_cases_this_week"), width = 6)
    ),
    
    shiny::fluidRow(
      shinydashboard::infoBoxOutput(shiny::NS(id, "total_percent"), width = 6),
      shinydashboard::infoBoxOutput(shiny::NS(id, "percent_this_week"), width = 6)
    )
  )
}
    
#' home Server Functions
#'
#' @noRd 
mod_home_server <- function(id){
  moduleServer( id, function(input, output, session){
    
    ## Total Cases
    output$total_cases <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Total Reported Cases from Population",
        paste0(nrow(Allcases)), 
        subtitle = paste("Successful Calls: ", nrow(dplyr::filter(Allcases,
                                                           CaseFileStatus != "Cancelled"))),
        icon = icon("calculator"), 
        color ="navy")
    })
    
    ## Total Traveller Cases
    output$total_trav_cases <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Total Reported Cases from International Travellers", 
        paste0(nrow(travellers)), 
        icon = icon("globe-europe"), 
        color ="blue")
    })
    
    ## Total Cases this Week
    output$cases_this_week <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Reported Cases This Week from Population", 
        paste0(nrow(dplyr::filter(Allcases,
                           EpiweekCreated == current.epiweek))), 
        subtitle = paste("Successful Calls: ", nrow(dplyr::filter(Allcases,
                                                           EpiweekCreated == current.epiweek,
                                                           CaseFileStatus != "Cancelled"))),
        icon = icon("calculator"), 
        color = "navy")
    })
    
    ## Total Traveller Cases this Week
    output$trav_cases_this_week <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Reported Cases This Week from International Travellers", 
        paste0(nrow(dplyr::filter(travellers,
                           EpiweekCreated == current.epiweek))), 
        icon = icon("globe-europe"), 
        color = "blue")
    })
    
    ## Total Percent
    output$total_percent <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Percentage of Total Cases that are International Travellers", 
        paste0(round((nrow(travellers)/nrow(Allcases))*100, 2), "%"), 
        subtitle = paste("From Successful Calls:",
                         round((nrow(travellers)/
                                  nrow(dplyr::filter(Allcases,
                                                     CaseFileStatus != "Cancelled")))*100, 2),
                         "%"),
        icon = icon("percent"), 
        color ="light-blue")
    })
    
    ## Total Percent this Week
    
    output$percent_this_week <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Percentage of Cases This Week that are International Travellers", 
        paste0(round((nrow(dplyr::filter(travellers,
                                  EpiweekCreated == current.epiweek))/
                        nrow(dplyr::filter(Allcases,
                                          EpiweekCreated == current.epiweek)))*100, 2),
               "%"), 
        subtitle = paste("From Successful Calls:",
                         round((nrow(dplyr::filter(travellers,
                                            EpiweekCreated == current.epiweek))/
                                  nrow(dplyr::filter(Allcases,
                                              EpiweekCreated == current.epiweek,
                                              CaseFileStatus != "Cancelled")))*100, 2),
                         "%"),
        icon = icon("percent"), 
        color = "light-blue")
    })
  })
}
    
## To be copied in the UI
# mod_home_ui("home_ui_1")
    
## To be copied in the server
# mod_home_server("home_ui_1")
