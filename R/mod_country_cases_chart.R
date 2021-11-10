#' country_cases_chart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_country_cases_chart_ui <- function(id){
  
  shinydashboard::tabItem(
    tabName = "cases_by_country",
    
    shiny::fluidRow(
      
      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Countries and Cases by Epiweek",
        p("The graphs below shows the count of cases that have traveled outside of Northern Ireland, including the countries, in 2021 per epiweek."),
      ),
      
      shinydashboard::tabBox(
        width = 12,
        
        shiny::tabPanel(
          width = 12,
          title = "Countries and Cases by Epiweek",
          p("This graph shows the count of cases from travellers outside NI per epiweek, broken down by countries travelled" ),
          hr(),
          shiny::sliderInput(
            inputId = "week_selection1",
            label = "Please select desired Epiweek range",
            value = c(0,as.numeric(strftime(Sys.Date(), format = "%V"))),
            min = 0, max = as.numeric(strftime(Sys.Date(), format = "%V")),
            width = "400px"), #Added slider input fo x axis determination on chart
          shinycssloaders::withSpinner(plotly::plotlyOutput(shiny::NS(id,"all.case.by.week.chart"), height = NULL))
        ),
        
        shiny::tabPanel(
          width = 12,
          title = "Percentage of Countries and Cases by Epiweek",
          p("This graph shows the scale of cases from travellers outside NI per epiweek as a fraction of countries travelled" ),
          hr(),
          shiny::sliderInput(
            inputId = "week_selection2",
            label = "Please select desired Epiweek range",
            value = c(0,as.numeric(strftime(Sys.Date(), format = "%V"))),
            min = 0, max = as.numeric(strftime(Sys.Date(), format = "%V")),
            width = "400px"), #Added slider input fo x axis determination on chart
          shinycssloaders::withSpinner(plotly::plotlyOutput(shiny::NS(id, "all.case.by.week.chart.per"), height = NULL))
        )
      )
    )
  )
}
    
#' country_cases_chart Server Functions
#'
#' @noRd 
mod_country_cases_chart_server <- function(id, date_range1, date_range2){
  moduleServer( id, function(input, output, session){
    
    country.plot <- travellers %>% 
      dplyr::select(CountriesVisited, EpiweekReturned) %>% 
      tidyr::drop_na(CountriesVisited)
    
    country.plot$country <- country.plot$CountriesVisited
    country.plot$country[country.plot$country != as.character(topten[1,1]) &
                           country.plot$country != as.character(topten[2,1]) &
                           country.plot$country != as.character(topten[3,1]) &
                           country.plot$country != as.character(topten[4,1]) & 
                           country.plot$country != as.character(topten[5,1]) &
                           country.plot$country != as.character(topten[6,1]) &
                           country.plot$country != as.character(topten[7,1]) &
                           country.plot$country != as.character(topten[8,1]) &
                           country.plot$country != as.character(topten[9,1]) &
                           country.plot$country != as.character(topten[10,1])] <- "Other" #Top Ten 
    
    country.plot$country <- factor(country.plot$country, levels=c(as.character(topten[1,1]),
                                                                  as.character(topten[2,1]),
                                                                  as.character(topten[3,1]),
                                                                  as.character(topten[4,1]),
                                                                  as.character(topten[5,1]),
                                                                  as.character(topten[6,1]),
                                                                  as.character(topten[7,1]),
                                                                  as.character(topten[8,1]),
                                                                  as.character(topten[9,1]),
                                                                  as.character(topten[10,1]),
                                                                  "Other"))
    
    country.plot$EpiweekReturned <- gsub("Epiweek"," ", as.character(country.plot$EpiweekReturned))
    country.plot$EpiweekReturned <- as.numeric(country.plot$EpiweekReturned)
    
    # All Countries by Epiweek
    output$all.case.by.week.chart <- plotly::renderPlotly({
      
      shiny::req(date_range1())
      
      all.countries.by.week <- ggplot2::ggplot(country.plot, ggplot2::aes(EpiweekReturned)) +
        ggplot2::geom_bar(ggplot2::aes(fill=country),
                 position = ggplot2::position_stack(reverse = TRUE)) +
        ggplot2::scale_fill_manual(values = c("seagreen",
                                              "red3",
                                              "royalblue",
                                              "gold",
                                              "maroon", 
                                              "springgreen4",
                                              "red",
                                              "olivedrab",
                                              "skyblue",
                                              "navy",
                                              "wheat4"),
                                   name = "Country") +
        ggplot2::scale_x_continuous(limits =  c(as.numeric(date_range1()[1]),
                                                as.numeric(date_range1()[2])),
                           breaks = c(1:as.numeric(strftime(Sys.Date(), format = "%V")))) + #Adjusting the size of graph in the tab 
        ggplot2::theme_bw()
                                     
      
      all.countries.by.week <- plotly::ggplotly(all.countries.by.week) %>% 
        plotly::layout(xaxis = list(tickangle = 45))
      
      all.countries.by.week
    })
    
    # All Countries Percentage by Epiweeek
    output$all.case.by.week.chart.per <- plotly::renderPlotly({
      
      shiny::req(date_range2())
  
      all.countries.by.week.per <- ggplot2::ggplot(country.plot, ggplot2::aes(EpiweekReturned)) +
        ggplot2::geom_bar(ggplot2::aes(fill=country),
                 position = ggplot2::position_fill(reverse = TRUE)) +
        ggplot2::scale_fill_manual(values = c("seagreen",
                                              "red3",
                                              "royalblue",
                                              "gold",
                                              "maroon", 
                                              "springgreen4",
                                              "red",
                                              "olivedrab",
                                              "skyblue",
                                              "navy",
                                              "wheat4"),
                                   name = "Country") +
        ggplot2::scale_x_continuous(limits =  c(as.numeric(date_range2()[1]),
                                                as.numeric(date_range2()[2])),
                           breaks = c(1:as.numeric(strftime(Sys.Date(), format = "%V")))) + #Adjusting the size of graph in the tab 
        ggplot2::theme_bw()
      
      all.countries.by.week.per <- plotly::ggplotly(all.countries.by.week.per) %>% 
        plotly::layout(xaxis = list(tickangle = 45))
      
      all.countries.by.week.per
    })
  })
}
    
## To be copied in the UI
# mod_country_cases_chart_ui("country_cases_chart_ui_1")
    
## To be copied in the server
# mod_country_cases_chart_server("country_cases_chart_ui_1")
