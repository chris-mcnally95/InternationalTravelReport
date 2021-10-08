######## SETUP ######## 

# Libraries
library(shinydashboard)
library(shinycssloaders)
library(ggplot2)
library(plotly)

######## SPINNER ########
options(spinner.color = "#0275D8", spinner.color.background="#ffffff", spinner.size =2)

######## UI ######## 

ui <- dashboardPage(
  
  dashboardHeader(title = "International Travel Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Country Frequencies", tabName = "country_frequencies", icon = icon("globe")),
      menuItem("Cases by Status", tabName = "cases_by_rag", icon = icon("chart-bar")),
      menuItem("Cases by Country", tabName = "cases_by_country", icon = icon("globe-europe")),
      menuItem("Reports", tabName = "reports", icon = icon("list")),
      menuItem("Country Status", tabName = "tables", icon = icon("table")),
      menuItem("Methodology", tabName = "methodology", icon = icon("clipboard-list"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      #--------------HOME--------------
      
      tabItem(
        tabName = "home",
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title ="Welcome to the International Travel Dashboard.",
            p("This dashboard has been built to aid the CTC Data Management Team / Surveillance Team to montior COVID-19 cases related to international travel in 2021"),
            p(strong("Please Note:")," The free text inputs of country entries are currently being addressed, therefore, some cases will be omitted temporarily."),
            p(strong("Please Note:")," As of 04/10/21 the Red, Amber, Green status of countries has been depractated and replaced with Red and Non-Red. This dashboard has 
              been updated to reflce that.")
          )
        ),
        
        fluidRow(
          withSpinner(infoBoxOutput("total_cases", width = 6), type = 2, color.background = "#ecf0f5"),
          infoBoxOutput("cases_this_week", width = 6)
        ),
        
        fluidRow(
          infoBoxOutput("total_percent", width = 6),
          infoBoxOutput("percent_this_week", width = 6)
        ),
        
        fluidRow(
          infoBoxOutput("total_non_red_cases", width = 6),
          infoBoxOutput("non_red_cases_this_week", width = 6)
        ),
        
        # fluidRow(
        #   infoBoxOutput("total_amber_cases", width = 6),
        #   infoBoxOutput("amber_cases_this_week", width = 6)
        # ),
        
        fluidRow(
          infoBoxOutput("total_red_cases", width = 6),
          infoBoxOutput("red_cases_this_week", width = 6)
        )
      ),
      
      #--------------COUNTRY FREQUENCIES--------------
      tabItem(
        tabName = "country_frequencies",
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title ="Countries by Frequency in 2021",
            p("This table displays the top countries associated with the most COVID-19 cases in 2021"),
            p(strong("Please Note:")," The free text inputs of country entries (as well as cases with multiple countries visited) are currently being addressed, therefore, some cases will be omitted and/or be erroneous temporarily"),
            hr(),
            shinycssloaders::withSpinner(DT::dataTableOutput("top_ten"), type = 2)
          )
        )
      ),
      
      #--------------CASES BY RAG--------------
      tabItem(
        tabName = "cases_by_rag",
        fluidRow(
          
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Cases by Epiweek by RAG Status",
            p("The graphs below shows the count of cases that have traveled outside of Northern Ireland, including the countries, in 2021 per epiweek."),
          ),
          
          tabBox(
            width = 12,
            
            tabPanel(
              width = 12,
              title = "Cases by Epiweek",
              p("This graph shows the count of cases from travellers outside NI per epiweek, by RAG Status of those countries" ),
              hr(),
              sliderInput(
                inputId = "week",
                label = "Please select desired Epiweek range",
                value = c(0,as.numeric(strftime(Sys.Date(), format = "%V"))),
                min = 0, max = as.numeric(strftime(Sys.Date(), format = "%V")),
                width = "400px"), #Added slider input fo x axis determination on chart
              shinycssloaders::withSpinner(plotlyOutput("case.by.week.chart", height = NULL))
            ),
            
            tabPanel(
              width = 12,
              title = "Cases by Month",
              p("This graph shows the count of cases from travellers outside NI per month, by RAG Status of those countries" ),
              hr(),
              shinycssloaders::withSpinner(plotlyOutput("case.by.month.chart", height = NULL))
            )
          )
        )
      ),
      
      # #--------------CASES BY COUNTRY--------------
      tabItem(
        tabName = "cases_by_country",
        fluidRow(
          
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Countries and Cases by Epiweek",
            p("The graphs below shows the count of cases that have traveled outside of Northern Ireland, including the countries, in 2021 per epiweek."),
          ),
          
          tabBox(
            width = 12,
            
            tabPanel(
              width = 12,
              title = "Countries and Cases by Epiweek",
              p("This graph shows the count of cases from travellers outside NI per epiweek, broken down by countries travelled" ),
              hr(),
              sliderInput(
                inputId = "week2",
                label = "Please select desired Epiweek range",
                value = c(0,as.numeric(strftime(Sys.Date(), format = "%V"))),
                min = 0, max = as.numeric(strftime(Sys.Date(), format = "%V")),
                width = "400px"), #Added slider input fo x axis determination on chart
              shinycssloaders::withSpinner(plotlyOutput("all.rag.case.by.week.chart", height = NULL))
            ),
            
            tabPanel(
              width = 12,
              title = "Percentage of Countries and Cases by Epiweek",
              p("This graph shows the scale of cases from travellers outside NI per epiweek as a fraction of countries travelled" ),
              hr(),
              sliderInput(
                inputId = "week2",
                label = "Please select desired Epiweek range",
                value = c(0,as.numeric(strftime(Sys.Date(), format = "%V"))),
                min = 0, max = as.numeric(strftime(Sys.Date(), format = "%V")),
                width = "400px"), #Added slider input fo x axis determination on chart
              shinycssloaders::withSpinner(plotlyOutput("all.rag.case.by.week.chart.per", height = NULL))
            )
            # 
            #    tabPanel(
            #      title = "Green Countries and Cases by Epiweek",
            #      p("This graph shows the count of cases from travellers outside NI per epiweek, broken down by green countries travelled" ),
            #      p(strong("Please Note:"), "NA countries omitted from this dataset" ),
            #      hr(),
            #      sliderInput(
            #        inputId = "week3",
            #        label = "Please select desired Epiweek range",
            #        value = c(0,as.numeric(strftime(Sys.Date(), format = "%V"))),
            #        min = 0, max = as.numeric(strftime(Sys.Date(), format = "%V")),
            #        width = "400px"), #Added slider input fo x axis determination on chart
            #      shinycssloaders::withSpinner(plotlyOutput("green.case.by.week.chart", height = NULL)),
            #    ),
            #    
            #    tabPanel(
            #      title = "Amber Countries and Cases by Epiweek",
            #      p("This graph shows the count of cases from travellers outside NI per epiweek, broken down by amber countries travelled" ),
            #      p(strong("Please Note:"), "NA countries omitted from this dataset" ),
            #      hr(),
            #      sliderInput(
            #        inputId = "week4",
            #        label = "Please select desired Epiweek range",
            #        value = c(0,as.numeric(strftime(Sys.Date(), format = "%V"))),
            #        min = 0, max = as.numeric(strftime(Sys.Date(), format = "%V")),
            #        width = "400px"), #Added slider input fo x axis determination on chart
            #      shinycssloaders::withSpinner(plotlyOutput("amber.case.by.week.chart", height = NULL)),
            #)
          )
        )
      ),
      
      #--------------DOWNLOADABLE REPORTS--------------
      tabItem(
        
        tabName = "reports",
        fluidRow(
          
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Weekly Reports",
            p("The tables below show reports for the cases that have travelled outside Northern Ireland yesterday, within the current epiweek, as well as the previous epiweek within the year 2021."),
          ),
          
          tabBox(
            width = 12,
            
            tabPanel(
              width = 12,
              title = "Daily Report - Yesterday",
              downloadButton("downloadData_yesterday", "Download Yesterday Report"),
              hr(),
              withSpinner(DT::dataTableOutput("travellers_yesterday"), type = 2)
            ),
            
            tabPanel(
              width = 12,
              title = "Weekly Report - Current Epiweek",
              downloadButton("downloadData_this_week", "Download Current Epiweek Report"),
              hr(),
              withSpinner(DT::dataTableOutput("travellers_this_week"), type = 2)
            ),
            
            tabPanel(
              title = "Weekly Report - Previous Epiweek",
              downloadButton("downloadData_last_week", "Download Previous Epiweek Report"),
              hr(),
              withSpinner(DT::dataTableOutput("travellers_last_week"), type = 2)
            ),
            
            tabPanel(
              title = "Weekly Report - Previous Two Epiweeks",
              downloadButton("downloadData_last_Two_weeks", "Download Previous 2 Epiweeks Report"),
              hr(),
              withSpinner(DT::dataTableOutput("travellers_two_week"), type = 2)
            ),
            
            tabPanel(
              title = "Cumulative Report",
              downloadButton("downloadData_cumulative", "Download Cumulative Travel Report"),
              hr(),
              withSpinner(DT::dataTableOutput("travellers_cum"), type = 2)
            )
          )
        )
      ),
      
      #--------------TABLES--------------
      tabItem(
        
        tabName = "tables",
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Country Status List",
            p("Please enter name of country in search bar for RAG status"), 
            hr(),
            withSpinner(DT::dataTableOutput("all_countries"), type = 2)
          )
        )
      ),
      
      #--------------METHODOLOGY--------------
      
      tabItem(
        
        tabName = "methodology",
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Methodology",
            p(strong("Home")),
            p("The information boxes displayed on the home tab give an insight into how many COVID-19 cases were assosiated with travel outside NI within the year 2021.
          Each week is split up into epiweeks and a countries RAG status is updated every week based upon scraped data from the NI Direct webpage."),
            p("An amber country is a country not presented within the green or red lists."),
            
            p(strong("Country Frequencies")),
            p("This table gives an insight into the number of cases associated with a country within the year 2021.
          Each week is split up into epiweeks and a countries RAG status is updated every week based upon scraped data from the NI Direct webpage."),
            
            p(strong("Cases by Epiweek")),
            p("The data displayed in this chart depicts the number of cases per epiweek associated with travel outside NI by RAG status of countries.
          Each week is split up into epiweeks and a countries RAG status is updated every week based upon scraped data from the NI Direct webpage."),
            
            p(strong("Countries by Epiweek")),
            p("The data displayed in this chart depicts the number of cases per epiweek associated with travel outside NI by the countries travlled.
          This chart is then split into tabs depicted only green and amber countries travelled. (Red countries are omitted due to low numbers.)
          Each week is split up into epiweeks and a countries RAG status is updated every week based upon scraped data from the NI Direct webpage."),
            
            p(strong("Country Status")),
            p("This table shows the current RAG status for every country within the current epiweek."),
            
            p(strong("References")),
            tagList("Green Countries Reference:", a("Green List.", href= "https://www.nidirect.gov.uk/node/14416#toc-1")), 
            tagList("Red Countries Reference:", a("Red List.", href= "https://www.nidirect.gov.uk/articles/coronavirus-covid-19-travelling-red-list-country#toc-0"))
          )
        )
      )
    )
  )
)
