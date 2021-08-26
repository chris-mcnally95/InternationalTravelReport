######## SETUP ######## 

# Libraries
library(shinydashboard)
library(shinycssloaders)

######## SPINNER ########
options(spinner.color = "#0275D8", spinner.color.background="#ffffff", spinner.size =2)

######## UI ######## 

ui <- dashboardPage(
  
  dashboardHeader(title = "International Travel Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Country Status", tabName = "tables", icon = icon("list"))
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
          p(strong("Please Note:")," The free text inputs of country entries are currently being addressed, therefore, some cases will be omitted temporarily")
        )
      ),
      
      fluidRow(
        withSpinner(infoBoxOutput("total_cases", width = 6), type = 2),
        infoBoxOutput("cases_this_week", width = 6)
      ),
      
        fluidRow(
        infoBoxOutput("total_green_cases", width = 6),
        infoBoxOutput("green_cases_this_week", width = 6)
       ),
      
      fluidRow(
        infoBoxOutput("total_amber_cases", width = 6),
        infoBoxOutput("amber_cases_this_week", width = 6)
      ),
      
      fluidRow(
        infoBoxOutput("total_red_cases", width = 6),
        infoBoxOutput("red_cases_this_week", width = 6)
      ),
      
      fluidRow(
        box(
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          title ="Countries by Frequency in 2021",
          p("This table displays the countries associated with the most COVID-19 cases in 2021"),
          p(strong("Please Note:")," The free text inputs of country entries are currently being addressed, therefore, some cases will be omitted temporarily"),
          hr(),
          shinycssloaders::withSpinner(DT::dataTableOutput("top_ten"), type = 2)
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
   )
  )
 )
)
