#' reports UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_reports_ui <- function(id){
  
  shinydashboard::tabItem(
    tabName = "reports",
    
    shiny::fluidRow(
      
      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Weekly Reports",
        p("The tables below show reports for the cases that have travelled outside Northern Ireland yesterday, within the current epiweek, as well as the previous epiweek within the year 2021."),
      ),
      
      shinydashboard::tabBox(
        width = 12,
        
        shiny::tabPanel(
          width = 12,
          title = "Daily Report - Yesterday",
          shiny::downloadButton(shiny::NS(id, "downloadData_yesterday"), "Download Yesterday Report"),
          hr(),
          shinycssloaders::withSpinner(DT::dataTableOutput(shiny::NS(id, "travellers_yesterday")))
        ),
        
        shiny::tabPanel(
          width = 12,
          title = "Weekly Report - Current Epiweek",
          shiny::downloadButton(shiny::NS(id, "downloadData_this_week"), "Download Current Epiweek Report"),
          hr(),
          shinycssloaders::withSpinner(DT::dataTableOutput(shiny::NS(id, "travellers_this_week")))
        ),
        
        shiny::tabPanel(
          title = "Weekly Report - Previous Epiweek",
          shiny::downloadButton(shiny::NS(id, "downloadData_last_week"), "Download Previous Epiweek Report"),
          hr(),
          shinycssloaders::withSpinner(DT::dataTableOutput(shiny::NS(id, "travellers_last_week")))
        ),
        
        shiny::tabPanel(
          title = "Weekly Report - Previous Two Epiweeks",
          shiny::downloadButton(shiny::NS(id, "downloadData_last_Two_weeks"), "Download Previous 2 Epiweeks Report"),
          hr(),
          shinycssloaders::withSpinner(DT::dataTableOutput(shiny::NS(id, "travellers_two_week")))
        ),
        
        shiny::tabPanel(
          title = "Cumulative Report",
          shiny::downloadButton(shiny::NS(id, "downloadData_cumulative"), "Download Cumulative Travel Report"),
          hr(),
          shinycssloaders::withSpinner(DT::dataTableOutput(shiny::NS(id, "travellers_cum")))
        )
      )
    )
  )
}
    
#' reports Server Functions
#'
#' @noRd 
mod_reports_server <- function(id){
  moduleServer( id, function(input, output, session){
    # Previous Day
    
    output$travellers_yesterday = DT::renderDataTable({
      DT::datatable(previous.day.report,
                    filter = 'top',
                    options = list(
                      dom = 'lBftrip',
                      scrollX = T,
                      order = list(
                        9,
                        "desc"),
                      columnDefs = list(
                        list(visible = FALSE, targets = 0))))
    })
    
    ## Downloadable csv of selected dataset 
    output$downloadData_yesterday <- shiny::downloadHandler(
      filename = "DataPreviousDay.csv",
      content = function(file) {
        write.csv(previous.day.report, file, row.names = FALSE)
      }
    )
    
    # Current Week
    
    output$travellers_this_week = DT::renderDataTable({
      DT::datatable(current.week.report, 
                    filter = 'top',
                    options = list(
                      dom = 'lBftrip',
                      scrollX = T,
                      order = list(
                        9,
                        "desc"),
                      columnDefs = list(
                        list(visible = FALSE, targets = 0))))
    })
    
    ## Downloadable csv of selected dataset 
    output$downloadData_this_week <- shiny::downloadHandler(
      filename = "DataCurrentEpiweek.csv",
      content = function(file) {
        write.csv(current.week.report, file, row.names = FALSE)
      }
    )
    
    # Previous Week
    
    output$travellers_last_week = DT::renderDataTable({
      DT::datatable(previous.week.report, 
                    filter = 'top',
                    options = list(
                      dom = 'lBftrip',
                      scrollX = T,
                      order = list(
                        9,
                        "desc"),
                      columnDefs = list(
                        list(visible = FALSE, targets = 0))))
    })
    
    ## Downloadable csv of selected dataset
    output$downloadData_last_week <- shiny::downloadHandler(
      filename = "DataPreviousEpiweek.csv",
      content = function(file) {
        write.csv(previous.week.report, file, row.names = FALSE)
      }
    )
    
    # Previous 2 Weeks
    
    output$travellers_two_week = DT::renderDataTable({
      DT::datatable(last2.epiweeks.report,
                    filter = 'top',
                    options = list(
                      dom = 'lBftrip',
                      scrollX = T,
                      order = list(
                        9,
                        "desc"),
                      columnDefs = list(
                        list(visible = FALSE, targets = 0))))
    })
    
    ## Downloadable csv of selected dataset
    output$downloadData_last_Two_weeks <- shiny::downloadHandler(
      filename = "DataPreviousTwoEpiweeks.csv",
      content = function(file) {
        write.csv(last2.epiweeks.report, file, row.names = FALSE)
      }
    )
    
    # Cumulative
    
    output$travellers_cum = DT::renderDataTable({
      DT::datatable(culmulative.report, 
                    filter = 'top',
                    options = list(
                      dom = 'lBftrip',
                      scrollX = T,
                      order = list(
                        9,
                        "desc"),
                      columnDefs = list(
                        list(visible = FALSE, targets = 0)))
      )
    })
    
    ## Downloadable csv of selected dataset
    output$downloadData_cumulative <- shiny::downloadHandler(
      filename = "Cumulativetravel.csv",
      content = function(file) {
        write.csv(culmulative.report, file, row.names = FALSE)
      }
    )
  })
}
    
## To be copied in the UI
# mod_reports_ui("reports_ui_1")
    
## To be copied in the server
# mod_reports_server("reports_ui_1")
