####### SETUP  ####### 

# Libraries
library(shinydashboard)
library(rangeBuilder)
library(tidyverse)
library(plyr)
library(lubridate)
library(rvest)
library(rworldmap)
library(clinUtils)
library(DT)
library(ggplot2)
library(plotly)

#detach("package:plyr", unload = TRUE)

# Source scripts
source("./load_data.R")
source("./build_data.R")

#load app

function(input, output, session) {
  
  
  ####### HOME ######
  
  # InfoBoxes
  
  ## Total Cases
  output$total_cases <- renderInfoBox({
    infoBox(
      "Total Reported Cases from International Travellers", 
      paste0(nrow(filter(travellers.status))), 
      icon = icon("globe-europe"), 
      color ="blue")
  })
  ## Total Cases this Week
  output$cases_this_week <- renderInfoBox({
    infoBox(
      "Reported Cases This Week from International Travellers", 
      paste0(nrow(filter(travellers.status,
                         EpiweekCreated == current.epiweek))), 
      icon = icon("globe-europe"), 
      color = "blue")
  })
  
  ## Total Percent
  output$total_percent <- renderInfoBox({
    infoBox(
      "Percentage of Total Cases that are International Travellers", 
      paste0(round((nrow(travellers.status)/nrow(Allcases))*100, 2), "%"), 
      icon = icon("percent"), 
      color ="light-blue")
  })
  
  ## Total Percent this Week
  
  output$percent_this_week <- renderInfoBox({
    infoBox(
      "Percentage of Cases This Week that are International Travellers", 
      paste0(round((nrow(filter(travellers.status, EpiweekCreated == current.epiweek))/nrow(filter(Allcases, EpiweekCreated == current.epiweek)))*100, 2), "%"),
      icon = icon("percent"), 
      color = "light-blue")
  })
  
  output$total_non_red_cases <- renderInfoBox({
    infoBox(
      "Total Reported Cases from Non-Red Countries", 
      paste0(nrow(filter(travellers.status,
                         Status == "Non-Red"))), 
      icon = icon("chart-bar"), 
      color ="green")
  })
  
  output$non_red_cases_this_week <- renderInfoBox({
    infoBox(
      "Cases This Week from Non-Red Countries", 
      paste0(nrow(filter(travellers.status,
                         Status == "Non-Red",
                         EpiweekCreated == current.epiweek))), 
      icon = icon("plane-arrival"), 
      color = "green")
  })
  
  output$total_red_cases <- renderInfoBox({
    infoBox(
      "Total Reported Cases from Red Countries", 
      paste0(nrow(filter(travellers.status,
                         Status == "Red"))), 
      icon = icon("chart-bar"), 
      color ="red")
  })
  
  output$red_cases_this_week <- renderInfoBox({
    infoBox(
      "Cases This Week from Red Countries", 
      paste0(nrow(filter(travellers.status,
                         Status == "Red",
                         EpiweekCreated == current.epiweek))), 
      icon = icon("plane-arrival"), 
      color = "red")
  })
  
  # output$total_red_cases <- renderInfoBox({
  #   infoBox(
  #     "Total Reported Cases from Red Countries", 
  #     paste0(nrow(filter(travellers.status,
  #                        Status == "Red"))), 
  #     icon = icon("chart-bar"), 
  #     color ="red")
  # })
  # 
  # output$red_cases_this_week <- renderInfoBox({
  #   infoBox(
  #     "Cases This Week from Red Countries", 
  #     paste0(nrow(filter(travellers.status,
  #                        Status == "Red",
  #                        EpiweekCreated == current.epiweek))), 
  #     icon = icon("plane-arrival"), 
  #     color = "red")
  # })
  
  ####### COUNTRY FREQUENCY  #######
  
  output$top_ten = DT::renderDataTable({
    DT::datatable(topten, options = list(pageLength = 10))
  })
  
  ####### CASES BY RAG  #######
  
  # By Epiweek
  
  output$case.by.week.chart <- renderPlotly({
    
    travellers.status.week.plot <- travellers.status %>% 
      select(EpiweekReturned, Status) 
    
    travellers.status.week.plot$EpiweekReturned <- gsub("Epiweek"," ", as.character(travellers.status.week.plot$EpiweekReturned))
    travellers.status.week.plot$EpiweekReturned <- as.numeric(travellers.status.week.plot$EpiweekReturned)
    travellers.status.week.plot$Status[is.na(travellers.status.week.plot$Status)] <- "NA"
    travellers.status.week.plot$Status <- as.factor(travellers.status.week.plot$Status)
    travellers.status.week.plot$Status <- factor(travellers.status.week.plot$Status, levels=c("Red",
                                                                                              #"Amber",
                                                                                              "Non-Red",
                                                                                              "NA"))
    
    case.by.week.chart <- ggplot(travellers.status.week.plot, aes(EpiweekReturned)) +
      geom_bar(aes(fill=Status)) +
      scale_fill_manual(values = c("firebrick",
                                   #"goldenrod",
                                   "seagreen",
                                   "grey45"), name = "Status") +
      scale_x_continuous(limits =  c(input$week[1], input$week[2]), breaks = c(1:as.numeric(strftime(Sys.Date(), format = "%V")))) +
      theme_bw()
    
    case.by.week.chart <- ggplotly(case.by.week.chart) %>% 
      layout(xaxis = list(tickangle = 45))
    
    case.by.week.chart
  })
  
  # By Month
  
  #detach("package:plyr", unload = TRUE)
  
  output$case.by.month.chart <- renderPlotly({
    
    travellers.status.mon.plot <- travellers.status %>% 
      select(CreatedOn, Status) %>% 
      filter(CreatedOn >= "2021-01-01" & CreatedOn <= Sys.Date()+1) %>%
      dplyr::group_by(month = lubridate::floor_date(as.Date(CreatedOn, format = "%d-%m"), "month")) %>% 
      dplyr::mutate(Non_Red = if_else(Status == "Non-Red", 1, 0)) %>% 
     # dplyr::mutate(Amber = if_else(Status == "Amber", 1, 0)) %>% 
      dplyr::mutate(Red = if_else(Status == "Red", 1, 0)) %>% 
      dplyr::mutate("NA" = if_else(is.na(Status), 1, 0)) %>%
      dplyr::summarise_at(c("Non_Red",
                            #"Amber",
                            "Red",
                            "NA"), sum, na.rm = TRUE) %>% 
      tidyr::pivot_longer(cols = c("Non_Red",
                                   #"Amber",
                                   "Red",
                                   "NA")) %>% 
      filter(month >= "2021-01-01" & month <= Sys.Date()+1)
    
    travellers.status.mon.plot$name <- as.factor(travellers.status.mon.plot$name)
    travellers.status.mon.plot$name <- factor(travellers.status.mon.plot$name, levels=c("Red",
                                                                                       # "Amber",
                                                                                        "Non-Red",
                                                                                        "NA"))
    
    
    case.by.month.plot <- ggplot(travellers.status.mon.plot, aes(fill = name, x = month, y = value)) +
      geom_bar(position="stack", stat="identity") +
      scale_fill_manual(values = c("firebrick",
                                   #"goldenrod",
                                   "seagreen",
                                   "grey45"), name = "Status") +
      scale_x_date(date_labels = "%m-%Y", breaks = "1 month") +
      theme_bw()
    
    case.by.month.plot <- ggplotly(case.by.month.plot) %>% 
      layout(xaxis = list(tickangle = 45))
    
    case.by.month.plot
  })
  
  ####### CASES BY COUNTRY #######
  
  # All Countries by Epiweek
  output$all.rag.case.by.week.chart <- renderPlotly({
    
    country.plot <- travellers.status %>% 
      select(CountriesVisited, EpiweekReturned) 
    
    
    
    country.count <- as.data.frame(table(travellers$CountriesVisited))
    country.count <- arrange(country.count, desc(Freq))
    
    country.plot$country <- country.plot$CountriesVisited
    country.plot$country[country.plot$country != "ROI" &
                           country.plot$country != "England" &
                           country.plot$country != "Scotland" &
                           country.plot$country != "Spain" & 
                           country.plot$country != "Portugal" &
                           country.plot$country != "Wales" &
                           country.plot$country != "Poland" &
                           country.plot$country != "India" &
                           country.plot$country != "Jersey" &
                           country.plot$country != "USA"] <- "Other" #Top Ten 
    
    country.plot$country <- factor(country.plot$country, levels=c("ROI", "England", "Scotland", "Spain", "Portugal", "Wales", "Poland", "India", "Jersey", "USA", "Other"))
    
    country.plot$EpiweekReturned <- gsub("Epiweek"," ", as.character(country.plot$EpiweekReturned))
    country.plot$EpiweekReturned <- as.numeric(country.plot$EpiweekReturned)
    
    all.countries.by.week <- ggplot(country.plot, aes(EpiweekReturned)) +
      geom_bar(aes(fill=country), position = position_stack(reverse = TRUE)) +
      scale_fill_manual(values = c("seagreen", "red3", "royalblue", "gold", "maroon", 
                                   "springgreen4", "red", "olivedrab", "skyblue", "navy", "wheat4"), name = "Country") +
      scale_x_continuous(limits =  c(input$week2[1], input$week2[2]), breaks = c(1:as.numeric(strftime(Sys.Date(), format = "%V")))) + #Adjusting the size of graph in the tab 
      theme_bw()
    
    all.countries.by.week <- ggplotly(all.countries.by.week) %>% 
      layout(xaxis = list(tickangle = 45))
    
    all.countries.by.week
  })
  
  # All Countries Percentage by Epiweeek
  output$all.rag.case.by.week.chart.per <- renderPlotly({
    
    country.plot <- travellers.status %>% 
      select(CountriesVisited, EpiweekReturned) 
    
    country.plot$country <- country.plot$CountriesVisited
    country.plot$country[country.plot$country != "ROI" &
                           country.plot$country != "England" &
                           country.plot$country != "Scotland" &
                           country.plot$country != "Spain" & 
                           country.plot$country != "Portugal" &
                           country.plot$country != "Wales" &
                           country.plot$country != "Poland" &
                           country.plot$country != "India" &
                           country.plot$country != "Jersey" &
                           country.plot$country != "United States"] <- "Other" #Top Ten. Could use %in%
    
    country.plot$country <- factor(country.plot$country, levels=c("ROI", "England", "Scotland", "Spain", "Portugal", "Wales", "Poland", "India", "Jersey", "United States", "Other"))
    
    country.plot$EpiweekReturned <- gsub("Epiweek"," ", as.character(country.plot$EpiweekReturned))
    country.plot$EpiweekReturned <- as.numeric(country.plot$EpiweekReturned)
    
    all.countries.by.week.per <- ggplot(country.plot, aes(EpiweekReturned)) +
      geom_bar(aes(fill=country), position = position_fill(reverse = TRUE)) +
      scale_fill_manual(values = c("seagreen", "red3", "royalblue", "gold", "maroon", 
                                   "springgreen4", "red", "olivedrab", "skyblue", "navy", "wheat4"), name = "Country") +
      scale_x_continuous(limits =  c(input$week2[1], input$week2[2]), breaks = c(1:as.numeric(strftime(Sys.Date(), format = "%V")))) + #Adjusting the size of graph in the tab 
      theme_bw()
    
    all.countries.by.week.per <- ggplotly(all.countries.by.week.per) %>% 
      layout(xaxis = list(tickangle = 45))
    
    all.countries.by.week.per
  })
  
  # All Countries by Month
  output$all.rag.case.by.mon.chart <- renderPlotly({
    
    country.plot.mon <- travellers.status %>% 
      select(CreatedOn, CountriesVisited) 
    
    country.plot.mon$CountriesVisited <- country.plot$CountriesVisited
    country.plot.mon$CountriesVisited[country.plot.mon$CountriesVisited != "ROI" &
                                        country.plot.mon$CountriesVisited != "England" &
                                        country.plot.mon$CountriesVisited != "Scotland" &
                                        country.plot.mon$CountriesVisited != "Spain" & 
                                        country.plot.mon$CountriesVisited != "Portugal" &
                                        country.plot.mon$CountriesVisited != "Wales" &
                                        country.plot.mon$CountriesVisited != "Poland" &
                                        country.plot.mon$CountriesVisited != "India" &
                                        country.plot.mon$CountriesVisited != "Jersey" &
                                        country.plot.mon$CountriesVisited != "United States"] <- "Other" #Top Ten. Could use %in%
    
    country.plot.mon <- country.plot.mon %>%  
      filter(CreatedOn >= "2021-01-01" & CreatedOn <= Sys.Date()+1) %>%
      dplyr::group_by(month = lubridate::floor_date(as.Date(CreatedOn, format = "%d-%m"), "month")) %>% 
      dplyr::mutate(ROI = if_else(CountriesVisited == "ROI", 1, 0)) %>% 
      dplyr::mutate(England = if_else(CountriesVisited == "England", 1, 0)) %>% 
      dplyr::mutate(Scotland = if_else(CountriesVisited == "Scotland", 1, 0)) %>%
      dplyr::mutate(Portugal = if_else(CountriesVisited == "Portugal", 1, 0)) %>%
      dplyr::mutate(Wales = if_else(CountriesVisited == "Wales", 1, 0)) %>%
      dplyr::mutate(Poland = if_else(CountriesVisited == "Poland", 1, 0)) %>%
      dplyr::mutate(India = if_else(CountriesVisited == "India", 1, 0)) %>%
      dplyr::mutate(Jersey = if_else(CountriesVisited == "Jersey", 1, 0)) %>%
      dplyr::mutate(UnitedStates = if_else(CountriesVisited == "United States", 1, 0)) %>%
      dplyr::mutate(Other = if_else(CountriesVisited == "Other", 1, 0)) %>%
      dplyr::mutate("NA" = if_else(is.na(CountriesVisited), 1, 0)) %>%                  #Could make df and use %in%
      dplyr::summarise_at(c("ROI", "England", "Scotland", "Scotland", "Portugal", "Wales", "Poland", "India", "Jersey", "UnitedStates", "Other", "NA"),
                          sum, na.rm = TRUE) %>% 
      tidyr::pivot_longer(cols = c("ROI", "England", "Scotland", "Scotland", "Portugal", "Wales", "Poland", "India", "Jersey", "UnitedStates", "Other", "NA")) %>% 
      filter(month >= "2021-01-01" & month <= Sys.Date()+1)
    
    country.plot.mon$name <- factor(country.plot.mon$name, levels=c("ROI", "England", "Scotland", "Spain", "Portugal", "Wales", "Poland", "India", "Jersey", "United States", "Other", "NA"))
    
    all.countries.by.month <- ggplot(country.plot.mon, aes(fill = name, x = month, y = value)) +
      geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
      scale_fill_manual(values = c("seagreen", "red3", "royalblue", "gold", "maroon", 
                                   "springgreen4", "red", "olivedrab", "skyblue", "navy", "wheat4", "grey45"), name = "Country") +
      #scale_x_continuous(limits =  c(input$week2[1], input$week2[2]), breaks = c(1:as.numeric(strftime(Sys.Date(), format = "%V")))) + #Adjusting the size of graph in the tab 
      theme_bw()
    
    all.countries.by.month <- ggplotly(all.countries.by.month) %>% 
      layout(xaxis = list(tickangle = 45))
    
    all.countries.by.month
  })
  
  #### OLD CODE #####
  # # Green Countries 
  #   output$green.case.by.week.chart <- renderPlotly({
  #     
  #   country.count.green <- travellers.status %>% 
  #                          filter(Status=="Green")
  #   
  #   country.count.green <- as.data.frame(table(country.count.green$CountriesVisited))
  #   country.count.green <- arrange(country.count.green, desc(Freq))
  #   topten.green <- head(country.count.green, 10)
  #   colnames(topten.green) <- c("country", "count")
  #   topten.green <- left_join(topten.green, all.countries.status, by = "country")
  #   colnames(topten.green) <- c("Country", "Count", "Status")
  #   
  #   green.country.plot <- travellers.status %>% 
  #     filter(Status == "Green") %>% 
  #     select(CountriesVisited, EpiweekReturned) 
  #   
  #   ## Green Top Ten   
  #   green.country.plot$country <- green.country.plot$CountriesVisited
  #   green.country.plot$country[green.country.plot$country != "ROI" &
  #                              green.country.plot$country != "England" &
  #                              green.country.plot$country != "Scotland" &
  #                              green.country.plot$country != "Wales" &
  #                              green.country.plot$country != "Jersey" & 
  #                              green.country.plot$country != "Bulgaria" &
  #                              green.country.plot$country != "Croatia" &
  #                              green.country.plot$country != "Malta" &
  #                              green.country.plot$country != "Slovakia" &
  #                              green.country.plot$country != "Germany"] <- "Other" #Could probably use %in% too
  #   
  #   
  #   green.country.plot$country <- factor(green.country.plot$country, levels=c("ROI", "England", "Scotland", "Wales", "Jersey", "Bulgaria", "Croatia", "Malta", "Slovakia", "Germany", "Other"))
  #   
  #     green.country.plot$EpiweekReturned <- gsub("Epiweek"," ", as.character(green.country.plot$EpiweekReturned))
  #     green.country.plot$EpiweekReturned <- as.numeric(green.country.plot$EpiweekReturned)
  #   
  #   green.countries.by.week <- ggplot(green.country.plot, aes(EpiweekReturned)) +
  #     geom_bar(aes(fill=country), position = position_stack(reverse = TRUE)) +
  #     scale_x_continuous(limits =  c(input$week3[1], input$week3[2]), breaks = c(1:as.numeric(strftime(Sys.Date(), format = "%V")))) + #Adjusting the size of graph in the tab 
  #     theme_bw()
  #   
  #   green.countries.by.week <- ggplotly(green.countries.by.week) %>% 
  #     layout(xaxis = list(tickangle = 45))
  #   
  #  green.countries.by.week
  # })
  #   
  # # Amber Countries 
  #   output$amber.case.by.week.chart <- renderPlotly({
  #     
  #     country.count.amber <- travellers.status %>% 
  #       filter(Status=="Amber")
  #     
  #     country.count.amber <- as.data.frame(table(country.count.amber$CountriesVisited))
  #     country.count.amber <- arrange(country.count.amber, desc(Freq))
  #     topten.amber <- head(country.count.amber, 10)
  #     colnames(topten.amber) <- c("country", "count")
  #     topten.amber <- left_join(topten.amber, all.countries.status, by = "country")
  #     colnames(topten.amber) <- c("Country", "Count", "Status")
  #     
  #     amber.country.plot <- travellers.status %>% 
  #       filter(Status == "Amber") %>% 
  #       select(CountriesVisited, EpiweekReturned) 
  #     
  #     ## Amber Top Ten   
  #     amber.country.plot$country <- amber.country.plot$CountriesVisited
  #     amber.country.plot$country[amber.country.plot$country != "Spain" &
  #                                  amber.country.plot$country != "Portugal" &
  #                                  amber.country.plot$country != "Poland" &
  #                                  amber.country.plot$country != "United States" &
  #                                  amber.country.plot$country != "France" & 
  #                                  amber.country.plot$country != "Greece" &
  #                                  amber.country.plot$country != "India" &
  #                                  amber.country.plot$country != "Ukraine" &
  #                                  amber.country.plot$country != "Italy" &
  #                                  amber.country.plot$country != "Morocco"] <- "Other" #Could probably use %in% too
  #     
  #     
  #     amber.country.plot$country <- factor(amber.country.plot$country, levels=c("Spain", "Portugal", "Poland", "United States", "France", "Greece", "India", "Ukraine", "Italy", "Morocco", "Other"))
  #     
  #     amber.country.plot$EpiweekReturned <- gsub("Epiweek"," ", as.character(amber.country.plot$EpiweekReturned))
  #     amber.country.plot$EpiweekReturned <- as.numeric(amber.country.plot$EpiweekReturned)
  #     
  #     amber.countries.by.week <- ggplot(amber.country.plot, aes(EpiweekReturned)) +
  #       geom_bar(aes(fill=country), position = position_stack(reverse = TRUE)) +
  #       scale_x_continuous(limits =  c(input$week4[1], input$week4[2]), breaks = c(1:as.numeric(strftime(Sys.Date(), format = "%V")))) + #Adjusting the size of graph in the tab
  #       theme_bw()
  #     
  #     amber.countries.by.week <- ggplotly(amber.countries.by.week) %>% 
  #       layout(xaxis = list(tickangle = 45))
  #     
  #     amber.countries.by.week
  #   })
  #### OLD CODE END ####
  
  ####### REPORT DOWNLOADS  #######
  
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
  output$downloadData_yesterday <- downloadHandler(
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
  output$downloadData_this_week <- downloadHandler(
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
  output$downloadData_last_week <- downloadHandler(
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
  output$downloadData_last_Two_weeks <- downloadHandler(
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
  output$downloadData_cumulative <- downloadHandler(
    filename = "Cumulativetravel.csv",
    content = function(file) {
      write.csv(culmulative.report, file, row.names = FALSE)
    }
  )
  
  
  ####### STATUS LIST  ####### 
  
  output$all_countries = DT::renderDataTable({
    DT::datatable(all.countries.status, options = list(pageLength = 50))
  })
  

  ####### METHODOLOGY  #######  
  
}
