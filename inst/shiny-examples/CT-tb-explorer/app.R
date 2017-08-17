
library(shiny)
library(shinydashboard)
source("global.R")

# UI
header <- dashboardHeader(title = "COMTRADE data explorer")
sidebar <- dashboardSidebar(uploadTradeDataInput("test"),
                            sidebarMenu(
                              id = "tab",
                              menuItem(
                                "Trade balance by partners",
                                tabName = "tbByPart",
                                icon = icon("line-chart"),
                                selected = TRUE
                              )
                            ))
annualTradeBalanceTab <-
  function() {
    tabItem(tabName = "tbByPart",
            fluidPage(
              h2("tbByPart"),
              tbCountryInput("test"),
              tbCountryOutput("test")
            ))
    
  }
body <-
  dashboardBody(tabItems(annualTradeBalanceTab()))
ui <- dashboardPage(header, sidebar, body)

# SERVER
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)
  allData <- callModule(uploadTradeData, "test")
  callModule(tbCountryInputUpdate, "test", getData = allData)
  getTbPlot <- callModule(tbCountry, "test", getData = allData)
  callModule(tbCountryOutputLogic, "test", getTbPlot)
  
}

shinyApp(ui, server)