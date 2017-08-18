
library(shiny)
library(shinydashboard)

source("global.R")

# UI
header <- dashboardHeader(title = "COMTRADE data explorer")
sidebar <- dashboardSidebar(# uploadTradeDataInput("DataAccess"),
  sidebarMenu(
    id = "tab",
    menuItem(
      "Data availability",
      tabName = "availTab",
      icon = icon("database"),
      selected = TRUE
    ),
    menuItem(
      "TB by partners",
      tabName = "tbByPart",
      icon = icon("globe")
    ),
    menuItem(
      "TB by commodity",
      tabName = "tbByCom",
      icon = icon("line-chart")
    )
  ))

dataAvailTAb <-
  function() {
    fluidPage(
      h2("Data availability"),
      ctDataAvailabilityUI("DataAccess")
    )
  }

tbCountriesTab <-
  function() {
    fluidPage(
      h2("Trade Balance of a reporter by partner"),
      tbCountryInput("tbCountryModule"),
      tbCountryOutput("tbCountryModule")
    )
  }

tbCommoditiesTab <-
  function() {
    fluidPage(
      h2("Trade Balance of a reporter by commodity"),
      tbCommodityInput("tbCommoditiesModule"),
      tbCountryOutput("tbCommoditiesModule")
    )
  }

body <-
  dashboardBody(tabItems(
    tabItem(tabName = "availTab", dataAvailTAb()),
    tabItem(tabName = "tbByPart", tbCountriesTab()),
    tabItem(tabName = "tbByCom", tbCommoditiesTab())
  ))

ui <- dashboardPage(header, sidebar, body)

# SERVER
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)
  
  # Server logic for loading all data
  # allData <- callModule(uploadTradeData, "DataAccess")
  avReps <- callModule(getAvailableReporters, "DataAccess")
  avData <- callModule(getDataAvailability, "DataAccess")
  callModule(ctDataAvailability, "DataAccess", getData = avData)
  
  # Country specific trade balance module
  countryData <-
    callModule(loadReporterTradeData, "tbCountryModule")
  callModule(
    tbCountryInputUpdate,
    "tbCountryModule",
    getData = countryData,
    allReporters = avReps
  )
  getTbPlot <-
    callModule(tbCountry, "tbCountryModule", getData = countryData)
  callModule(tbCountryOutputLogic, "tbCountryModule", getTbPlot)
  
  
  comData <-
    callModule(loadReporterTradeData, "tbCommoditiesModule")
  callModule(
    tbCountryInputUpdate,
    "tbCommoditiesModule",
    getData = comData,
    allReporters = avReps
  )
  getTbComPlot <-
    callModule(tbCommodity, "tbCommoditiesModule", getData = comData)
  callModule(tbCountryOutputLogic, "tbCommoditiesModule", getTbComPlot)
}

shinyApp(ui, server)