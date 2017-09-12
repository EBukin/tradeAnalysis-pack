library(shiny)
library(shinydashboard)

if (file.exists("global.R")) source("global.R")

dataPath <- "~/ctData/ShinyData/"

if (file.exists("./dataPath.txt")) dataPath <- readLines("./dataPath.txt")

# SERVER
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)
  
  ### ### ### Global server code
  session$onSessionEnded(stopApp)
  observeEvent(input$close, {
    js$closeWindow()
    stopApp()
  })
  
  ### ### ### Server logic for loading all data ### ### ###
  avReps <- callModule(getAvailableReporters, "DataAccess", path = dataPath)
  avData <- callModule(getDataAvailability, "DataAccess", path = dataPath)
  callModule(ctDataAvailability, "DataAccess", getData = avData)
  
  ### ### ### Global analysis ### ### ###
  worldData <- callModule(loadReporterData, "mainTradersModule", path = dataPath)
  callModule(mainTradersInputUpdate, "mainTradersModule", getData = worldData)
  getMainTradersPlot <- callModule(mainTradersPlotLogic, "mainTradersModule", getData = worldData)
  callModule(byPartnersOutputLogic, "mainTradersModule", getMainTradersPlot)
  
  ### ### ### Country specific trade balance module ### ### ###
  # Loading data
  countryData <- callModule(loadReporterData, "tbCountryModule", path = dataPath)
  
  # Logic for updating input of the by country logic
  callModule(byPartnersInputUpdate, "tbCountryModule", getData = countryData, allReporters = avReps)
  getTbPlot <- callModule(tbPlotLogic, "tbCountryModule", getData = countryData)
  callModule(byPartnersOutputLogic, "tbCountryModule", getTbPlot)
  
  
  comData <-
    callModule(loadReporterData, "tbCommoditiesModule", path = dataPath)
  callModule(
    byPartnersInputUpdate,
    "tbCommoditiesModule",
    getData = comData,
    allReporters = avReps
  )
  getTbComPlot <-
    callModule(byCommodityLogic, "tbCommoditiesModule", getData = comData)
  callModule(byCommodityOutputLogic, "tbCommoditiesModule", getTbComPlot)
}
