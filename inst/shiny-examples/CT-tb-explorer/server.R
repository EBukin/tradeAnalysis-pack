library(shiny)
library(shinydashboard)

if (file.exists("global.R")) source("global.R")

dataPath <- "~/ctData/ShinyData/"

if (file.exists("./dataPath.txt")) dataPath <- readLines("./dataPath.txt")

# SERVER
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)
  
  # Server logic for loading all data
  # allData <- callModule(uploadTradeData, "DataAccess")
  avReps <- callModule(getAvailableReporters, "DataAccess", path = dataPath)
  avData <- callModule(getDataAvailability, "DataAccess", path = dataPath)
  callModule(ctDataAvailability, "DataAccess", getData = avData)
  
  # Country specific trade balance module
  countryData <-
    callModule(loadReporterTradeData, "tbCountryModule", path = dataPath)
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
    callModule(loadReporterTradeData, "tbCommoditiesModule", path = dataPath)
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
