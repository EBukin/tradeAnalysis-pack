# Developing new modules for the app


library(tidyverse)
library(tradeAnalysis)
library(stringr)
library(readr)
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)


# Sourcing modules
# source("inst/shiny-examples/CT-tb-explorer/uploadTradeData.R")
source("inst/shiny-examples/CT-tb-explorer/uploadTradeDataInput.R")
source("inst/shiny-examples/CT-tb-explorer/tbCountryInput.R")
source("inst/shiny-examples/CT-tb-explorer/tbCountryInputUpdate.R")
source("inst/shiny-examples/CT-tb-explorer/tbCountryOutput.R")
source("inst/shiny-examples/CT-tb-explorer/tbCountryOutputLogic.R")
source("inst/shiny-examples/CT-tb-explorer/tbCountry.R")
source("inst/shiny-examples/CT-tb-explorer/tbCommodityInput.R")
source("inst/shiny-examples/CT-tb-explorer/tbCommodity.R")


# 
# # UI for loading trade data into the shiny app
# uploadTradeDataInput <- function(id) {
#   ns <- NS(id)
#   tagList(
#     fileInput(ns("uploadData"), "Upload data", accept = c("rds"))
#   )
# }
# 
# uploadTradeDataObserver <- function(input, output, session) {
#   obs <- reactive({input$uploadData})
#   return(obs)
# }
# 
# # Server logic for loading trade data
# uploadTradeData <- function(input, output, session) {
#   allDataTable <- reactiveValues(allData = NULL)
#   allDataUpload <- reactive({
#     browser()
#     if (!is.null(input$uploadData)) {
#       allDataTable$allData <- readr::read_rds(input$uploadData$datapath)
#     }
#     allDataTable$allData
#   })
#   return(allDataUpload)
# }

getAvailableReporters <- function(input, output, session, path = "~/ctData/ShinyData/") {
  listFiles <- reactive({
    list.files(path, ".rds")
  })
  return(listFiles)
}

# Server logic for loading trade data
loadReporterTradeData <- function(input, output, session, path = "~/ctData/ShinyData/") {
  allDataTable <- reactiveValues(allData = NULL)
  listFiles <- reactive({
    list.files(path, ".rds")
  })
  allDataUpload <- eventReactive(input$tbReporter, {
    # browser()
    oneFilePath <-  str_c(path, "/", listFiles()[str_detect(listFiles(), str_c("^", input$tbReporter, ".rds$"))])
    
    if (file.exists(oneFilePath)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Loading data from the file", value = 0)
      progress$inc(0.1)
      allDataTable$allData <- readr::read_rds(oneFilePath)
    } 
    allDataTable$allData
  })
  return(allDataUpload)
}

getSelectedReporter <- function(input, output, session) {
  
  allDataTable <- reactive({
    input$tbReporter
  })
  return(allDataTable)
}


library(shiny)
library(shinydashboard)

# UI
header <- dashboardHeader(title = "COMTRADE data explorer")
sidebar <- dashboardSidebar(
  # uploadTradeDataInput("DataAccess"),
                            sidebarMenu(
                              id = "tab",
                              menuItem(
                                "TB by partners",
                                tabName = "tbByPart",
                                icon = icon("globe"),
                                selected = TRUE
                              ),
                              menuItem(
                                "TB by commodity",
                                tabName = "tbByCom",
                                icon = icon("line-chart"),
                                selected = TRUE
                              )
                            ))
tbCountriesTab <-
  function() {
    fluidPage(
      h2("tbByPart"),
      tbCountryInput("tbCountryModule"),
      tbCountryOutput("tbCountryModule")
    )
    
  }

tbCommoditiesTab <-
  function() {
    fluidPage(h2("tbByComqw"),
              tbCommodityInput("tbCommoditiesModule"),
              tbCountryOutput("tbCommoditiesModule"))
  }


body <-
  dashboardBody(
    tabItems(tabItem(tabName = "tbByPart", tbCountriesTab()), 
             tabItem(tabName = "tbByCom", tbCommoditiesTab())
             )
    )

ui <- dashboardPage(header, sidebar, body)

# SERVER
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)
  
  # Server logic for loading all data
  # allData <- callModule(uploadTradeData, "DataAccess")
  avReps <- callModule(getAvailableReporters, "DataAccess")
  
  # Country specific trade balance module
  countryData <- callModule(loadReporterTradeData, "tbCountryModule")
  callModule(tbCountryInputUpdate, "tbCountryModule", getData = countryData, allReporters = avReps)
  getTbPlot <- callModule(tbCountry, "tbCountryModule", getData = countryData)
  callModule(tbCountryOutputLogic, "tbCountryModule", getTbPlot)
  
  
  comData <- callModule(loadReporterTradeData, "tbCommoditiesModule")
  callModule(tbCountryInputUpdate, "tbCommoditiesModule", getData = comData, allReporters = avReps)
  getTbComPlot <- callModule(tbCommodity, "tbCommoditiesModule", getData = comData)
  callModule(tbCountryOutputLogic, "tbCommoditiesModule", getTbComPlot)
  # observeEvent(getTbPlot(), getTbComPlot())
}


shinyApp(ui, server)
