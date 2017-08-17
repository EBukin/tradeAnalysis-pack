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
# source("inst/shiny-examples/CT-tb-explorer/uploadTradeDataInput.R")
source("inst/shiny-examples/CT-tb-explorer/tbCountryInput.R")
source("inst/shiny-examples/CT-tb-explorer/tbCountryInputUpdate.R")
source("inst/shiny-examples/CT-tb-explorer/tbCountryOutput.R")
source("inst/shiny-examples/CT-tb-explorer/tbCountry.R")
source("inst/shiny-examples/CT-tb-explorer/tbCommodityInput.R")



# UI for loading trade data into the shiny app
uploadTradeDataInput <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("uploadData"), "Upload data", accept = c("rds"))
  )
}

uploadTradeDataObserver <- function(input, output, session) {
  obs <- reactive({input$uploadData})
  return(obs)
}

# Server logic for loading trade data
uploadTradeData <- function(input, output, session) {
  allDataTable <- reactiveValues(allData = NULL)
  allDataUpload <- reactive({
    browser()
    if (!is.null(input$uploadData)) {
      allDataTable$allData <- readr::read_rds(input$uploadData$datapath)
    }
    allDataTable$allData
  })
  return(allDataUpload)
}





library(shiny)
library(shinydashboard)

# UI
header <- dashboardHeader(title = "COMTRADE data explorer")
sidebar <- dashboardSidebar(
  uploadTradeDataInput("DataAccess"),
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
              tbCommodityInput("tbCommoditiesModule"))
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
  allData <- callModule(uploadTradeData, "DataAccess")
  callModule(tbCountryInputUpdate, "tbCountryModule", getData = allData)
  getTbPlot <- callModule(tbCountry, "tbCountryModule", getData = allData)
  callModule(tbCountryOutputLogic, "tbCountryModule", getTbPlot)
  callModule(tbCountryInputUpdate, "tbCommoditiesModule", getData = allData)
  
}


shinyApp(ui, server)
