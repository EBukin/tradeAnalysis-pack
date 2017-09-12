library(shiny)
library(shinydashboard)
library(shinyjs)

if (file.exists("global.R")) source("global.R")

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# UI
header <- dashboardHeader(title = "COMTRADE data explorer")
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tab",
    menuItem(
      "Data availability",
      tabName = "dataAvailability",
      icon = icon("database"),
      selected = TRUE
    ),
    menuItem(
      "Main importers/exporters",
      tabName = "mainImpExp",
      icon = icon("globe")
    ),
    menuItem(
      "By partner: structure of trade",
      tabName = "byPartners",
      icon = icon("globe")
    ),
    menuItem(
      "By commodity: structure of trade",
      tabName = "byCommodities",
      icon = icon("line-chart")
    ),
    actionButton("close", "Close the App and exit", status = "danger")
  ))



dataAvailTab <-
  function() {
    fluidPage(
      h2("Data availability"),
      ctDataAvailabilityUI("DataAccess"),
      useShinyjs(),
      extendShinyjs(text = jscode, functions = c("closeWindow"))
    )
  }

mainTradersTab <- 
  function() {
    fluidPage(
      h2("Main importers and exporters by commodity"),
      mainTradersInput("mainTradersModule"),
      byPartnersOutput("mainTradersModule")
    )
  }

tbCountriesTab <-
  function() {
    fluidPage(
      h2("Trade structure by partners"),
      byPartnersInput("tbCountryModule"),
      byPartnersOutput("tbCountryModule")
    )
  }

tbCommoditiesTab <-
  function() {
    fluidPage(
      h2("Trade Balance of a reporter by commodity"),
      byCommodityInput("tbCommoditiesModule"),
      byPartnersOutput("tbCommoditiesModule")
    )
  }

body <-
  dashboardBody(tabItems(
    tabItem(tabName = "dataAvailability", dataAvailTab()),
    tabItem(tabName = "mainImpExp", mainTradersTab()),
    tabItem(tabName = "byPartners", tbCountriesTab()),
    tabItem(tabName = "byCommodities", tbCommoditiesTab())
  ))

ui <- dashboardPage(header, sidebar, body)