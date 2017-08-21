library(shiny)
library(shinydashboard)

if (file.exists("global.R")) source("global.R")


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