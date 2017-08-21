# Developing new modules for the app


library(tidyverse)
library(tradeAnalysis)
library(stringr)
library(readr)
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

folder <- "inst/shiny-examples/CT-tb-explorer/helpers/"
lapply(file.path(folder,list.files(folder, ".R$")), source)

source("inst/shiny-examples/CT-tb-explorer/ui.R")
source("inst/shiny-examples/CT-tb-explorer/server.R")

app <- shinyApp(ui, server)
runApp(app)

