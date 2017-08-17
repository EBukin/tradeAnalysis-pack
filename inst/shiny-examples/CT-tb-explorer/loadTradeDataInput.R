
# UI for loading trade data into the shiny app
loadTradeDataInput <- function(id) {
  ns <- NS(id)
  tagList(actionButton(ns("loadData"), 
                       "Load data",
                       # style = "background-color: #4CAF50; border: 2px solid black",
                       icon = icon("database"), 
                       width = "80%"))
}