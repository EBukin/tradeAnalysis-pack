# UI for loading trade data into the shiny app
uploadTradeDataInput <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("uploadData"), "Upload data", accept = c("rds"))
  )
}


