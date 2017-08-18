# UI for loading trade data into the shiny app
uploadTradeDataInput <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("uploadData"), "Upload data", accept = c("rds"))
  )
}

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

