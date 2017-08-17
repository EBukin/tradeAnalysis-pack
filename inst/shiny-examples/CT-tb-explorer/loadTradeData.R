
# Server logic for loading trade data
loadTradeData <- function(input, output, session, path = "data/all.rds") {
  allDataTable <- reactiveValues(allData = NULL)
  allData <- reactive({
    if (input$loadData != 0) {
      if (is.null(allDataTable$allData)) {
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Loading data from a file", value = 0)
        progress$inc(0.1)
        allDataTable$allData <- readr::read_rds(path)
        progress$inc(0.8)
      }
    }
    
    allDataTable$allData
  })
  return(allData)
}