# Server logic for loading trade data
uploadTradeData <- function(input, output, session) {
  allDataTable <- reactiveValues(allData = NULL)
  allDataUpload <- reactive({
    if (!is.null(input$uploadData)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Loading data from the file", value = 0)
      progress$inc(0.1)
      allDataTable$allData <- readr::read_rds(input$uploadData$datapath)
    }
    allDataTable$allData
  })
  return(allDataUpload)
}