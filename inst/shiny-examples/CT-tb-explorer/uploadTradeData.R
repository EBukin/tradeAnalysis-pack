# Server logic for loading trade data
uploadTradeData <- function(input, output, session) {
  allDataTable <- reactiveValues(allData = NULL)
  allDataUpload <- reactive({
    if (!is.null(input$uploadData)) {
      allDataTable$allData <- readr::read_rds(input$uploadData$datapath)
    }
    allDataTable$allData
  })
  return(allDataUpload)
}