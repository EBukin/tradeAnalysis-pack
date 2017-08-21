# Server logic for loading trade data
loadReporterTradeData <-
  function(input, output, session, path = "~/ctData/ShinyData/") {
    allDataTable <- reactiveValues(allData = NULL)
    listFiles <- reactive({
      list.files(path, ".rds")
    })
    allDataUpload <- eventReactive(input$tbReporter, {
      # browser()
      # invalidateLater(2000, session)
      oneFilePath <-
        str_c(path, "/", listFiles()[str_detect(listFiles(), str_c("^", input$tbReporter, ".rds$"))])
      
      if (file.exists(oneFilePath)) {
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Loading data from the file", value = 0)
        progress$inc(0.1)
        allDataTable$allData <- readr::read_rds(oneFilePath)
      }
      allDataTable$allData
    })
    return(allDataUpload)
  }

getSelectedReporter <- function(input, output, session) {
  allDataTable <- reactive({
    input$tbReporter
  })
  return(allDataTable)
}
