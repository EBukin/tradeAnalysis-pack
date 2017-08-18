getAvailableReporters <-
  function(input, output, session, path = "~/ctData/ShinyData/") {
    listFiles <- reactive({
      list.files(path, ".rds")
    })
    return(listFiles)
  }