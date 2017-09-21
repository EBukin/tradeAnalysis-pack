getAvailableReporters <-
  function(input, output, session, path = "~/ctData/ShinyData/") {
    listFiles <- reactive({
      files <- list.files(path, ".rds")
      # files[files != "0.rds"]
    })
    return(listFiles)
  }