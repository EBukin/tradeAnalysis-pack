getDataAvailability <-
  function(input, output, session, path = "~/ctData/ShinyData/") {
    listFiles <- reactive({
      readr::read_csv(
        file.path(path, list.files(path, ".csv")),
        col_types = cols(
          Reporter.Code = col_integer(),
          Year = col_integer(),
          Type = col_character()
        )
      )
    })
    return(listFiles)
  }
