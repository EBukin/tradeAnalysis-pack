#' Copy and save locally shiny ap from a package.
create_local_app <- function(appName, toFolder = "~") {
  require(dplyr)
  # locate all the shiny app examples that exist
  validExamples <-
    list.files(system.file("shiny-examples", package = "tradeAnalysis"))
  
  validExamplesMsg <-
    paste0("Valid examples are: '",
           paste(validExamples, collapse = "', '"),
           "'")
  
  # if an invalid example is given, throw an error
  if (missing(appName) || !nzchar(appName) ||
      !appName %in% validExamples) {
    stop(
      'Please run `runShinyExample()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE
    )
  }
  
  # find and launch the app
  appDir <-
    system.file("shiny-examples", appName, package = "tradeAnalysis")
  
  # New location
  newLocation <- file.path(toFolder, appName)
  
  file.copy(
    from = stringr::str_c(appDir, "/"),
    to = toFolder,
    overwrite = TRUE,
    recursive = TRUE,
    copy.mode = TRUE
  )
  
  dataLocation <- file.path("./ShinyData")
  fromDataLocation <- file.path("~/ctData/ShinyData/")
  dir.create(dataLocation, showWarnings = FALSE)
  writeLines(dataLocation, file.path(newLocation, "dataPath.txt"))
  
  if (dir.exists(fromDataLocation)) {
    file.copy(from = fromDataLocation, 
              to = stringr::str_c(dataLocation,"/"), 
              recursive = TRUE, overwrite = TRUE)
  } else {
    message(stringr::str_c("You need to fill in folder ", dataLocation, " with all data manually."))
  }
  
}
