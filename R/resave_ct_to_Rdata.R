#' Resave one CT zip file in an R data file.
resave_ct_to_Rdata <-
  function(data,
           fromFolder,
           toFolder) {
    require(stringr)
    require(tidyverse)
    # Defining files
    # browser()
    fromFile <- file.path(fromFolder, data$name.x)
    toFile <-
      file.path(toFolder, data$name.y)
    
    # Moving to old old R data file
    if (file.exists(file.path(toFolder, data$name.y))) {
      moveToOld(file.path(toFolder, data$name.y), fromFolder)
    }
    
    # Saving new Rdata file
    object <-
      str_c("ct", data$r, data$px, data$freq, data$ps, sep = "_")
    assign(object, readCTZIP(file = data$name.x, folder = fromFolder))
    save(list = c(object), file = toFile)
    rm(list = object)
    gc(verbose = F)
  }