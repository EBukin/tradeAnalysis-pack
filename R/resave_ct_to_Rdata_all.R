#' Resaving all CT data files from zip archives from one folder to another in R data format
resave_ct_to_Rdata_all <-
  function(fromFolder = fromFolderA,
           toFolder = toFolderA) {
    # browser()
    if (!dir.exists(toFolder)) {
      dir.create(toFolder, showWarnings = FALSE)
    }
    
    # Raw data
    dataToLoad <- listCTdata(fromFolder)
    loadedData <- listCTdata(toFolder, "*.Rdata$")
    
    # Loading data that has changes
    if (is.null(loadedData)) {
      loadedData <-
        dataToLoad %>%
        mutate(publicationDate = publicationDate - 1)
    }
    
    dataToLoad <- 
      left_join(dataToLoad,
              loadedData,
              by = c("r", "px", "ps", "freq", "year", "month")) %>% 
      mutate(reload = publicationDate.x > publicationDate.y,
             reload = ifelse(is.na(publicationDate.y), TRUE, reload),
             name.y = ifelse(is.na(publicationDate.y), name.x, name.y),
             name.y = ifelse(str_detect(name.x, "\\.zip$"), 
                             str_replace(name.x, "\\.zip$", ".Rdata"), 
                             name.y),
             month = ifelse(freq == "MONTHLY", as.integer(month), month),
             reload = ifelse(freq == "MONTHLY" & month > 12, FALSE, reload)) %>%
      filter(reload) 
    
    if(nrow(dataToLoad) > 0) {
      dataToLoad %>% 
        select(name.x, name.y, r, px, freq, ps) %>%
        rowwise() %>%
        do(resave_ct_to_Rdata(., fromFolder = fromFolder, toFolder = toFolder))
      
      message(str_c(
        nrow(dataToLoad), 
        " data files were reloaded into the folder ", 
        toFolder))
    } else {
      message("No new data files were reloaded")
    }

  }
