#' Compare to sets of CT data files in two different folders and reload outdated files.
flt_data_to_reload <- function(fromFolder,
                               toFolder,
                               newPrefix = "",
                               fromExt = ".Rdata$",
                               toExt = ".Rdata$") {
  
  # Raw data
  dataToLoad <- listCTdata(fromFolder, fromExt)
  loadedData <- listCTdata(toFolder, toExt)
  
  # Loading data that has changes
  if (is.null(loadedData)) {
    loadedData <-
      dataToLoad %>%
      mutate(publicationDate = publicationDate - 1)
  }
  # browser()
  full_join(dataToLoad,
            loadedData,
            by = c("r", "px", "ps", "freq", "year", "month")) %>% 
    mutate(reload = publicationDate.x > publicationDate.y,
           reload = ifelse(is.na(publicationDate.y), TRUE, reload),
           name.z = ifelse(reload, name.y, NA),
           name.y = ifelse(is.na(publicationDate.y), name.x, name.y),
           name.y = ifelse(str_detect(name.x, fromExt), 
                           str_replace(name.x, fromExt, ".Rdata"), 
                           name.y),
           name.y =  ifelse(!is.na(name.y), str_c(newPrefix, name.y), name.y),
           month = ifelse(freq == "MONTHLY", as.integer(month), month),
           reload = ifelse(freq == "MONTHLY" & month > 12, FALSE, reload)) %>%
    rename(originNewFile = name.x,
           destOldFile = name.z,
           destNewFile = name.y) %>% 
    select(originNewFile, destOldFile, destNewFile, reload, r, px, ps, freq, year, month) %>% 
    filter(reload) 
  
}
