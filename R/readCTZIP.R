#' Reading one CT bulk ziped data file
readCTZIP <-
  function(file, folder, filter_hs_list = NULL, delete = TRUE, ...) {
    require(stringr)
    from <- folder
    to <- str_c(from,"/temp")
    
    # unzipping all files
    try(unzip(zipfile = str_c(from, "/", file), exdir = to))
    csv_file <- str_c(to, "/", unzip(zipfile = str_c(from, "/", file), exdir = to, list = TRUE)$Name)
    
    ctdf <- readCTCSV(csv_file)
    
    if(!is.null(filter_hs_list)) {
      ctdf <- 
        ctdf %>%
        filter(Commodity.Code %in% filter_hs_list)
    }
    
    gc()
    
    if(delete) file.remove(csv_file)
    
    ctdf
    
  }