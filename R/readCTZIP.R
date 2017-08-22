#' Reading one CT bulk ziped data file
#' 
#' @param file file name with extension, for example "data.zip" or "DaTa.ZIp" (case sencitive).
#' @param folder path to the folder where there is a file, for example "./data/".
#' @param filter_hs_list list of the HS codes that should be supseted from the original data file.
#' @param delete if TRUE, the temporary unzipped data file will be deleted.
readCTZIP <-
  function(file, folder, filter_hs_list = NULL, delete = TRUE, ...) {
    from <- folder
    to <- file.path(from,"temp")
    
    # unzipping all files
    try(unzip(zipfile = file.path(from, file), exdir = to))
    csv_file <- file.path(to, unzip(zipfile = file.path(from, file), exdir = to, list = TRUE)$Name)
    
    ctdf <- readCTCSV(csv_file)
    
    if (!is.null(filter_hs_list)) {
      ctdf <-
        ctdf %>%
        filter(Commodity.Code %in% filter_hs_list)
    }
    
    gc()
    
    if (delete)
      file.remove(csv_file)
    
    ctdf
    
  }