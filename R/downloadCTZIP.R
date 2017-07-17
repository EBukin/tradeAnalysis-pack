#' Download a CT zip archive with data from the website
#'
#' @param df A list with the attributes `$name`, `$filesize` and `df$downloadUri`
#'    which are derived from the datframe with the CT countries.
#' @param toFolder Directory, where the dowloaded files will be saved.
#' @param iteration Parametr for recursive use of the funcition for making sure
#'    that one file is not attempted o be downloaded more than 5 times.
downloadCTZIP <-
  function(df,
           toFolder,
           base_url = "https://comtrade.un.org",
           iteration = 1,
           maxIterations = 3,
           token) {
    require(stringr)
    
    handle <-
      RCurl::getCurlHandle(
        useragent = str_c(R.version$platform, R.version$version.string, sep = ","),
        httpheader = c(from = "fao@fao.org")
      )
    
    suppressWarnings(dir.create(toFolder))
    
    file <- str_c(toFolder, "/", df$name)
    
    if (!file.exists(file) | file.size(file) != df$filesize) {
      if (!is.na(token)) {
        url = stringr::str_c(base_url, df$downloadUri, "?token=", token)
      } else {
        url = stringr::str_c(base_url, df$downloadUri)
      }
      
      tmpFile <- try(RCurl::getBinaryURL(url, curl = handle), silent = TRUE)
      try(writeBin(tmpFile, file), silent = TRUE)
      Sys.sleep(0.2)
    }
    
    if (file.exists(file) &
        file.size(file) != df$filesize &
        iteration < maxIterations) {
      downloadCTZIP(
        df,
        toFolder,
        iteration = iteration + 1,
        maxIterations = maxIterations,
        token = token
      )
    } else {
      moveToOld(df$name, toFolder)
    }
    
  }
