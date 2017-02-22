#' Download a CT zip archive with data from the website
#'
#' @param df A list with the attributes `$name`, `$filesize` and `df$downloadUri`
#'    which are derived from the datframe with the CT countries.
#' @param folder Directory, where the dowloaded files will be saved.
#' @param iteration Parametr for recursive use of the funcition for making sure
#'    that one file is not attempted o be downloaded more than 5 times.
# downloadCTZIP <-
#   function(df,
#            folder = "data_raw/ct_zip" ,
#            iteration = 1) {
#     require(stringr)
#
#     suppressWarnings(dir.create(folder))
#
#     file <- str_c(folder, "/", df$name)
#
#     if (!file.exists(file) | file.size(file) != df$filesize) {
#       try(download.file(
#         df$downloadUri,
#         destfile = file,
#         mode = "wb",
#         quiet = TRUE
#       ),
#       silent = TRUE)
#
#       Sys.sleep(abs(rnorm(1, mean = 0.8, 0.2)) + 1)
#
#     }
#
#     if (file.exists(file) &
#         file.size(file) != df$filesize & iteration < 5) {
#       downloadCTZIP(df, folder, iteration = iteration + 1)
#
#     }
#
#   }
downloadCTZIP <-
  function(df,
           folder = "data_raw/ct_zip" ,
           base_url = "https://comtrade.un.org",
           iteration = 1) {
    require(stringr)
    require(RCurl)
    
    handle <-
      getCurlHandle(
        useragent = str_c(R.version$platform, R.version$version.string, sep = ","),
        httpheader = c(from = "fao@fao.org")
      )
    
    suppressWarnings(dir.create(folder))
    
    file <- str_c(folder, "/", df$name)
    
    if (!file.exists(file) | file.size(file) != df$filesize) {
      tmpFile <-
        try(getBinaryURL(url = paste0(base_url, df$downloadUri),
                         curl = handle), silent = TRUE)
      try(writeBin(tmpFile, file), silent = TRUE)
      Sys.sleep(0.2)
    }
    
    if (file.exists(file) &
        file.size(file) != df$filesize &
        iteration < 3) {
      downloadCTZIP(df, folder, iteration = iteration + 1)
    }
    
  }
