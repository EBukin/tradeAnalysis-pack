#' List all files with the COMTRADE data in a folder
#' 
listCTdata <-
  function (folder, pattern = "*.zip")
  {
    require(dplyr)
    require(stringr)
    data.frame(name = list.files(path = folder, pattern = pattern),
               stringsAsFactors = FALSE) %>%
      tbl_df() %>%
      mutate(
        r = str_sub(
          str_extract(name, pattern = "(?:(_r-)).{1,}(?=(_ps))"), start = 4
        ),
        px = str_sub(
          str_extract(name, pattern = "(?:(_px-)).{1,}(?=(_pub-))"), start = 5
        ),
        ps = str_sub(
          str_extract(name, pattern = "(?:(_ps-)).{1,}(?=(_freq-))"), start = 5
        ),
        freq = str_sub(
          str_extract(name, pattern = "(?:(_freq-)).{1,}(?=(_px))"), start = 7
        ),
        freq = ifelse(freq == "A", "ANNUAL", "MONTHLY"),
        year = str_sub(ps, start = 1, end = 4),
        month = str_sub(ps, start = 5, end = 6),
        month = ifelse(month == "" &
                         freq == "ANNUAL", "AN", month),
        publicationDate = str_sub(
          str_extract(name, pattern = "(?:(_pub-)).{1,}(?=(_fmt))"), start = 6
        ),
        publicationDate = as.Date(str_c(
          str_sub(publicationDate, 1, 4), "-", str_sub(publicationDate, 5, 6), "-", str_sub(publicationDate, 7, 8)
        )),
        extractDate = str_sub(
          str_extract(name, pattern = "(?:(_ex-)).{1,}(?=(\\.))"), start = 5
        ),
        extractDate = as.Date(str_c(
          str_sub(extractDate, 1, 4), "-", str_sub(extractDate, 5, 6), "-", str_sub(extractDate, 7, 8)
        )),
        filesize = file.size(str_c(folder, "/", name)),
        downloadDate = file.info(str_c(folder, "/", name))$ctime %>% 
          str_sub(1,10) %>% 
          as.Date()

      )
  }
