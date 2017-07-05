#' List all files with the COMTRADE data in a folder
#'
listCTdata <-
  function (folder, pattern = "*.zip")
  {
    require(tidyverse)
    require(stringr)
    if (length(list.files(path = folder, pattern = pattern)) > 0) {
      filesList <-
        tibble(name = list.files(path = folder, pattern = pattern)) %>%
        mutate(
          r = str_extract(name, "(?<=_r-)(.*?)(?=_ps-)"),
          px = str_extract(name, "(?<=_px-)(.*?)(?=_pub-)"),
          ps = str_extract(name, "(?<=_ps-)(.*?)(?=_freq-)"),
          freq = str_extract(name, "(?<=_freq-)(.*?)(?=_px-)"),
          freq = ifelse(freq == "A", "ANNUAL", "MONTHLY"),
          year = str_sub(ps, 1, 4),
          month = str_sub(ps, 5, 6),
          month = ifelse(month == "" &
                           freq == "ANNUAL", "AN", month),
          publicationDate = str_extract(name, "(?<=_pub-)(.*?)(?=_fmt-)"),
          publicationDate = str_c(
            str_sub(publicationDate, 1, 4),
            str_sub(publicationDate, 5, 6),
            str_sub(publicationDate, 7, 8),
            sep = "-"
          ) %>%
            as.Date(),
          extractDate = str_extract(name, pattern = "(?<=_ex-)(.*?)(?=\\.)"),
          extractDate = str_c(
            str_sub(extractDate, 1, 4),
            str_sub(extractDate, 5, 6),
            str_sub(extractDate, 7, 8),
            sep = "-"
          ) %>%
            as.Date(),
          filesize = file.size(file.path(folder, name)),
          downloadDate = file.info(file.path(folder, name))$ctime %>%
            str_sub(1, 10) %>%
            as.Date()
        )
    } else {
      warning(str_c("No CT data files in the folder ", folder))
      filesList <- NULL
    }
    return(filesList)
  }
