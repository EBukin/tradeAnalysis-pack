#' Function for updating zip archives with tade data in a specific folder
#'
#' @param repo List of the reporters, we want to focus on. If it is specified 
#'    "ALL", only files with data for all countries will be checked. If specified
#'    NA or NULL, only  
updateCTData <-
  function(ctAval, folder, period = "MONTHLY", repo = NA) {
    
    localData <- listCTdata(folder)
    
    # Cleaning available data
    ctCurData <-
      ctAval %>%
      mutate(
        year = str_sub(ps, start = 1, end = 4),
        month = stringr::str_sub(ps, start = 5, end = 6),
        month = ifelse(month == "" & freq == "ANNUAL", "AN", month),
        publicationDate = as.Date(str_sub(
          publicationDate, start = 1, end = 10
        )),
        extractDate = as.Date(str_sub(
          extractDate, start = 1, end = 10
        ))
      ) %>%
      filter(px == "HS") %>%
      select(-type) %>%
      arrange(r, year) %>%
      filter(freq %in% period)
    
    if (any(!is.na(repo))) {
      ctCurData <- ctCurData %>% filter(r %in% repo)
    } else {
      ctCurData <- ctCurData %>% filter(!(r %in% "ALL"))
    }
    
    toDownload <-
      ctCurData %>%
      left_join(localData,
                by = c("r", "year", "month", "px", "freq", "ps")) %>%
      bind_rows(
        localData %>%
          anti_join(ctCurData,
                    by = c("r", "year", "month")) %>%
          rename(
            name.y = name,
            publicationDate.y = publicationDate,
            extractDate.y = extractDate,
            filesize.y = filesize
          )
      ) %>%
      mutate(
        new_data = is.na(publicationDate.y),
        updated_data = publicationDate.x > publicationDate.y,
        updated_data = 
          ifelse(publicationDate.x == publicationDate.y & filesize.x != filesize.y, 
                 TRUE, 
                 updated_data),
        not_available = is.na(publicationDate.x)
      )
    
    if (nrow(toDownload %>% filter(new_data)) > 0) {
      print(paste0(
        "New files to download - ",
        nrow(filter(toDownload, new_data)),
        ", total size ",
        round(sum(
          filter(toDownload, new_data)$filesize.x
        ) / 1024 ^ 3, 3),
        " GB."
      ))
    }
    
    if (nrow(toDownload %>% filter(updated_data)) > 0) {
      print(paste0(
        "Existing files to uptade - ",
        nrow(filter(toDownload, updated_data)),
        ", total size ",
        round(sum(
          as.numeric(filter(toDownload, updated_data)$filesize.x), na.rm = TRUE
        ) / 1024 ^ 3, 3),
        " GB."
      ))
    }
    
      
    if (nrow(toDownload %>% filter(new_data)) > 0) {

      d_ply(toDownload %>% filter(new_data),
            "name.x",
            function(x) {
              newDF <-
                x %>%
                select(name.x, filesize.x, downloadUri) %>%
                rename(name = name.x, filesize = filesize.x)
              try(downloadCTZIP(df = newDF, folder = folder))
            },
            .progress = "text")
    } else {
      message("No new data to download")
    }
    

    if (nrow(toDownload %>% filter(updated_data)) > 0) {

      d_ply(toDownload %>% filter(updated_data),
            "name.x",
            function(x) {
              oldName <- x$name.y
              newDF <-
                x %>%
                select(name.x, filesize.x, downloadUri) %>%
                rename(name = name.x, filesize = filesize.x)
              
              moveToOld(file_name = oldName, folder = folder)
              downloadCTZIP(df = newDF, folder = folder)
              
            },
            .progress = "text")
    } else {
      message("No new data to update old data")
    }
    
    # Writing all existing data the Index.csv file
    localData <- listCTdata(folder)
    write.csv(localData, str_c(folder, "index.csv"), row.names = FALSE)
    
  }
