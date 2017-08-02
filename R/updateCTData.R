#' Function for updating zip archives with tade data in a specific folder
#'
#' @param reporters List of the reporters, we want to focus on. If it is specified
#'    "ALL", only files with data for all countries will be checked. If specified
#'    NA or NULL, only
updateCTData <-
  function(toFolder,
           ctCurData = getCTParameters(),
           classification = "HS",
           types = "COMMODITIES",
           frequency,
           reporters = getCTReporters()$Reporter.Code,
           period = NA,
           token = NA,
           maxIterations = 3) {
    localData <- listCTdata(toFolder)
    
    if(is.null(localData)) {
      localData <- 
        ctAval[0,] %>% 
        select(-downloadUri)
    }
    
    toDownload <-
      # Comparing available data with existing
      left_join(ctCurData,
                localData,
                by = c("r", "year", "month", "px", "freq", "ps", "type")) %>%
      
      # Ading existing data that is not available now.
      bind_rows(
        anti_join(localData, ctCurData, by = c("r", "year", "month")) %>%
          rename(
            name.y = name,
            publicationDate.y = publicationDate,
            extractDate.y = extractDate,
            filesize.y = filesize
          )
      ) %>%
      
      # Comparing new and old data
      mutate(
        new_data = is.na(publicationDate.y),
        updated_data = publicationDate.x > publicationDate.y,
        updated_data =
          ifelse(
            publicationDate.x == publicationDate.y & filesize.x != filesize.y,
            TRUE,
            updated_data
          ),
        not_available = is.na(publicationDate.x)
      ) %>% 
      flt_clas_ct(clas = classification) %>% 
      flt_type_ct(types = types) %>% 
      flt_rep_ct(reps = reporters) %>% 
      flt_year_ct(years = period) %>% 
      flt_freq_ct(freqs = frequency)
    
    # Showing messages
    if (nrow(filter(toDownload, new_data)) > 0) {
      message(paste0(
        "New files to download - ",
        nrow(filter(toDownload, new_data)),
        ", total size ",
        round(sum(
          as.numeric(filter(toDownload, new_data)$filesize.x), na.rm = TRUE
        ) / 1024 ^ 3, 3),
        " GB."
      ))
    }
    
    if (nrow(filter(toDownload, updated_data)) > 0) {
      message(paste0(
        "Existing files to uptade - ",
        nrow(filter(toDownload, updated_data)),
        ", total size ",
        round(sum(
          as.numeric(filter(toDownload, updated_data)$filesize.x), na.rm = TRUE
        ) / 1024 ^ 3, 3),
        " GB."
      ))
    }
    
    # Dowloading new data
    if (nrow(toDownload %>% filter(new_data)) > 0) {
      message("Downloading new data. To stop, press Esc.")
      plyr::d_ply(filter(toDownload, new_data),
                  "name.x",
                  function(x) {
                    downloadCTZIP(
                      df = list(
                        name = x$name.x,
                        filesize = x$filesize.x,
                        downloadUri = x$downloadUri
                      ),
                      toFolder = toFolder,
                      token = token,
                      maxIterations = maxIterations
                    )
                  },
                  .progress = "text")
    } else {
      message("No new data to download")
    }
    
    # Updating old data
    if (nrow(filter(toDownload, updated_data)) > 0) {
      message("Updating existing data. To stop, press Esc.")
      plyr::d_ply(filter(toDownload, updated_data),
                  "name.x",
                  function(x) {
                    moveToOld(file_name = x$name.y, folder = toFolder)
                    downloadCTZIP(
                      df = list(
                        name = x$name.x,
                        filesize = x$filesize.x,
                        downloadUri = x$downloadUri
                      ),
                      toFolder = toFolder,
                      token = token,
                      maxIterations = maxIterations
                    )
                  },
                  .progress = "text")
      
    } else {
      message("No new data to update old data")
    }
    
    # Updating old data
    if (nrow(filter(toDownload, not_available)) > 0) {
      message("Moving to the 'NotAvailable' folder all no longer available files")
      plyr::d_ply(filter(toDownload, not_available),
                  "name.y",
                  function(x) {
                    moveToOld(file_name = x$name.y, folder = toFolder, oldName = "NotAvailable")
                  },
                  .progress = "text")
      
    } else {
      message("No not-available data to remove")
    }
    
    
    # # Writing all existing data into the Index.csv file
    # localData <- listCTdata(toFolder)
    # write.csv(localData, str_c(toFolder, "index.csv"), row.names = FALSE)
    
  }
