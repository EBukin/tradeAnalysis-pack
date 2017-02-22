#' Get list of CT reporters
getCTReporters <-
  function() {
    require(plyr)
    require(dplyr)
    
    
    ct_reporters <-
      data.frame(
        jsonlite::fromJSON("http://comtrade.un.org/data/cache/reporterAreas.json")$results
      ) %>%
      dplyr::tbl_df()
    
    ct_reporters %>%
      dplyr::rename(Reporter.Code = id,
                    Reporter = text) %>%
      dplyr::filter(Reporter != "All") %>%
      dplyr::mutate(Reporter.Code = as.integer(Reporter.Code))
      
      
  }