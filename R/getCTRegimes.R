#' Get list of CT regimes
getCTRegimes <-
  function() {
    require(plyr)
    require(dplyr)
    
    ctRegimes <-
      data.frame(
        jsonlite::fromJSON("http://comtrade.un.org/data/cache/tradeRegimes.json")$results,
        stringsAsFactors = FALSE
      ) %>%
      dplyr::tbl_df()
    
    ctRegimes %>%
      dplyr::rename(Trade.Flow.Code = id,
                    Trade.Flow = text) %>%
      dplyr::mutate(Trade.Flow.Code = as.integer(Trade.Flow.Code))
    
    
  }