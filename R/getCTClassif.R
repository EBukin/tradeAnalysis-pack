#' Get the COMTRADE classification (hs codes with names)
getCTClass <- 
  function() {
    require(plyr)
    require(dplyr)
    
    ctClassif <-
      data.frame(
        jsonlite::fromJSON("http://comtrade.un.org/data/cache/classificationHS.json")$results, 
        stringsAsFactors = FALSE) %>% 
      dplyr::tbl_df()
    
    ctClassif %>% 
      dplyr::rename(Commodity.Code = id,
                    Commodity = text) %>% 
      dplyr::select(-parent)
    
  }