#' Get list of CT partners
getCTPartners <- 
  function() {
    require(plyr)
    require(dplyr)
    
    ct_partners <-
      data.frame(
        jsonlite::fromJSON("http://comtrade.un.org/data/cache/partnerAreas.json")$results, 
        stringsAsFactors = FALSE) %>% 
      dplyr::tbl_df()
    
    ct_partners %>% 
      dplyr::rename(Partner.Code = id,
             Partner = text) %>% 
      dplyr::filter(Partner != "All") %>% 
      dplyr::mutate(Partner.Code = as.integer(Partner.Code))
    
  }