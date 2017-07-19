#' Filter WTO Commodity Codes based on the mapping tables embeded inot the package.
flt_wto_com <- 
  function(data,
           wtoShortMT = wtoAgFood) {
    
    # Logic:
    # Split wtoAgFood into exclkusive parts:
    # Unique 6 didgits components of the WTO Agfood.
    # Unique 4 didgits components of the WTO AgFood and all 6 didgits subcomponents.
    # Unique 2 didgits components of the WTO AgFood and all 4 and 6 didgits subcomponents.
    
    require(tradeAnalysis)
    
    wtoShortMT[wtoShortMT == ""] <- NA
    
    wtoAgFoodClean <-
      wtoShortMT %>% 
      select(cc1, cc2, cc3) %>% 
      mutate(groups = ifelse(!is.na(cc1) & !is.na(cc2) & !is.na(cc3), 1L, NA),
             groups = ifelse(!is.na(cc1) & !is.na(cc2) &  is.na(cc3), 2L, groups),
             groups = ifelse(!is.na(cc1) &  is.na(cc2) &  is.na(cc3), 3L, groups))
    
    data <- 
      data %>% 
      mutate(cc1 = str_sub(Commodity.Code, 1,2),
             cc2 = str_sub(Commodity.Code, 3,4),
             cc3 = str_sub(Commodity.Code, 5,6)) 
    
    wtoAgFoodClean %>% 
      group_by(groups) %>% 
      do({
        mt <-
          ungroup(.) %>% 
          select(-groups) 
        mt <-
          mt[!map(mt,is.na) %>% 
               map_lgl(all)]
        mt %>% 
          left_join(data, by = names(mt))
      }) %>% 
      ungroup()
  }
