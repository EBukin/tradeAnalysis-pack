#' Aggregate all CT reporters
#' 
#' 
agg_reporters <-
  function(data,
           RegionsType = "Income",
           mt = system.file("extdata", str_c(mtType, "_regions.csv"), package = "tradeAnalysis"),
           mtType = "ct",
           valCol = "Value",
           dropCols = NULL,
           onlyAggregates = TRUE) {
    
    # All available names
    nonAggName <- c("Reporter", "Partner", "areacode", "areaname", "Classification", "Type", dropCols)
    dataGroupName <- names(data)[!names(data) %in% c(nonAggName, valCol)]
    dropName <- names(data)[names(data) %in% nonAggName]
    
    if(length(dropName) != 0) {
      message(str_c("Variable(s) ", str_c(dropName, collapse = ", ")," are dropped during aggregation.") )
    }
    
    # Maping table
    regionsMT <-
      read.csv(mt, stringsAsFactors = FALSE) %>% 
      tbl_df() %>% 
      select(Code, contains(RegionsType, ignore.case = TRUE), -contains("source"))
    names(regionsMT)[2] <- "Region"
    
    dataAgg <- 
      data %>% 
      left_join(regionsMT, by = c("Reporter.Code" = "Code")) %>% 
      select(-Reporter.Code) %>% 
      rename(Reporter.Code = Region) %>% 
      group_by_(.dots = dataGroupName) %>% 
      summarise_(.dots = setNames(list(lazyeval::interp(~ sum(.x), .x = as.name(valCol))), valCol)) %>% 
      ungroup()
    
    if(!onlyAggregates) {
      dataAgg <- 
        dataAgg %>% 
        mutate(Reporter.Code = as.character(Reporter.Code)) %>% 
        bind_rows(data)
    }
    
    return(dataAgg)
    
  }
