#' Aggregating trade items using provided mapping table
agg_commodities <-
  function(data,
           mt = system.file("extdata", "HS_agg_mt.csv", package = "tradeAnalysis"),
           onlyAggregates = TRUE) {
    require(stringr)
    require(dplyr)
    
    if (any(names(data) %in% c("Partner.Top", "Rank", "Commodity")))
      message(str_c("Column(s) ",
                    names(data)[names(data) %in% c("Partner.Top", "Rank", "Commodity")],
                    " are dropped."))
    grouping_vars <-
      c(
        "Classification",
        "Year",
        "Period",
        "Trade.Flow.Code",
        "Reporter.Code",
        "Partner.Code",
        "Commodity.Code",
        "Variable",
        "Source",
        "Reporter",
        "Partner",
        "Trade.Flow"
      )
    
    mappingTable <-
      read.csv(mt, stringsAsFactors = FALSE, colClasses = "character") %>%
      tbl_df() %>%
      select(Commodity.Code, Commodity.Agg)
    
    dataAgg <-
      data %>%
      mutate(Commodity.Code = as.character(Commodity.Code)) %>%
      left_join(mappingTable, by = "Commodity.Code") %>%
      mutate(Commodity.Code = Commodity.Agg) %>% 
      filter(!is.na(Commodity.Code)) %>% 
      select_(.dots = names(.)[!names(.) %in% c("Partner.Top", "Rank", "Commodity.Agg", "Commodity")]) %>%
      group_by_(.dots = names(.)[names(.) %in% grouping_vars]) %>%
      summarise_each(funs(sum(., na.rm = TRUE))) %>%
      ungroup()
    
    if (onlyAggregates) {
      return(dataAgg)
    } else {
      return(bind_rows(data %>%
                         select_(.dots = names(.)[!names(.) %in% c("Partner.Top", "Rank", "Commodity.Agg", "Commodity")]),
                       dataAgg))
    }
    
  }