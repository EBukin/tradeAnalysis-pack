#' Aggregating trade items using provided mapping table
agg_commodities <-
  function(data,
           mt = "data/HS_agg_mt.csv",
           onlyAggregates = TRUE) {
    require(dplyr)
    
    if (any(names(data) %in% c("Partner.Top", "Rank")))
      message(str_c("Columns as ",
                    names(data)[names(data) %in% c("Partner.Top", "Rank")],
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
        "Source"
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
      select_(.dots = names(.)[!names(.) %in% c("Partner.Top", "Rank", "Commodity.Agg")]) %>%
      group_by_(.dots = names(.)[names(.) %in% grouping_vars]) %>%
      summarise_each(funs(sum(., na.rm = TRUE))) %>%
      ungroup()
    
    if (onlyAggregates) {
      return(dataAgg)
    } else {
      return(bind_rows(data %>%
                         select_(.dots = names(.)[!names(.) %in% c("Partner.Top", "Rank", "Commodity.Agg")]),
                       dataAgg))
    }
    
  }