#' Aggregate mounthly data by commodities into years.

agg_month_to_years <-
  function(data,
           grouping_vars =
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
               "Qty.Unit.Code",
               "Trade.Flow",
               "Type"
             ),
           valuse_Vars = 
             c(
               "Qty",
               "Netweight..kg.",
               "Trade.Value..US..",
               "Value"
             ),
           drop_vars = 
             c(
               "Qty.Unit.Code",
               "Flag"
             )) {
    
    valuse_Vars <-
      names(data)[names(data) %in% valuse_Vars]
    
    if(length(names(data)[names(data) %in% drop_vars]) > 0) {
      data <- 
        data%>% 
        select_(.dots = stringr::str_c("-", names(data)[names(data) %in% drop_vars]))
    }
    
    # browser()
    data %>%
      mutate(Period = as.character(Year))  %>% 
      group_by_(.dots = names(.)[names(.) %in% grouping_vars]) %>% 
      summarise_at(.vars = valuse_Vars, sum, na.rm = TRUE) %>% 
      ungroup()
  }