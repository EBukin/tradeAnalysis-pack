#' Aggregate mounthly data by commodities into years.

agg_month_to_years <-
  function(data,
           grouping_vars =
             c(
               "Classification",
               "Year",
               "Trade.Flow.Code",
               "Reporter.Code",
               "Partner.Code",
               "Commodity.Code",
               "Variable",
               "Source",
               "Reporter",
               "Partner",
               "Qty.Unit.Code",
               "Trade.Flow"
             ),
           valuse_Vars = 
             c(
               "Qty",
               "Netweight..kg.",
               "Trade.Value..US.."
             ),
           drop_vars = 
             c(
               "Qty.Unit.Code",
               "Flag"
             )) {
    
    # browser()
    data %>%
      mutate(Period = Year) %>% 
      select_(.dots = stringr::str_c("-", drop_vars)) %>% 
      group_by_(.dots = names(.)[names(.) %in% grouping_vars]) %>% 
      summarise_at(.cols = valuse_Vars, sum, na.rm = TRUE) %>% 
      ungroup()
  }