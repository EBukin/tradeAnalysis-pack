# Function for calculating shares
calc_share_in <- 
  function(df, shareBaseCommodity.Code = "TOTAL") {
    dfVares <- c("Classification", "Year", "Period", "Trade.Flow.Code", "Reporter.Code", "Partner.Code", "Variable")
    
    df %>% 
      left_join(., 
                select_(., .dots = c(dfVares, "Commodity.Code", "Value")) %>% 
                  filter(Commodity.Code == shareBaseCommodity.Code) %>% 
                  spread(Commodity.Code, Value), 
                by = names(.)[names(.) %in% dfVares]) %>% 
      mutate_(.dots = setNames(str_c("Value / ", shareBaseCommodity.Code), str_c("Share_in_", shareBaseCommodity.Code))) %>%
      mutate_(.dots = setNames(str_c( "ifelse(", str_c("Share_in_", shareBaseCommodity.Code), ">1, NA,", str_c("Share_in_", shareBaseCommodity.Code), ")"), str_c("Share_in_", shareBaseCommodity.Code))) %>%
      select_(.dots = str_c("-", shareBaseCommodity.Code)) %>% 
      return()
  }