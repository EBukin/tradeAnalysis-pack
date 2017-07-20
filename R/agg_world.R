#' Aggregate the world in the CT data based on available numbers for both Partners and Reporters depends on specifications
#' 
#' 
agg_world <- function(data,
                      aggReps = TRUE,
                      aggPart = TRUE,
                      returnAll = TRUE) {
  aggregates <- data
  group_var <-
    c(
      "Partner",
      "Partner.Code",
      "Reporter",
      "Reporter.Code",
      "Year",
      "Trade.Flow.Code",
      "Commodity.Code",
      "Variable",
      "Type",
      "Trade.Flow",
      "Commodity",
      "Unit",
      "Qty.Unit.Code",
      "Unit.Description",
      "Period",
      "Classification"
    )
  
  if (aggPart) {
    if(any(names(data) %in% "Partner.Code")) 
      aggregates <- 
        aggregates %>% 
        filter(Partner.Code != 0) %>% 
        mutate(Partner.Code = 0L)
    
    if(any(names(data) %in% "Partner")) 
      aggregates <- mutate(aggregates, Partner = "World")
  }
  
  if (aggReps) {
    if(any(names(data) %in% "Reporter.Code")) 
      aggregates <- mutate(aggregates, Reporter.Code = 0L)
    
    if(any(names(data) %in% "Reporter")) 
      aggregates <- mutate(aggregates, Reporter = "World")
  }
  
  group_var <- names(data)[names(data) %in% group_var]
  
  aggregates <-  
    aggregates %>%
    group_by_(.dots = group_var) %>%
    summarise(Value = sum(Value, na.rm = TRUE)) %>% 
    ungroup()
  
  if(returnAll) {
    aggregates <- bind_rows(data, aggregates)
  }
  
  return(aggregates)
  
}
