#' Simple functiont for mirroing all existing data.
get_mirror_values <- function(data,
                            importFlowCode = 1,
                            exportFlowCode = 2,
                            return.direct = TRUE) {
  
  require(dplyr)
  
  # Mirroring bilateral trade
  datam <-
    data %>%
    select_(.dots = str_c("-", names(.)[names(.) == "Classification"])) %>% 
    filter(Partner.Code != 0) %>%
    mutate(
      Type = "Mirror",
      Trade.Flow.Code =
        ifelse(
          Trade.Flow.Code == importFlowCode,
          exportFlowCode,
          ifelse(Trade.Flow.Code == exportFlowCode,
                 importFlowCode,
                 NA)
        ),
      Reporter.Code_m = Partner.Code,
      Partner.Code = Reporter.Code,
      Reporter.Code = Reporter.Code_m
    ) %>%
    select(-Reporter.Code_m)  %>% 
    agg_world(aggReps = F, aggPart = T, returnAll = T) %>%
    tbl_df()
  
  if (return.direct) {
    datam <-
      # Aggregating World for reporters
      bind_rows(
        datam,
        mutate(data %>%
                 select_(.dots = str_c("-", names(.)[names(.) == "Classification"])), Type = "Direct")) %>%
      tbl_df()
  } 
  
  return(datam)
  
}