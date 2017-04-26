
#' Aggregate CT regions according to the mapping table
#' 
#' @param data normalized trade data frame
#' @param RegionType type of the region form the mapping tabel
#' @param mt Path to the mapping table
#' @param intraExtraOnly return only intra- and extra-regional trade
#' @param onlyAggregates return aonly ggregates or also source data
function(data, 
         RegionsType = "Income", 
         mt = "policyBrief/ct_regions.csv", 
         intraExtraOnly = TRUE,
         onlyAggregates = TRUE) {
  
  # All available names
  nonAggName <- c("Reporter", "Partner", "Classification")
  dataGroupName <- names(data)[!names(data) %in% c(nonAggName, "Value")]
  dropName <- names(data)[names(data) %in% nonAggName]
  if(length(dropName) != 0) {
    message(str_c("Variables ", str_c(dropName, collapse = ", ")," are dropped during aggregation.") )
  }
  
  # Maping table
  regionsMT <-
    read.csv(mt, stringsAsFactors = FALSE) %>% 
    tbl_df() %>% 
    select(Code, contains(RegionsType, ignore.case = TRUE), -contains("source"))
  names(regionsMT)[2] <- "Region"
  
  # Calculating intraand extra regional trade trade
  dataPreAgg <- 
    data %>% 
    left_join(regionsMT, by = c("Reporter.Code" = "Code")) 
  dataAgg <- 
    dataPreAgg %>% 
    select(-Reporter.Code) %>% 
    rename(Reporter.Code = Region) %>% 
    left_join(regionsMT, by = c("Partner.Code" = "Code")) %>% 
    select(-Partner.Code) %>% 
    rename(Partner.Code = Region) %>% 
    group_by_(.dots = dataGroupName) %>% 
    summarise(Value = sum(Value)) %>% 
    ungroup() %>% 
    mutate(Region = RegionsType)
  
  if (intraExtraOnly) {
    dataAgg <-
      dataAgg %>% 
      mutate(Partner.Code = ifelse(Reporter.Code == Partner.Code, "Intra-region trade", Partner.Code),
             Partner.Code = ifelse(!Partner.Code %in% c("0", "World", "Intra-region trade"), "Extra-region trade", Partner.Code)) %>% 
      group_by_(.dots = dataGroupName) %>% 
      summarise(Value = sum(Value)) %>% 
      ungroup() %>% 
      mutate(Region = RegionsType)
  }
  
  if(!onlyAggregates) {
    dataAgg <- 
      dataPreAgg %>% 
      mutate(Reporter.Code = as.character(Reporter.Code),
             Partner.Code = as.character(Partner.Code)) %>% 
      bind_rows(dataAgg)
  }
  
  return(dataAgg)
}

