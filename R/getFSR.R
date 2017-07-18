#' Get list of former soviet republics in the CT countries codes
getFSR <- 
  function() {
    require(dplyr)
  part %>% 
    filter(Partner.Code %in% 
             c(112, 643, 498, 804, 268, 51, 31, 398, 417, 762, 860, 795))
  }