#' Get a list of EU countries
getEU <-
  function() {
    require(dplyr)
    partners %>% 
      filter(Partner.Code %in% 
               c(20, 40, 56, 58, 100, 191, 196, 203, 208, 
                 233, 234, 246, 251, 276, 300, 336, 348, 
                 352, 372, 381, 428, 440, 442, 470, 528, 
                 574, 620, 642, 638, 674, 703, 705, 724, 
                 752, 826, 616))
  }