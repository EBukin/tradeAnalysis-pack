#' Set of filterring function for CT data availability lically and online
#' 
flt_rep_ct <- function(x, reps) {
  x %>% 
    dplyr::filter(r %in% reps)
}

flt_clas_ct <- function(x, clas = "HS") {
  if(clas == 1) clas = "HS"
  x %>% 
    dplyr::filter(px %in% clas)
}

flt_type_ct <- function(x, types) {
  if(types == 1) types = "COMMODITIES"
  if(types == 2) types = "SERVICES"
  x %>% 
    dplyr::filter(type %in% types)
}

flt_freq_ct <- function(x, freqs) {
  if(freqs == 1) freqs = "ANNUAL"
  if(freqs == 2) freqs = "MONTHLY"
  x %>% 
    dplyr::filter(freq %in% freqs)
}

flt_year_ct <- function(x, years) {
  x %>% 
    dplyr::filter(year %in% years)
}

flt_default_ct <-
  function(x,
           classification = "HS",
           types = "COMMODITIES",
           frequency = "ANNUAL",
           reporters = NA,
           period = NA) {
    
    if (all(!is.na(classification))) {
      x <- flt_clas_ct(x, classification)
    }
    
    if (all(!is.na(types))) {
      x <- flt_type_ct(x, types = types)
    }
    if (all(!is.na(frequency))) {
      x <- flt_freq_ct(x, frequency)
    }
    
    if (all(!is.na(reporters))) {
      x <- flt_rep_ct(x, reporters)
    }
    
    
    if (all(!is.na(period))) {
      x <- flt_year_ct(x, period)
    }
    
    x
  }
