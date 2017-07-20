#' Set of filterring function for CT data availability lically and online
#' 
flt_rep_ct <- function(x, reps) {
  if (all(!is.na(reps))) {
    if ("r" %in% names(x)) {
      x <-
        x %>%
        dplyr::filter(r %in% reps)
    }
    
    if ("Reporter.Code" %in% names(x)) {
      x <-
        x %>%
        dplyr::filter(Reporter.Code %in% reps)
    }
  }
  x
}


flt_par_ct <- function(x, part) {
  if (any(!is.na(part))) {
    if ("Partner.Code" %in% names(x)) {
      x <-
        x %>%
        dplyr::filter(Partner.Code %in% part)
    }
  }
  x
}

flt_clas_ct <- function(x, clas = "HS") {
  if (all(!is.na(clas))) {
    x <-
      x %>%
      dplyr::filter(px %in% clas)
  }
  x
}

flt_type_ct <- function(x, types) {
  if (all(!is.na(types))) {
    x <-
      x %>%
      dplyr::filter(type %in% types)
  }
  x
}

flt_freq_ct <- function(x, freqs) {
  if (all(!is.na(freqs))) {
    x <-
      x %>%
      dplyr::filter(freq %in% freqs)
  }
  x
}

flt_year_ct <- function(x, years) {
  if(all(!is.na(years))) {
    x <- 
      x %>% 
      dplyr::filter(year %in% years)
  }
  x
}

flt_default_ct <-
  function(x,
           classification = "HS",
           types = "COMMODITIES",
           frequency = "ANNUAL",
           reporters = NA,
           period = NA) {
    
    flt_clas_ct(x, classification) %>% 
      flt_type_ct(x, types) %>% 
      flt_freq_ct(x, frequency) %>% 
      flt_rep_ct(x, reporters) %>%
      flt_year_ct(x, period)
  }

flt_com_ct <- function(x, commodity) {
  if(all(!is.na(commodity))) {
    x <- 
      x %>% 
      dplyr::filter(Commodity.Code %in% commodity)
  }
  x
}

