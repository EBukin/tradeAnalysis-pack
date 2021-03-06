#' Get availability of annual and monthly data
#' 
#' @param annualData,monthlyData CT normalized data frame with annual and monthly data
#' @param reps list of reporters.
#' 
get_ct_avail <-
  function(ctData) {
    available_wto_an_agg <-
      ctData %>%
      dplyr::filter(Type == "Direct") %>%
      sel_dist(Period, Reporter.Code, Type)
    
    expand.grid(
      Period = unique(available_wto_an_agg$Period),
      Reporter.Code = unique(available_wto_an_agg$Reporter.Code),
      stringsAsFactors = FALSE
    ) %>%
      tbl_df() %>%
      dplyr::left_join(available_wto_an_agg, by = c("Reporter.Code", "Period")) %>%
      dplyr::mutate(Type = ifelse(is.na(Type), "Mirror", Type)) %>%
      join_lables() %>% 
      split_ct_period() %>% 
      select(-Period)
  }

get_combined_ct_avail <-
  function(annualData, monthlyData, reps = reporters$Reporter.Code) {
    anAv <- get_ct_avail(flt_rep_ct(annualData, reps)) %>%
      dplyr::filter(Type == "Direct")
    mnAv <- get_ct_avail(flt_rep_ct(monthlyData, reps)) %>%
      dplyr::filter(Type == "Direct")
    
    if(is.na(reps)) reps <- reporters$Reporter.Code
    
    mnAvSum <-
      mnAv %>%
      sel_dist(Year, Month, Reporter.Code, Type) %>%
      dplyr::group_by(Reporter.Code, Year) %>%
      dplyr::summarise(avDirectDataMonth = n()) %>% 
      dplyr::ungroup()
    
    expand.grid(
      Year = unique(c(mnAvSum$Year, anAv$Year)),
      Reporter.Code = reps,
      stringsAsFactors = FALSE
    ) %>%
      mutate(Reporter.Code = as.integer(Reporter.Code)) %>% 
      tbl_df()  %>% 
      dplyr::left_join(select(anAv, -Month), by = c("Reporter.Code", "Year")) %>%  
      dplyr::left_join(mnAvSum, by = c("Reporter.Code", "Year")) %>% 
      dplyr::mutate(Type = ifelse(is.na(Type) & avDirectDataMonth >= 11, str_c("Monthly ", avDirectDataMonth), Type),
                    Type = ifelse(is.na(Type) & avDirectDataMonth < 11, str_c("Mir.-month.", avDirectDataMonth), Type),
                    Type = ifelse(is.na(Type) & is.na(avDirectDataMonth), "Mirror", Type)) %>% 
      join_lables() %>% 
      select(-avDirectDataMonth)
  }

# get_an_mn_avail <- 
#   function(annualData = NULL, monthlyData = NULL, reps = getCTReporters()$Reporter.Code) {
#     
#     if(!is.null(annualData)) {
#       # Looking at the data availability:
#       available_wto_an_agg <- 
#         annualData %>% 
#         dplyr::filter(Type == "Direct") %>% 
#         sel_dist(Period, Reporter.Code, Type)  %>%
#         split_ct_period() %>% 
#         dplyr::select(-Period, -Month)
#     } else {
#       available_wto_an_agg <- tibble(Reporter.Code = 0L, Year = 0L, Type = NA)
#     }
#     
#     if(!is.null(monthlyData)) {
#       # Montly data availability
#       available_wto_mn_agg <-
#         monthlyData %>%
#         dplyr::filter(Type == "Direct") %>%
#         sel_dist(Period, Reporter.Code, Type) %>%
#         split_ct_period() %>% 
#         dplyr::select(-Period) %>%
#         dplyr::group_by(Reporter.Code, Year) %>%
#         dplyr::summarise(avDirectDataMonth = n()) %>% 
#         dplyr::ungroup()
#     } else {
#       available_wto_mn_agg <- tibble(Reporter.Code = 0L, Year = 0L, avDirectDataMonth = NA)
#     }
#     
#     # Desired list of countries to build plots
#     plots_list <-
#       reps %>% 
#       map_df(function(x) tibble(Reporter.Code = x, 
#                                 Year = seq(min(available_wto_an_agg$Year),max(available_wto_an_agg$Year)))) %>%
#       ungroup()
#     
#     # Analyzing what data is available and what has to be completed.
#     dataAvailability <-
#       plots_list %>% 
#       dplyr::left_join(available_wto_an_agg, by = c("Reporter.Code", "Year")) %>%  
#       dplyr::left_join(available_wto_mn_agg, by = c("Reporter.Code", "Year")) %>% 
#       dplyr::mutate(Type = ifelse(is.na(Type) & avDirectDataMonth >= 11, str_c("Monthly ", avDirectDataMonth), Type),
#                     Type = ifelse(is.na(Type) & avDirectDataMonth < 11, str_c("Mirror - monthly for ", avDirectDataMonth), Type),
#                     Type = ifelse(is.na(Type) & is.na(avDirectDataMonth), "Mirror", Type)) %>% 
#       join_lables() %>%
#       dplyr::select(-avDirectDataMonth) 
#     
#     dataAvailability
#   }
