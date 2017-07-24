#' Runking top N partners
rank_agg_top_partners <-
  function(data,
           top_n,
           agg = TRUE,
           oneEU = TRUE,
           oneFSR = TRUE,
           otherEU = TRUE,
           otherFSR = TRUE,
           sepRUS = FALSE,
           topPeriod = 5) {
    top_n <- as.integer(top_n)
    
    # Technical variables
    groupVars <- get_ct_group_vars()
    
    # Partners lists
    fsr.Partners <- getFSR()$Partner.Code
    eu.Partners <- getEU()$Partner.Code
    rus.Partner <- 643L
    
    # Introduce in the CT countries mapping table new country codes
    euCode <- 97L
    fsrCode <- 889L
    otherEUCode <- 7492L
    otherFSRCode <- 7889L
    rowCode <- 888L
    
    # Group periods if necessery
    if (topPeriod == 0)
      topPeriod <- 1000
    
    data <-
      data %>% 
      
      # Cleaning time frame dependso on the topPeriod specification
      dplyr::filter(Year > max(Year) - topPeriod) %>%
      dplyr::select_(.dots = stringr::str_c("-", names(.)[names(.) %in% c("Year", "Period")])) %>%
      dplyr::group_by_(.dots = get_ct_group_vars(names(.), excl = c("Classification", "Type"))) %>%
      dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>% 
      dplyr::mutate(Partner.Top = Partner.Code) %>%
      
      # If any selected cases has to be shown - one EU/FSR/RUS case
      dplyr::mutate(Partner.Top = ifelse(oneEU &
                                           (Partner.Top %in% eu.Partners), euCode, Partner.Top)) %>%
      dplyr::mutate(Partner.Top = ifelse(oneFSR &
                                           (Partner.Top %in% fsr.Partners), fsrCode, Partner.Top)) %>%
      dplyr::mutate(Partner.Top = ifelse(sepRUS &
                                           (Partner.Top %in% rus.Partner), rus.Partner, Partner.Top)) %>%
      
      # Calculating sum for each group in the defined range of years for summing
      dplyr::group_by_(.dots = get_ct_group_vars(
        names(.),
        excl = c("Year", "Period", "Classification", "Partner.Code", "Partner", "Type"),
        include = c("Partner.Top")
      )) %>%
      dplyr::mutate(Value.Sum = sum(Value, na.rm = TRUE)) %>%
      
      # Calculating the rank
      dplyr::group_by_(.dots = get_ct_group_vars(
        names(.),
        excl = c(
          "Year",
          "Period",
          "Classification",
          "Partner.Code", 
          "Partner",
          "Partner.Top",
          "Type"
        )
      )) %>%
      dplyr::mutate(Rank = dense_rank(desc(Value.Sum))) %>% arrange(Rank) %>%
      dplyr::select(-Value.Sum) %>%
      dplyr::ungroup() %>% 
      dplyr::mutate(Partner.Top = ifelse(Rank <= top_n, Partner.Top, rowCode)) %>% 
      
      # EU Other if applicable
      dplyr::mutate(Partner.Top = ifelse(
        Rank > top_n & otherEU & !oneEU & (Partner.Code %in% eu.Partners),
        otherEUCode,
        Partner.Top
      )) %>% 
      
      # FSR Other if applicable
      dplyr::mutate(Partner.Top = ifelse(
        Rank > top_n & otherEU & !oneEU & (Partner.Code %in% fsr.Partners),
        otherFSRCode,
        Partner.Top
      )) %>%
      
      # Recalculating ranks again
      dplyr::group_by_(.dots =
                         get_ct_group_vars(
                           names(.),
                           excl = c("Year", "Period", "Classification", "Partner.Code"),
                           include = c("Partner.Top")
                         )) %>%
      dplyr::mutate(Value.Sum = sum(Value, na.rm = TRUE)) %>%
      dplyr::group_by_(.dots = get_ct_group_vars(
        names(.),
        excl = c(
          "Year",
          "Period",
          "Classification",
          "Partner.Code",
          "Partner.Top"
        )
      )) %>%
      dplyr::mutate(Rank = dense_rank(desc(Value.Sum)) - 1L) %>%
      dplyr::arrange(Rank) %>%
      dplyr::select(-Value.Sum) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        Reporter.Code,
        Trade.Flow.Code,
        Partner.Code,
        Commodity.Code,
        Variable,
        Partner.Top,
        Rank
      ) %>%
      
      # Combining original data
      dplyr::right_join(data, names(data)[names(data) %in% c("Trade.Flow.Code",
                                                             "Reporter.Code",
                                                             "Partner.Code",
                                                             "Commodity.Code",
                                                             "Variable")]) 
    
    # Sometimes, in the selected timeframe, there is no information about some countries.
    #   We aggregated them under ROW classification with the same rank.
    rowRank <- data %>% filter(., !is.na(Rank), Partner.Top == rowCode) %>% .$Rank %>% unique()
    data <- 
      data %>% 
      dplyr::mutate(Partner.Top = ifelse(is.na(Partner.Top), rowCode, Partner.Top),
                    Rank = ifelse(is.na(Rank), rowRank, Rank))
    
    if (agg) {
      data <-
        data %>%
        dplyr::mutate(Partner.Code = Partner.Top) %>%
        dplyr::group_by_(.dots = get_ct_group_vars(names(.), include = c("Rank"))) %>%
        dplyr::summarise(Value = sum(Value)) %>%
        dplyr::ungroup()
    }
    
    return(data)
    
  }
