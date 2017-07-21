#' Runking top N partners
rank_agg_top_partners <- function(df, top_n, agg = TRUE, oneEU = TRUE, oneFSR = TRUE, oneRUS = FALSE, otherEU = FALSE, topPeriod = 4) {
  # top_n <- 5
  top_n <- as.integer(top_n)
  
  groupVars <-
    c(
      "Classification",
      "Year",
      "Period",
      "Trade.Flow.Code",
      "Commodity.Code",
      "Variable",
      "Trade.Flow",
      "Commodity",
      "Unit",
      "Qty.Unit.Code",
      "Unit.Description",
      "Reporter",
      "Reporter.Code"
    )
  
  
  fsr.Partners <- getFSR()$Partner.Code
  
  # List of the EU countries must be carefully checked from the comtrade reporters/partners list
  eu.Partners <- c(20, 40, 56, 58, 100, 191, 196, 203, 208, 233, 234,
                   246, 251, 276, 300, 336, 348, 352, 372, 381, 428,
                   440, 442, 470, 528, 574, 579, 620, 642, 647, 638,
                   674, 688, 891, 703, 705, 724, 752, 826)
  
  # Code of Russia as a partner
  rus.Partner <- 643L
  
  # Introduce in the CT countries mapping table new country codes
  #   889 - Former Soviet Republics
  #   888 - Rest of the World
  eu.Code <- 492L
  fsr.Code <- 889L
  row.Code <- 888L
  
  
  if (topPeriod == 0) {
    df <-
      df %>%
      dplyr::mutate(
        Partner.Top = Partner.Code,
        Partner.Top = ifelse(
          oneEU &
            !otherEU & Partner.Top %in% eu.Partners,
          eu.Code,
          Partner.Top
        ),
        Partner.Top = ifelse(
          oneRUS & Partner.Top %in% rus.Partner,
          rus.Partner,
          Partner.Top
        ),
        Partner.Top = ifelse(oneFSR &
                               Partner.Top %in% fsr.Partners, fsr.Code, Partner.Top)
      ) %>%
      dplyr::group_by_(.dots = names(.)[names(.) %in% c(groupVars, "Partner.Top")]) %>%
      dplyr::mutate(Value.Sum = sum(Value, na.rm = TRUE)) %>%
      dplyr::group_by_(.dots = names(.)[names(.) %in% c(groupVars)]) %>%
      dplyr::mutate(Rank = dense_rank(desc(Value.Sum)) - 1L) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        Partner.Top = ifelse(Rank >= top_n, row.Code, Partner.Top),
        Partner.Top = ifelse(
          Rank >= top_n &
            otherEU & Partner.Code %in% eu.Partners,
          eu.Code,
          Partner.Top
        ),
        Rank = ifelse(Rank >= top_n, top_n, Rank)
      ) %>%
      dplyr::select(-Value.Sum)
    
  } else {
    # Actual calculations
    df <-
      df %>%
      dplyr::filter(Year > max(Year) - topPeriod) %>%
      dplyr::mutate(
        Partner.Top = Partner.Code,
        Partner.Top = ifelse(
          oneEU &
            !otherEU & Partner.Top %in% eu.Partners,
          eu.Code,
          Partner.Top
        ),
        Partner.Top = ifelse(
          oneRUS & Partner.Top %in% rus.Partner,
          rus.Partner,
          Partner.Top
        ),
        Partner.Top = ifelse(oneFSR &
                               Partner.Top %in% fsr.Partners, fsr.Code, Partner.Top)
      ) %>%
      dplyr::group_by_(.dots = names(.)[names(.) %in% c(groupVars, "Partner.Top", "Partner.Code") &
                                          !names(.) %in% c("Year", "Period", "Classification")]) %>%
      dplyr::summarise(Value.Sum = sum(Value, na.rm = TRUE)) %>%
      dplyr::group_by_(.dots = names(.)[names(.) %in% c(groupVars)]) %>%
      dplyr::mutate(Rank = dense_rank(desc(Value.Sum)) - 1L) %>%
      dplyr::select(-Value.Sum) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        Partner.Top = ifelse(Rank >= top_n, row.Code, Partner.Top),
        Partner.Top = ifelse(
          Rank >= top_n &
            otherEU & Partner.Code %in% eu.Partners,
          eu.Code,
          Partner.Top
        ),
        Rank = ifelse(Rank >= top_n, top_n, Rank)
      ) %>%
      dplyr::right_join(df, names(df)[names(df) %in% c("Trade.Flow.Code",
                                                       "Reporter.Code",
                                                       "Partner.Code",
                                                       "Commodity.Code",
                                                       "Variable")]) %>%
      dplyr::mutate(
        Rank = ifelse(is.na(Rank), top_n, Rank),
        Partner.Top = ifelse(Rank >= top_n, row.Code, Partner.Top),
        Partner.Top = ifelse(
          Rank >= top_n &
            otherEU & Partner.Code %in% eu.Partners,
          eu.Code,
          Partner.Top
        ),
        Rank = ifelse(Rank >= top_n, top_n, Rank)
      )
  }
  
  if (agg) {
    df <-
      df %>%
      dplyr::mutate(Partner.Code = Partner.Top) %>%
      dplyr::select(-Rank, -Partner.Top) %>%
      dplyr::group_by_(.dots = names(.)[names(.) %in% c(groupVars, "Partner.Code")]) %>%
      dplyr::summarise(Value = sum(Value)) %>%
      dplyr::ungroup()
  }
  
  return(df)
  
}