#' Runking top N partners
rank_top_N_partners <- function(df, top_n, oneEU = TRUE, oneFSR = TRUE, oneRUS = FALSE) {
  # top_n <- 5
  top_n <- as.integer(top_n)
  
  group_var <-
    c("Classification",
      "Year",
      "Period",
      "Trade.Flow.Code",
      "Commodity.Code",
      "Partner.Code",
      "Variable",
      "Type",
      "Partner",
      "Trade.Flow",
      "Commodity",
      "Unit",
      "Qty.Unit.Code",
      "Unit.Description",
      "Partner.Code.agg",
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
  
  
  world <- df %>% filter(Partner.Code == 0)
  
  df %>% 
    filter(Partner.Code != 0) %>% 
    mutate(Partner.Top = as.integer(Partner.Code),
           Partner.Top = if_else(oneEU & Partner.Top %in% eu.Partners, eu.Code, Partner.Top),
           Partner.Top = if_else(oneRUS & Partner.Top %in% rus.Partner, rus.Partner, Partner.Top),
           Partner.Top = if_else(oneFSR & Partner.Top %in% fsr.Partners, fsr.Code, Partner.Top)) %>%
    group_by_(.dots = names(.)[names(.) %in% c(groupVars, "Partner.Top")]) %>%
    mutate(Value.Sum = sum(Value, na.rm = TRUE)) %>% 
    group_by_(.dots = names(.)[names(.) %in% c(groupVars)]) %>% 
    mutate(Rank = dense_rank(desc(Value.Sum))) %>% 
    arrange(desc(Value.Sum)) %>% 
    mutate(Partner.Top = if_else(Rank >= top_n, row.Code, Partner.Top),
           Rank = if_else(Rank >= top_n, top_n, Rank)) %>% 
    select(-Value.Sum) %>% 
    ungroup() %>% 
    bind_rows(world %>% mutate(Rank = 0L, Partner.Top = Partner.Code)) %>% 
    return()
  
}