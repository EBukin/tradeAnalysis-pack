#' Add lables of codes to the trade data
#'
#'  @param mappingTbls,commAggMT Path to the Rdata file with mapping tables.
#'  @param keepCodes Logical variable for keeping or removing the codes.
join_lables <-
  function(data,
           # mappingTbls = "ctClass.Rdata",
           commAggMT = system.file("extdata", "HS_agg_names.csv", package = "tradeAnalysis"),
           reportersExtras = system.file("extdata", "reporters_extras_names.csv", package = "tradeAnalysis"),
           partnersExtras = system.file("extdata", "partners_extras_names.csv", package = "tradeAnalysis"),
           keepCodes = TRUE) {
    require(tidyverse)
    
    class <-
      bind_rows(classes, read.csv(commAggMT, stringsAsFactors = FALSE))
    
    partners <-
      bind_rows(partners,
                readr::read_csv(
                  partnersExtras,
                  col_types = cols(Partner.Code = col_integer(),
                                   Partner = col_character())
                )) %>% distinct()
    
    reporters <-
      bind_rows(reporters,
                readr::read_csv(
                  reportersExtras,
                  col_types = cols(Reporter.Code = col_integer(),
                                   Reporter = col_character())
                )) %>% distinct()
    
    oldNames <- c("r",
                  "Reporter.Code",
                  "Partner.Code",
                  "Trade.Flow.Code",
                  "Qty.Unit.Code")
    newNames <-
      c("Reporter", "Partner", "Trade.Flow", "Unit", "Commodity")
    
    data <-
      data %>%
      select_(.dots = names(.)[!names(.) %in% newNames])
    
    # Reporters names
    if ("Reporter.Code" %in% names(data)) {
      data <- data %>%
        mutate(Reporter.Code = as.integer(Reporter.Code)) %>%
        left_join(reporters, by = "Reporter.Code")
    }
    
    # Reporters names
    if ("r" %in% names(data)) {
      data <- data %>%
        mutate(r = as.integer(r)) %>%
        left_join(reporters, by = c("r" = "Reporter.Code")) %>%
        mutate(Reporter = ifelse(is.na(Reporter, Reporter.Code, Reporter)))
    }
    
    # Partners names
    if ("Partner.Code" %in% names(data)) {
      data <- data %>%
        mutate(Partner.Code = as.integer(Partner.Code)) %>%
        left_join(partners, by = "Partner.Code") %>%
        mutate(Partner = ifelse(is.na(Partner), Partner.Code, Partner))
    }
    
    # Trade flows names
    if ("Trade.Flow.Code" %in% names(data)) {
      data <- data %>%
        mutate(Trade.Flow.Code = as.integer(Trade.Flow.Code)) %>%
        left_join(regimes, by = "Trade.Flow.Code")
    }
    
    if ("Qty.Unit.Code" %in% names(data)) {
      data <- data %>%
        mutate(Qty.Unit.Code = as.integer(Qty.Unit.Code)) %>%
        left_join(units %>% select(-Unit.Description),
                  by = "Qty.Unit.Code")
    }
    
    if ("Commodity.Code" %in% names(data)) {
      data <- data %>%
        mutate(Commodity.Code = as.character(Commodity.Code)) %>%
        left_join(classes, by = "Commodity.Code")
    }
    
    if (!keepCodes) {
      data <-
        data %>%
        select_(.dots = one_of(oldNames))
    }
    
    return(data)
  }