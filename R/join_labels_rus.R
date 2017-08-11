#' Add russian lables of codes to the trade data
#'
#'  @param rusCountries,rusFlows Path to the Rdata file with mapping tables.
#'  @param keepCodes Logical variable for keeping or removing the codes.
join_labels_rus <-
  function(data,
           rusCountries = system.file("extdata", "rus_countries.xlsx", package = "tradeAnalysis"),
           rusFlows = system.file("extdata", "rus_flows.xlsx", package = "tradeAnalysis"),
           commAggMT = system.file("extdata", "HS_agg_names.csv", package = "tradeAnalysis"),
           keepCodes = TRUE) {
    require(tidyverse)
    require(readxl)
    
    class <-
      bind_rows(classes, read.csv(commAggMT, stringsAsFactors = FALSE))
    
    rusCountriesMT <-
      readxl::read_excel(rusCountries, sheet = 1, col_names = TRUE) %>%
      dplyr::select(Code, Name)
    
    rusFlows <-
      readxl::read_excel(rusFlows, sheet = 1, col_names = TRUE)
    
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
        dplyr::mutate(Reporter.Code = as.integer(Reporter.Code)) %>%
        dplyr::left_join(dplyr::rename(rusCountriesMT, Reporter = Name),
                         by = c("Reporter.Code" = "Code")) %>%
        dplyr::mutate(Reporter = ifelse(is.na(Reporter), Reporter.Code, Reporter))
    }
    
    # Reporters names
    if ("r" %in% names(data)) {
      data <- data %>%
        dplyr::mutate(r = as.integer(r)) %>%
        dplyr::left_join(dplyr::rename(rusCountriesMT, Reporter = Name),
                         by = c("r" = "Code")) %>%
        dplyr::mutate(Reporter = ifelse(is.na(Reporter), r, Reporter))
    }
    
    # Partners names
    if ("Partner.Code" %in% names(data)) {
      data <- data %>%
        dplyr::mutate(Partner.Code = as.integer(Partner.Code)) %>%
        dplyr::left_join(dplyr::rename(rusCountriesMT, Partner = Name),
                         by = c("Partner.Code" = "Code")) %>%
        dplyr::mutate(Partner = ifelse(is.na(Partner), Partner.Code, Partner))
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
        select_(.dots = names(.)[!names(.) %in% oldNames])
    }
    
    return(data)
    
  }