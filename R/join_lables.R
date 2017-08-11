#' Add lables of codes to the trade data
#'
#'  @param mappingTbls,commAggMT Path to the Rdata file with mapping tables.
#'  @param keepCodes Logical variable for keeping or removing the codes.
join_lables <-
  function(data,
           lang = NA,
           # mappingTbls = "ctClass.Rdata",
           commAggMT = system.file("extdata", "HS_agg_names.csv", package = "tradeAnalysis"),
           countriesExtras = system.file("extdata", "countries_extras_names.csv", package = "tradeAnalysis"),
           keepCodes = TRUE) {
    require(tidyverse)
    
    if (is.na(lang)) {
      class <-
        bind_rows(classes, read.csv(commAggMT, stringsAsFactors = FALSE))
      
      countries <-
        bind_rows(
          dplyr::rename(partners, Code = Partner.Code, Name = Partner),
          dplyr::rename(reporters, Code = Reporter.Code, Name = Reporter),
          readr::read_csv(
            countriesExtras,
            col_types = cols(Code = col_integer(),
                             Name = col_character())
          )
        ) %>%
        distinct()
      
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
        if(class(data$Reporter.Code) == "integer") {
          data <- 
            data %>%
            left_join(dplyr::rename(countries, Reporter = Name),
                      by = c("Reporter.Code" = "Code")) %>%
            mutate(Reporter = ifelse(is.na(Reporter), Reporter.Code, Reporter))
        }
        if(class(data$Reporter.Code) == "character") {
          data <- 
            data %>%
            left_join(
              countries %>% 
                dplyr::mutate(Code = as.character(Code)) %>% 
                dplyr::rename(Reporter = Name),
              by = c("Reporter.Code" = "Code")) %>%
            mutate(Reporter = ifelse(is.na(Reporter), Reporter.Code, Reporter))
        }
        if(class(data$Reporter.Code) == "numeric") {
          data <- 
            data %>%
            left_join(
              countries %>% 
                dplyr::mutate(Code = as.numeric(Code)) %>% 
                dplyr::rename(Reporter = Name),
              by = c("Reporter.Code" = "Code")) %>%
            mutate(Reporter = ifelse(is.na(Reporter), Reporter.Code, Reporter))
        }
        
      }
      
      # Reporters names
      if ("r" %in% names(data)) {
        if(is.integer(data$r)) {
          data <- 
            data %>%
            left_join(dplyr::rename(countries, Reporter = Name),
                      by = c("r" = "Code")) %>%
            mutate(Reporter = ifelse(is.na(Reporter), r, Reporter))
        }
        if(is.character(data$r)) {
          data <- 
            data %>%
            left_join(
              countries %>% 
                dplyr::mutate(Code = as.character(Code)) %>% 
                dplyr::rename(Reporter = Name),
              by = c("r" = "Code")) %>%
            mutate(Reporter = ifelse(is.na(Reporter), r, Reporter))
        }
      }
      
      # Partners names
      if ("Partner.Code" %in% names(data)) {
        if(is.integer(data$Partner.Code)) {
          data <- 
            data %>%
            left_join(dplyr::rename(countries, Partner = Name),
                      by = c("Partner.Code" = "Code")) %>%
            mutate(Partner = ifelse(is.na(Partner), Partner.Code, Partner))
        }
        if(is.character(data$Partner.Code)) {
          data <- 
            data %>%
            left_join(
              countries %>% 
                dplyr::mutate(Code = as.character(Code)) %>% 
                dplyr::rename(Partner = Name),
              by = c("Partner.Code" = "Code")) %>%
            mutate(Partner = ifelse(is.na(Partner), Partner.Code, Partner))
        }
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
          left_join(tradeAnalysis::units %>% select(-Unit.Description),
                    by = "Qty.Unit.Code")
      }
      
      if ("Commodity.Code" %in% names(data)) {
        data <- data %>%
          mutate(Commodity.Code = as.character(Commodity.Code)) %>%
          left_join(classes, by = "Commodity.Code") %>% 
          mutate(Commodity = ifelse(is.na(Commodity), Commodity.Code, Commodity))
      }
      
      if (!keepCodes) {
        data <-
          data %>%
          select_(.dots = one_of(oldNames))
      }
      
    } else {
      data <- join_labels_rus(data = data, keepCodes = keepCodes)
    }
    
    return(data)
    
  }