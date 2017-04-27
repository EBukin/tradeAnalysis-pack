#' Add lables of codes to the trade data
#'
#'  @param mappingTbls Path to the Rdata file with mapping tables.
#'  @param keepCodes Logical variable for keeping or removing the codes.
join_lables <-
  function(data,
           commAggMT = system.file("extdata", "HS_agg_names.csv", package = "tradeAnalysis"),
           keepCodes = TRUE) {
    # require(plyr)
    # require(dplyr)
    
    # Loading mapping tabels
    data("classes", envir = environment())
    aggNames <-
      read_csv(commAggMT,
               col_types =  cols(Commodity.Code = col_character(),
                                 Commodity = col_character()))
    data("rep", envir = environment())
    data("part", envir = environment())
    data("units", envir = environment())
    data("reg", envir = environment())
    
    classes <-
      bind_rows(classes, aggNames)
    
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
      if (all(is.integer(data$Reporter.Code))) {
        data <- data %>%
          mutate(Reporter.Code = as.integer(Reporter.Code)) %>%
          left_join(mutate(rep, Reporter.Code = as.integer(Reporter.Code)) %>% 
                      filter(!is.na(Reporter.Code)), 
                    by = "Reporter.Code")
      } else {
        data <- data %>%
          mutate(Reporter.Code = as.character(Reporter.Code)) %>%
          left_join(mutate(rep, Reporter.Code = as.character(Reporter.Code)) %>% 
                      filter(!is.na(Reporter.Code)), 
                    by = "Reporter.Code")
      }
    }
    
    # Reporters names
    if ("r" %in% names(data)) {
      data <- data %>%
        # mutate(r = as.integer(r)) %>%
        left_join(rep, by = c("r" = "Reporter.Code"))
    }
    
    # Partners names
    if ("Partner.Code" %in% names(data)) {
      if (all(is.integer(data$Partner.Code))) {
        data <- data %>%
          mutate(Partner.Code = as.integer(Partner.Code)) %>%
          left_join(mutate(part, Partner.Code = as.integer(Partner.Code)) %>% 
                      filter(!is.na(Partner.Code)), 
                    by = "Partner.Code")
      } else {
        data <- data %>%
          mutate(Partner.Code = as.character(Partner.Code)) %>%
          left_join(mutate(part, Partner.Code = as.character(Partner.Code)) %>% 
                      filter(!is.na(Partner.Code)), 
                    by = "Partner.Code")
      }
    }
    
    # Trade flows names
    if ("Trade.Flow.Code" %in% names(data)) {
      data <- data %>%
        mutate(Trade.Flow.Code = as.integer(Trade.Flow.Code)) %>%
        left_join(reg, by = "Trade.Flow.Code")
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
