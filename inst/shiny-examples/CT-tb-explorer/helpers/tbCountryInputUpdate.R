# Modile for updating app selectors according to the available data
tbCountryInputUpdate <-
  function(input,
           output,
           session,
           getData,
           allReporters) {
    
    # Updating Reporters
    observeEvent(list(allReporters()), {
      allReps <- str_sub(allReporters(), end = -5)
      if (any(!is.null(allReps))) {
        allReps <- tibble(Reporter.Code = allReps)
        selectedReporter <- input$tbReporter
        if (!is.null(selectedReporter)) {
          # Update reporters
          availableReporters <-
            sel_dist(allReps, Reporter.Code) %>%
            join_labels_rep() %>%
            arrange(Reporter)
          availableReporters <-
            setNames(availableReporters$Reporter.Code,
                     availableReporters$Reporter)
          updateSelectInput(session,
                               "tbReporter",
                               choices = availableReporters,
                               selected = selectedReporter)
        }
      }
    })
    
    # Updating years
    observeEvent(list(getData()), {
      allData <- getData()
      if (!is.null(allData)) {
        selectedTimeInterval <- input$tbPeriod
        
        if (!is.null(selectedTimeInterval)) {
          years <- c(min(allData$Year), max(allData$Year))
          if (min(selectedTimeInterval) < min(years))
            selectedTimeInterval[1] <- min(years)
          if (max(selectedTimeInterval) > max(years))
            selectedTimeInterval[2] <- max(years)
          updateSliderInput(
            session,
            "tbPeriod",
            min = min(years),
            max = max(years),
            value = selectedTimeInterval
          )
        }
      }
    })
    
    # Updating Partners
    observeEvent(list(getData()), {
      allData <- getData()
      if (!is.null(allData)) {
        selectedPartner <- input$tbPartner
        
        if (!is.null(selectedPartner)) {
          # Update partners
          availablePartners <-
            sel_dist(allData, Partner.Code) %>%
            join_labels_par() %>%
            arrange(Partner)
          availablePartners <-
            setNames(availablePartners$Partner.Code,
                     availablePartners$Partner)
          updateSelectizeInput(session,
                               "tbPartner",
                               choices = availablePartners,
                               selected = selectedPartner)
        }
      }
    })
    
    
    
    # Updating available stand along commodities
    observeEvent(list(getData()), {
      allData <- getData()
      if (!is.null(allData)) {
        selectedCommodity <- input$tbCommodity
        if (!is.null(selectedCommodity)) {
          # Update commodities
          avCom <-
            sel_dist(allData, Commodity.Code) %>%
            join_labels_com() %>%
            arrange(Commodity)
          avCom <- setNames(avCom$Commodity.Code, avCom$Commodity)
          tidyAvCom <- tidy_commodity_list(avCom, trunkSymbols = 60)
          if (any(!is.null(selectedCommodity)) &
              selectedCommodity == "")
            selectedCommodity <- tidyAvCom[1][1]
          updateSelectizeInput(session,
                               "tbCommodity",
                               choices = tidyAvCom,
                               selected = selectedCommodity)
        }
      }
    })
    
    # Updating available stand along commodities
    observeEvent(list(getData(), input$tbCommodityGroup), {
      allData <- getData()
      if (!is.null(allData)) {
        selectedCommodity <- input$tbCommodity
        selectedCommodityGroup <- input$tbCommodityGroup
        
        if (!is.null(selectedCommodityGroup)) {
          # if (selectedCommodityGroup == "") {
            # Update commodities
          avCom <-
            sel_dist(allData, Commodity.Code) %>%
            join_labels_com() %>%
            arrange(Commodity)
          avCom <- setNames(avCom$Commodity.Code, avCom$Commodity)
          tidyAvCom <- tidy_commodity_list(avCom, trunkSymbols = 60)
          tidyAvCom2 <- tidyAvCom %>% names()
          if (any(!is.null(selectedCommodityGroup)) &
              selectedCommodityGroup == "") {
            selectedCommodityGroup <- tidyAvCom2[2][1]
          }
          updateSelectizeInput(session,
                               "tbCommodityGroup",
                               choices = tidyAvCom2,
                               selected = selectedCommodityGroup)
          # }
        }
        if (!is.null(selectedCommodityGroup)) {
          if (selectedCommodityGroup != "") {
            # Update commodities
          avCom <-
            sel_dist(allData, Commodity.Code) %>%
            join_labels_com() %>%
            arrange(Commodity)
          avCom <- setNames(avCom$Commodity.Code, avCom$Commodity)
          tidyAvCom <- tidy_commodity_list(avCom, trunkSymbols = 60)
          selectedCommodity <- tidyAvCom[names(tidyAvCom) %in% selectedCommodityGroup]
          updateSelectizeInput(session,
                               "tbCommodity",
                               choices = tidyAvCom,
                               selected = selectedCommodity[[1]])
          }
        }
      }
    })
    
  }
