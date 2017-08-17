# Modile for updating app selectors according to the available data
tbCountryInputUpdate <-
  function(input,
           output,
           session,
           getData) {
    observeEvent(list(input$loadData,input$uploadData), {
      allData <- getData()
      if ( !is.null(allData)) {
        selectedReporter <- input$tbReporter
        selectedPartner <- input$tbPartner
        selectedTimeInterval <- input$tbPeriod
        selectedCommodity <- input$tbCommodity
        
        # Update years
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
        
        # Update reporters
        availableReporters <-
          sel_dist(allData, Reporter.Code) %>%
          join_labels_rep() %>%
          arrange(Reporter)
        availableReporters <-
          setNames(availableReporters$Reporter.Code,
                   availableReporters$Reporter)
        updateSelectizeInput(session,
                             "tbReporter",
                             choices = availableReporters,
                             selected = selectedReporter)
        
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
    })
  }
