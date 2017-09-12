# Modile for updating app selectors according to the available data
mainTradersInputUpdate <-
  function(input,
           output,
           session,
           getData) {
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
            selectedCommodity <-
              tidyAvCom[names(tidyAvCom) %in% selectedCommodityGroup]
            updateSelectizeInput(session,
                                 "tbCommodity",
                                 choices = tidyAvCom,
                                 selected = selectedCommodity[[1]])
          }
        }
      }
    })
    
  }
