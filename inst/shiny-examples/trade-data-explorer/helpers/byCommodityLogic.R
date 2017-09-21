


# Server Logic for plotting trade balance for countries

byCommodityLogic <- function(input, output, session, getData) {
  # Plot TB
  getTbPlot <- reactive({
    # Include progress
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Creating plot", value = 0)
    progress$inc(0.1)
    
    
    # Extract main data
    selectedReporter <- input$tbReporter
    selectedPartner <- input$tbPartner
    selectedPartNumb <- as.numeric(input$tbNumPartners)
    if (is.na(selectedPartNumb)) selectedPartNumb <- 0
    selectedCommodity <- input$tbCommodity
    selectedTimeInterval <- seq(input$tbPeriod[1], input$tbPeriod[2], 1)
    selectedLang <- input$tbLang
    selectedOneEU <- FALSE
    selectedOtherEU <- FALSE
    selectedOneFSR <- FALSE
    selectedOtherFSR <- FALSE
    selectedSepRUS <- FALSE
    selectedNumPeriods <- as.numeric(input$tbNumPeriods)
    if (is.na(selectedNumPeriods)) selectedNumPeriods <- 0
    selectedPalitra <- input$tbPalitra
    
    
    
    if (!str_detect(selectedLang, regex("en", ignore_case = TRUE))) {
      xAxisName <- ""
      yAxisName <-
        "Торговый баланс, млн. дол. США\n-Импорт/+Экспорт"
      tbVarName <- "Торговый баланс"
      stackVarName <- "Продукт"
      
    } else {
      xAxisName <- ""
      yAxisName <- "Trade balance, million US$\n-Import/+Еxport"
      tbVarName <- "Trade balance"
      stackVarName <- "Commodity"
    }
    
    nameLab <- function(selectedPartNumb, Rep, Part, selectedLang) {
      if (str_detect(selectedLang, regex("en", ignore_case = TRUE))) {
        stringr::str_c("Top ",
                       selectedPartNumb,
                       " commodities traded between reporter ",
                       Rep,
                       " and the partner ",
                       Part)
      } else {
        stringr::str_c("Топ ",
                       selectedPartNumb,
                       " продуктов которыми торгует ",
                       Rep,
                       " с ",
                       Part)
      }
    }
    
    # Extract all data
    allData <- getData()
    
    if (!is.null(allData) & 
        all(!is.null(selectedCommodity))) {
      plotData <-
        allData %>%
        flt_rep_ct(selectedReporter) %>%
        flt_par_ct(selectedPartner) %>%
        flt_com_ct(selectedCommodity) %>%
        flt_year_ct(selectedTimeInterval)
    } else {
      plotData <- tibble()
    }
    
    if (nrow(plotData) > 0) {
      progress$inc(0.3)
      
      # Chart label
      chartLable <-
        plotData %>% 
        sel_dist(Reporter.Code, Partner.Code) %>% 
        join_labs(lang = selectedLang) 
      reporterName <- chartLable$Reporter
      chartLable <- nameLab(selectedPartNumb, chartLable$Reporter, chartLable$Partner, selectedLang)
      
      
      # Preparing horisontal axis lables
      xAxisLables <-
        plotData %>%
        sel_dist(Year, Type) %>%
        mutate(Type = ifelse(Type == "Direct", "", Type)) %>%
        mutate(Type = ifelse(Type == "Mirror", "*", Type)) %>%
        mutate(Type = ifelse(Type == "Monthly", "**", Type)) %>%
        arrange(Year) %>%
        unite(xLabs, Year, Type, sep = "") %>%
        .$xLabs
      
      # Building a plot ## ## ## ## ## ## ## ## ## ##
      pData <-
        plotData %>%
        mutate(Commodity.Code2 = Partner.Code) %>%
        mutate(Partner.Code = Commodity.Code) %>%
        mutate(Commodity.Code = Commodity.Code2) %>%
        select(-Commodity.Code2) %>%
        rank_agg_top_partners(
          top_n = selectedPartNumb,
          agg = T,
          oneEU = selectedOneEU,
          oneFSR = selectedOneFSR,
          otherFSR = selectedOtherFSR,
          sepRUS = selectedSepRUS,
          otherEU = selectedOtherEU,
          topPeriod = selectedNumPeriods
        )  %>%
        mutate(Partner.Code = ifelse(Partner.Code == "888", "Other commodities", Partner.Code)) %>%
        mutate(Value = Value / 1000000) %>%
        mutate(Commodity.Code2 = Partner.Code) %>%
        mutate(Partner.Code = Commodity.Code) %>%
        mutate(Commodity.Code = Commodity.Code2) %>%
        select(-Commodity.Code2)
      
      stackVar = "Commodity"
      
      plt <-
        pData %>%
        plot_tb(
          stackVar = stackVar,
          stackVarName = stackVarName,
          brewPalName = selectedPalitra,
          brewScale = T,
          returnData = T,
          horizontalLine = tbVarName,
          lang = selectedLang
        )
      
      plt$plot <-
        plt$plot +
        ggplot2::scale_x_discrete(name = xAxisName, labels = xAxisLables) +
        ggplot2::scale_y_continuous(
          breaks = pretty_breaks(9),
          labels = format_format(
            big.mark = " ",
            digits = 1,
            decimal.mark = ",",
            scientific = FALSE
          ),
          name = yAxisName
        ) +
        ggplot2::ggtitle(chartLable)
      
      progress$inc(0.1)
      
     
      # Import
      plt$PlImport <-
        pData %>%
        plot_tb(
          stackVar = stackVar,
          plotTradeBalance = F,
          exp = "",
          stackVarName = stackVarName,
          brewPalName = selectedPalitra,
          brewScale = T,
          returnData = F,
          horizontalLine = tbVarName,
          lang = selectedLang
        ) %>%
        ggplotly()
      
      plt$RporterName <- reporterName
      # Export
      plt$PlExport <-
        pData %>%
        plot_tb(
          stackVar = stackVar,
          plotTradeBalance = F,
          imp = "",
          stackVarName = stackVarName,
          brewPalName = selectedPalitra,
          brewScale = T,
          returnData = F,
          horizontalLine = tbVarName,
          lang = selectedLang
        ) %>%
        ggplotly()
      
      plt$dataTable <-
        pData %>%
        join_labs() %>%
        select(
               Reporter,
               Partner,
               Trade.Flow,
               Commodity,
               Rank,
               Period,
               Value) %>%
        spread(Period, Value) %>%
        arrange(Trade.Flow, Rank)
      
    } else {
      plt <- NULL
    }
    plt
  })
  
  return(getTbPlot)
}

