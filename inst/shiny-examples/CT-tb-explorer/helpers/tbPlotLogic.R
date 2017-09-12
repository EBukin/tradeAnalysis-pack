


# Server Logic for plotting trade balance for countries

tbPlotLogic <- function(input, output, session, getData) {
  # Plot TB
  getTbPlot <- reactive({
    # Include progress
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Creating plot", value = 0)
    progress$inc(0.1)
    
    # Extract main data
    selectedReporter <- input$tbReporter
    selectedPartNumb <- input$tbNumPartners
    selectedCommodity <- input$tbCommodity
    selectedTimeInterval <-
      seq(input$tbPeriod[1], input$tbPeriod[2], 1)
    selectedLang <- input$tbLang
    selectedOneEU <- input$tbEU
    selectedOtherEU <- input$tbOtherEU
    selectedOneFSR <- input$tbFSR
    selectedOtherFSR <- input$tbOtherFSR
    selectedSepRUS <- input$tbSepRus
    selectedNumPeriods <- input$tbNumPeriods
    selectedPalitra <- input$tbPalitra
    
    
    
    if (!str_detect(selectedLang, regex("en", ignore_case = TRUE))) {
      xAxisName <- ""
      yAxisName <-
        "Торговый баланс, млн. дол. США\n-Импорт/+Экспорт"
      tbVarName <- "Торговый баланс"
      stackVarName <- "Партнер"
    } else {
      xAxisName <- ""
      yAxisName <- "Trade balance, million US$\n-Import/+Еxport"
      tbVarName <- "Trade balance"
      stackVarName <- "Partner"
    }
    # Extract all data
    allData <- getData()
    
    if (!is.null(allData)) {
      plotData <-
        allData %>%
        flt_rep_ct(selectedReporter) %>%
        flt_com_ct(selectedCommodity) %>%
        flt_year_ct(selectedTimeInterval)
    } else {
      plotData <- tibble()
    }
    
    if (nrow(plotData) > 0) {
      progress$inc(0.3)
      
      reporterName <-
        partners %>%
        flt_par_ct(selectedReporter) %>%
        join_labs(lang = selectedLang) %>%
        .$Partner
      chartLable <- str_c(reporterName)
      
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
        filter(Partner.Code != 0) %>%
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
        mutate(Value = Value / 1000000)
      
      plt <-
        pData %>%
        plot_tb(
          stackVar = "Partner",
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
      
      plt$RporterName <- reporterName
      # Import
      plt$PlImport <-
        pData %>%
        plot_tb(
          stackVar = "Partner",
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
      
      # Export
      plt$PlExport <-
        pData %>%
        plot_tb(
          stackVar = "Partner",
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
        select(Commodity,
               Reporter,
               Trade.Flow,
               Partner,
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


