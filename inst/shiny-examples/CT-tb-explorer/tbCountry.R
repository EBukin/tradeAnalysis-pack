


# Server Logic for plotting trade balance for countries

tbCountry <- function(input, output, session, getData) {
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


tbCountryOutputLogic <- function(input, output, session, getTbPlot) {
  # Preparing output
  output$tbSavePNG <- downloadHandler(
    filename = function() {
      paste(getTbPlot()$RporterName, '.png', sep = '')
    },
    content = function(file) {
      selRes <- input$tbSaveRes
      selHeight <- input$tbSaveHeight
      selWidth <- input$tbSaveWidth
      selUnits <- input$tbSaveUnits
      device <-
        function(..., width, height)
          grDevices::png(
            ...,
            width = selWidth,
            height = selHeight,
            res = selRes,
            units = selUnits
          )
      ggsave(file, plot = getTbPlot()$plot, device = device)
    }
  )
  
  output$tbSaveSVG <- downloadHandler(
    filename = function() {
      paste(getTbPlot()$RporterName, '.svg', sep = '')
    },
    content = function(file) {
      selRes <- input$tbSaveRes
      selHeight <- input$tbSaveHeight
      selWidth <- input$tbSaveWidth
      selUnits <- input$tbSaveUnits
      device <-
        function(..., width, height)
          grDevices::svg(..., width = selWidth, height = selHeight)
      ggsave(file, plot = getTbPlot()$plot, device = device)
    }
  )
  
  output$tbPlot <- renderPlot({
    getTbPlot()$plot
  })
  
  output$tbAreaName <- renderText({
    if (!is.null(getTbPlot()$data)) {
      paste0(getTbPlot()$RporterName,
             "  -  ",
             sel_unique(getTbPlot()$dataTable, Commodity))
    } else {
      NULL
    }
  })
  
  output$tbDataTable <-
    DT::renderDataTable({
      # browser()
      if (!is.null(getTbPlot()$data)) {
        # browser()
        pDataHere <-
          getTbPlot()$dataTable %>%
          select(-1, -2) %>%
          mutate(Rank = Rank + 1)
        datatable(
          pDataHere,
          options = list(
            autoWidth = TRUE,
            scrollX = TRUE,
            pageLength = 25
          ),
          filter = "top",
          rownames = FALSE
        ) %>%
          formatCurrency(
            4:length(pDataHere),
            currency = "",
            interval = 3,
            mark = " ",
            digits = 1
          )
      } else {
        NULL
      }
    },
    server = F)
  
  output$tbImport <- renderPlotly({
    if (!is.null(getTbPlot()$PlImport)) {
      getTbPlot()$PlImport
    } else {
      NULL
    }
  })
  
  output$tbExport <- renderPlotly({
    if (!is.null(getTbPlot()$PlExport)) {
      getTbPlot()$PlExport
    } else {
      NULL
    }
  })
  
  output$tbDataTableDownload <-
    downloadHandler(
      filename = function() {
        paste("tb_data_", Sys.Date(), '.csv', sep = '')
      },
      content = function(file) {
        if (!is.null(getTbPlot()$data)) {
          # browser()
          pDataHere <-
            getTbPlot()$dataTable %>%
            select(-1, -2) %>%
            mutate(Rank = Rank + 1)
        } else {
          pDataHere <- NULL
        }
        write.csv(pDataHere, file)
      },
      contentType = "text/csv"
    )
}