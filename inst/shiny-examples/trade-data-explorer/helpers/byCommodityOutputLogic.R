byCommodityOutputLogic <- function(input, output, session, getTbPlot) {
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
             "  -  by commodity trade structure with ",
             sel_unique(getTbPlot()$dataTable, Partner))
    } else {
      NULL
    }
  })
  
  output$tbDataTable <-
    DT::renderDataTable({
      if (!is.null(getTbPlot()$data)) {
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