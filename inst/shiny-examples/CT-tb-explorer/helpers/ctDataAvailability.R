

# Server logic for data availability module
ctDataAvailability <- function(input, output, session, getData) {
  values <- reactiveValues(avData = NULL)
  
  observeEvent(getData(), {
    values$avData <- getData()
    
    if (!is.null(values$avData)) {
      # Update the countries availability for selection
      selectedCountries <- input$countriesFilter
      availableCountries <-
        distinct(values$avData, Reporter.Code) %>%
        join_labs()
      availableCountries <-
        setNames(availableCountries$Reporter.Code,
                 availableCountries$Reporter)
      updateSelectizeInput(session,
                           "countriesFilter",
                           choices = availableCountries,
                           selected = selectedCountries)
      
      # Update years availability
      years <-
        isolate(c(min(values$avData$Year), max(values$avData$Year)))
      yearValue <- input$yearPeriod
      if (min(yearValue) < min(years))
        yearValue[1] <- min(years)
      if (max(yearValue) > max(years))
        yearValue[2] <- max(years)
      updateSliderInput(
        session,
        "yearPeriod",
        min = min(years),
        max = max(years),
        value = yearValue
      )
    }
    
  })
  #
  observeEvent({
    input$regionsFilter
  }, {
    selectedCountries <- input$countriesFilter
    selectedGroups <- input$regionsFilter
    if (!any(is.null(selectedGroups))) {
      suggestedCountries <- NULL
      if ("FSR" %in% selectedGroups) {
        suggestedCountries <-
          c(suggestedCountries,
            setNames(getFSR()$Partner.Code, getFSR()$Partner)) %>%
          unique()
      }
      if ("EU" %in% selectedGroups) {
        suggestedCountries <-
          c(suggestedCountries,
            setNames(getEU()$Partner.Code, getEU()$Partner)) %>%
          unique()
      }
      if ("All world" %in% selectedGroups) {
        suggestedCountries <-
          c(suggestedCountries,
            setNames(partners$Partner.Code, partners$Partner)) %>%
          unique()
      }
      selectedCountries <- unique(c(suggestedCountries))
      updateSelectizeInput(session, "countriesFilter", selected = selectedCountries)
    }
  })
  
  getAnnualAvail <- reactive({
    codes <- input$countriesFilter
    years <- seq(input$yearPeriod[1], input$yearPeriod[2], 1)
    # browser()
    if (!is.null(values$avData)) {
      out <-
        values$avData %>%
        filter(Reporter.Code %in% codes,
               Year %in% years) %>%
        join_labs()
    } else {
      out <- NULL
    }
    out
  })
  
  output$dataAvailCombined <-
    DT::renderDataTable({
      built_avail_table(getAnnualAvail())
    }, server = F)
  
  output$downloadDataAvailCombined <-
    downloadHandler(
      filename = function() {
        paste("Combined_CT_avail_", Sys.Date(), '.csv', sep = '')
      },
      content = function(file) {
        write.csv(
          output$downloadDataAvailCombined <-
            downloadHandler(
              filename = function() {
                paste("Combined_CT_avail_", Sys.Date(), '.csv', sep = '')
              },
              content = function(file) {
                write.csv(built_avail_table(getCombinedAvail())$x$data, file)
              },
              contentType = "text/csv"
            )$x$data,
          file
        )
      },
      contentType = "text/csv"
    )
}
