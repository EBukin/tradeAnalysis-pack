


ctDataAvailabilityUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = "Parameters selector",
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      fluidRow(
        column(
          width = 3,
          sliderInput(
            ns("yearPeriod"),
            "Years interval",
            2005,
            2020,
            step = 1,
            sep = "",
            value = c(2012, 2017)
          )
        ),
        column(
          width = 3,
          selectizeInput(
            ns("regionsFilter"),
            "Select regions",
            choices = c("FSR", "EU", "All world"),
            multiple = TRUE
          )
        ),
        column(
          width = 6,
          selectizeInput(
            ns("countriesFilter"),
            "Select countries",
            choices = NULL,
            multiple = TRUE
          )
        )
      )
    ),
    box(
      title = "Combined (annual and monthly) data availability",
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      downloadButton('downloadDataAvailCombined', 'Download table'),
      DT::dataTableOutput(ns('dataAvailCombined'))
    )
  )
}
