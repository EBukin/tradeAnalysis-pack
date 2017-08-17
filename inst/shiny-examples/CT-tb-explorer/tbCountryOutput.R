
# UI for generating output of the TB by country module
tbCountryOutput <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = "Trade balance",
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      collapsible = TRUE,
      plotOutput(ns("tbPlot"))
    ),
    box(
      title = "Data table",
      solidHeader = TRUE,
      status = "info",
      width = 12,
      collapsible = TRUE,
      textOutput(ns("tbAreaName")),
      br(),
      downloadButton(ns('tbDataTableDownload'), 'Download table'),
      br(),
      DT::dataTableOutput(ns('tbDataTable'))
    ),
    box(
      title = "Trade balance in details",
      solidHeader = TRUE,
      status = "info",
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      plotly::plotlyOutput(ns("tbImport")),
      plotly::plotlyOutput(ns("tbExport"))
    )
  )
}