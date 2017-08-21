tbCountryInput <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = "Parameters selector",
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      fluidRow(column(
        4,
        sliderInput(
          ns("tbPeriod"),
          "Years",
          2005,
          2020,
          step = 1,
          ticks = FALSE,
          sep = "",
          value = c(2007, 2017)
        )
      ),
      column(
        8,
        selectizeInput(
          ns("tbCommodity"),
          "Commodity",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        )
      )),
      fluidRow(
        column(
          3,
          selectizeInput(
            ns("tbReporter"),
            "Reporter",
            choices = c("Ukraine" = "804"),
            multiple = FALSE
          )
        ),
        # column(2,
        #        actionButton(ns('updateReporter'), "Load data")),
        column(3,
               numericInput(
                 ns("tbNumPartners"),
                 "Number of partners",
                 value = 5
               )),
        column(3,
               numericInput(
                 ns("tbNumPeriods"),
                 "Number of top periods",
                 value = 5
               )),
        column(
          3,
          radioButtons(
            ns("tbLang"),
            "Language",
            choices = c("English" = "eng",
                        "Russian" = "rus"),
            selected = "eng",
            inline = TRUE
          )
        )
      ) ,
      fluidRow(
        column(2,
               checkboxInput(ns("tbEU"),
                             "EU as a region",
                             value = TRUE)),
        column(2,
               checkboxInput(
                 ns("tbOtherEU"),
                 "Other EU as a region",
                 value = FALSE
               )),
        column(2,
               checkboxInput(ns("tbFSR"),
                             "FSR as a region",
                             value = FALSE)),
        column(2,
               checkboxInput(
                 ns("tbOtherFSR"),
                 "Other FSR as a region",
                 value = TRUE
               )),
        column(2,
               checkboxInput(ns("tbSepRus"),
                             "Show Russia",
                             value = TRUE)),
        column(
          2,
          selectInput(
            ns("tbPalitra"),
            "Choose palitra",
            choices = c(
              "Set3",
              "Set2",
              "Set1",
              "Pastel2",
              "Pastel1",
              "Paired",
              "Dark2",
              "Accent"
            ),
            selected = "Set3"
          )
        )
      )
    ),
    box(
      title = "Save trade balance figure",
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      fluidRow(
        column(2,
               numericInput(ns("tbSaveWidth"),
                            "Width",
                            21)),
        column(2,
               numericInput(ns("tbSaveHeight"),
                            "Height",
                            12)),
        column(
          2,
          selectInput(
            ns("tbSaveUnits"),
            "Unit",
            choices = c("cm", "in", "px"),
            selected = "cm"
          )
        ),
        column(2,
               numericInput(ns("tbSaveRes"),
                            "Resolution dpi",
                            600)),
        column(
          2,
          downloadButton(
            ns('tbSavePNG'),
            label = "Save PNG plot",
            icon = icon("download"),
            width = "100%"
          )
        ),
        column(
          2,
          downloadButton(
            ns('tbSaveSVG'),
            label = "Save SVG plot",
            icon = icon("download"),
            width = "100%"
          )
        )
      )
    )
  )
}
