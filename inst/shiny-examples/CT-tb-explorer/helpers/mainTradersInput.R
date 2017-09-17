mainTradersInput <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = "Parameters selector",
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      fluidRow(
        column(
          7,
          selectizeInput(
            ns("tbCommodity"),
            "Commodity",
            choices = NULL,
            selected = NULL,
            multiple = FALSE
          )
        ),
        column(2,
               selectInput(
                 ns("tbReporter"),
                 "Reporter",
                 choices = c("World" = "0"),
                 multiple = FALSE
               )),
        column(
          3,
          sliderInput(
            ns("tbPeriod"),
            "Years",
            2005,
            2020,
            step = 1,
            sep = "",
            value = c(2007, 2017)
          )
        )
      ),
      fluidRow(
        column(3,
               numericInput(
                 ns("tbNumPartners"),
                 "Partners number",
                 value = 5
               )),
        column(3,
               numericInput(
                 ns("tbNumPeriods"),
                 "Top periods number",
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
        ),
        # column(
        #   3,
        #   radioButtons(
        #     ns("dataType"),
        #     "Data type",
        #     choices = c("Only direct" = "Direct",
        #                 "Direct and Mirror" = "Mixed"),
        #     selected = "Mixed",
        #     inline = TRUE
        #   )
        # ),
        column(
          3,
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
        column(2,
               selectInput(
                 ns("tbSaveUnits"),
                 "Unit",
                 choices = c("cm", "in", "px"),
                 selected = "cm"
               )),
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
