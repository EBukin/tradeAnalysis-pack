#' Convert list of commodity codes to the tidy list of codes
#' 
#' @param existingCommodities vector of commodicy codes
#' @param trunkSymbols integer number of symbols to show in the names of commodities
#' @param orderedDataFrame if TRUE returns ordered data frame instead of a list 
#' @inheritParams join_labels_com
#' @examples 
#' tidy_commodity_list(tradeAnalysis::classes[7:10,]$Commodity.Code)
#' library(shiny)
#' ui <- fluidPage(
#'   titlePanel("Hello Shiny!"),
#'   sidebarLayout(
#'     sidebarPanel(actionButton("click", "Update commodities")),
#'     mainPanel(selectizeInput("bins", "CommodityCode", choices = NULL, width = "100%"))))
#' server <- function(input, output, session) {
#'   observeEvent(input$click, {
#'     updateSelectizeInput(session, inputId = "bins", choices = tidy_commodity_list(tradeAnalysis::classes), selected = tidy_commodity_list(tradeAnalysis::classes)$`CT aggregates`[1])
#'   })}
#' shinyApp(ui = ui, server = server)

tidy_commodity_list <-
  function(existingCommodities,
           trunkSymbols = 50,
           orderedDataFrame = FALSE,
           existingAggregares =
             read.csv(system.file("extdata", "HS_agg_names.csv", package = "tradeAnalysis"))) {
    
    allCommodities <-
      tradeAnalysis::classes
    
    suppressWarnings(
      priorityOne <-
        allCommodities %>%
        mutate(Group = is.na(as.integer(Commodity.Code))) %>%
        filter(Group) %>%
        mutate(Group = 0001,
               SubGroup = "CT aggregates")
    )
    priorityTwo <-
      existingAggregares %>%
      separate(Commodity.Code,
               c("gr", "com"),
               sep = "__",
               remove = F) %>%
      separate(Commodity.Code,
               c("comm", "dig"),
               sep = "_HS",
               remove = F) %>%
      mutate(SubGroup = stringr::str_c(gr, " WTO AgFood based on ", dig, " HS digits")) %>%
      select(-dig,-comm,-com,-gr) %>%
      mutate(Group = 0002)
    
    suppressWarnings(
      otherCommodities <-
        allCommodities %>%
        mutate(Group = is.na(as.integer(Commodity.Code))) %>%
        filter(!Group) %>%
        mutate(
          cc2 = str_sub(Commodity.Code, 1, 2),
          cc4 = str_sub(Commodity.Code, 3, 4),
          cc6 = str_sub(Commodity.Code, 5, 6)
        )
    )
    
    priorityTwoAndHalf <-
      otherCommodities %>%
      filter(Commodity.Code %in% wtoAgFood$Commodity.Code) %>%
      select(-cc2,-cc4,-cc6) %>%
      mutate(Group = 0004) %>%
      mutate(SubGroup = "All components of WTO Ag Food")
    
    priorityTree <-
      otherCommodities %>%
      filter(stringr::str_length(Commodity.Code) == 2) %>%
      select(-cc2,-cc4,-cc6) %>%
      mutate(Group = 0005) %>%
      mutate(SubGroup = "All 2 digits aggregates")
    
    priorityFour <-
      otherCommodities %>%
      filter(stringr::str_length(Commodity.Code) == 4) %>%
      select(-cc4, -cc6) %>%
      left_join(
        priorityTree %>%
          select(Commodity.Code, Commodity) %>%
          rename(SubGroup = Commodity),
        by = c("cc2" = "Commodity.Code")
      ) %>%
      select(-cc2) %>%
      mutate(Group = 0006)
    
    priorityFive <-
      otherCommodities %>%
      filter(stringr::str_length(Commodity.Code) == 6) %>%
      mutate(cc4 = str_c(str_replace_na(cc2, ""), str_replace_na(cc4, ""))) %>% 
      select(-cc2, -cc6) %>%
      left_join(
        priorityFour %>%
          select(Commodity.Code, Commodity) %>%
          rename(SubGroup = Commodity),
        by = c("cc4" = "Commodity.Code")
      ) %>%
      select(-cc4) %>%
      mutate(Group = 0007)
    
    data <-
      bind_rows(priorityOne, priorityTwo, priorityTwoAndHalf, priorityTree, priorityFour, priorityFive) %>% 
      filter(Commodity.Code %in% existingCommodities)
    dataList <- data %>%
      select(Group, SubGroup) %>%
      distinct()
    tidyList <-
      dataList$SubGroup %>%
      map(function(x) {
        x <- data %>%
          filter(SubGroup == x)
        # browser()
        names <- str_c(str_replace_na(x$Commodity, "")) %>% 
          str_trunc(trunkSymbols)
        setNames(
          x$Commodity.Code,names
        )
      })
    
    output <- setNames(as.list(tidyList), dataList$SubGroup) 
    
    if (orderedDataFrame) {
      output <- data
    }
    
    output
  }



