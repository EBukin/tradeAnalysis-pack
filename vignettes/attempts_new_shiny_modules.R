# Developing new modules for the app


library(tidyverse)
library(tradeAnalysis)
library(stringr)
library(readr)
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

folder <- "inst/shiny-examples/trade-data-explorer/helpers/"
lapply(file.path(folder,list.files(folder, ".R$")), source)

source("inst/shiny-examples/trade-data-explorer/ui.R")
source("inst/shiny-examples/trade-data-explorer/server.R")

app <- shinyApp(ui, server)

runApp(app)




# mainFolder <- "./ShinyData/"
# allFiles <- file.path(mainFolder, list.files(mainFolder, "*.rds"))
# library(tidyverse)
# library(readr)
# library(tradeAnalysis)
# oneFile <- read_rds(allFiles[1])
# oneFile %>%
#   filter(Partner.Code == 0) %>%
#   rename(Partner.Code = Reporter.Code,
#          Reporter.Code = Partner.Code)
# 
# world <-
#   map_df(
#     allFiles,
#     .f = function(x) {
#       read_rds(x) %>%
#         filter(Partner.Code == 0) %>%
#         rename(Partner.Code = Reporter.Code,
#                Reporter.Code = Partner.Code)
#     }
#   )
# world_DirMis <- 
#   world %>% filter(Type == "Direct")
# world_DirMis <-
#   bind_rows(world %>% filter(Type == "Direct"),
#             world %>%
#               mutate(Type = "Dir-Mir"))
# 
# world_DirMis_com <-
#   world_DirMis %>%
#   filter(Type == "Direct") %>%
#   group_by(Year,
#            Type,
#            Period,
#            Trade.Flow.Code,
#            Reporter.Code,
#            Commodity.Code,
#            Variable) %>%
#   summarise(Value = sum(Value, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(Partner.Code = 0) %>%
#   bind_rows(world_DirMis)
# 
# write_rds(x = world_DirMis_com, path = "ShinyData/0.rds", compress = "gz")
# 
# world %>%
#   group_by_(.dots = names(.)[!names(.)%in%c("Type", "Value")])

