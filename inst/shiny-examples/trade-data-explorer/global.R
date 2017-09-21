library(tidyverse)
library(tradeAnalysis)
library(stringr)
library(readr)
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

folder <- "./helpers/"
lapply(file.path(folder,list.files(folder, ".R$")), source)