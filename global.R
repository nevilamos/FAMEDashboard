library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(thematic)
library(plotly)
library(FAMEFMR)
library(readr)
library(qs)
library(shinydashboard)
library(shinyWidgets)

tfiLevels <- c(
  "NO DATA",
  "NONE",
  "ABOVE_MAX_TFI",
  "WITHIN_TFI",
  "BELOW_MIN_TFI"
)


# reads in data ( from FAME qs file)


fireDistricts<-fireFMZ <- delwpRegions <-fireRegions <-efgNames <- myDataList <- NULL

seasons = 0
myDataPath<-"./data/DashboardDemo.qs"
