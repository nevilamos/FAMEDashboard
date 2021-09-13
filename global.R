library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(thematic)
library(plotly)
# library(FAMEFMR)
library(readr)
library(qs)
library(shinydashboard)
library(shinyWidgets)


thematic::thematic_shiny()

#' Calculate Geometric mean
#'
#' @details Calculates geometric mean of a vector of positive numeric values
#' @param x numeric vector
#'
#' @return numeric or NA if values are not all > 0
#' @export
geoMean <- function(x) {
  if (any(is.na(x))) {
    y <- NA
    warning("vector contains NA values")
  } else if (any(x <= 0)) {
    y <- NA
    warning("Not all x are positive values")
  } else {
    y <- prod(x)^(1 / length(x))
  }
  return(y)
}



tfiLevels <- c(
  "NO DATA",
  "NONE",
  "ABOVE_MAX_TFI",
  "WITHIN_TFI",
  "BELOW_MIN_TFI"
)

tfiScale <- function(...) {
  ggplot2:::manual_scale("fill",
    values = setNames(
      c(
        "grey80",
        "grey60",
        "blue4",
        "deepskyblue2",
        "orange1"
      ),
      tfiLevels
    )
  )
}

# reads in data ( from FAME v3  qs file)
# library(qs)

myDataList <- qread("../FameShiny/FH_Outputs/75mwith5spDashboardDemo.qs") # "./data/DashboardSessionDumpAllSpeciesDefaultEOWholeStateFHwith1755.qs")
TFI <- myDataList$TFI %>%
  ungroup() %>%
  mutate(TFI_STATUS = as.character(TFI_STATUS)) %>%
  mutate(TFI_STATUS = replace_na(TFI_STATUS, "NO DATA")) %>%
  mutate(TFI_STATUS = factor(
    x = TFI_STATUS,
    levels = tfiLevels
  )) %>%
  filter(!is.na(EFG_NAME))

efgNames <- sort(unique(TFI$EFG_NAME))
fireRegions <- sort(unique(TFI$FIRE_REGION_NAME))
delwpRegions <- sort(unique(TFI$DELWP_REGION))
seasons <- sort(unique(TFI$SEASON))
fireFMZ <- sort(unique(TFI$FIRE_FMZ_NAME))
fireDistricts<-levels(TFI$DISTRICT_N)
BBTFI <<- myDataList$BBTFI %>%
  ungroup() %>%
  filter(!is.na(EFG_NAME)) %>%
  filter(!is.na(TBTFI)) %>%
  mutate(TBTFI = ifelse(TBTFI > 4, "5+", as.character(TBTFI))) %>%
  mutate(TBTFI = factor(x = TBTFI, levels = c("5+", as.character(4:1)))) %>%
  rename(SEASON = SEAS)

RA <<- myDataList$RA

TaxonList <- myDataList$TaxonList %>%
  mutate(VIC_ADVISORY_STATUS = ifelse(is.na(VIC_ADVISORY_STATUS), "Least Concern", VIC_ADVISORY_STATUS))
