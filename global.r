#' =============================================================================================================
#' --- Version Logs ---------------
#' 
#'  - v2.1.1: Updated with the latest version of the sCRM model function - Carl made it faster to run and cleaned the R code 
#' 

library(shiny)
library(shinydashboard)
library(rhandsontable)
library(plyr)
library(tidyverse)
library(magrittr)
library(stringr)
library(shinyBS)
library(msm)
library(shinyjs)
library(shinyWidgets)
library(data.table)
library(DT)
library(zip)
library(RColorBrewer)
library(pracma)


#devtools::install_github("rstudio/d3heatmap")
library(d3heatmap)

# library(dplyr)

source("helpers.r")
#source("bandModel_mock.r")
source("BandModel_function.R")

species <- sort(c("Arctic Skua", "Northern Fulmar", "Great Black backed Gull", "Common Guillemot", "Northern Gannet",
             "Black legged Kittiwake", "Lesser Black Backed Gull", "Little Auk", "Atlantic Puffin", 
             "Razorbill", "Arctic Tern", "Black headed Gull", "Black throated Diver", "Common Gull", "Common Scoter",
             "Common Tern", "Cormorant", "Eider", "European Shag", "Herring Gull", "Little Gull", "Manx Shearwater",
             "Red throated Diver", "Sandwich Tern"))


# set theme for ggplot
theme_set(theme_bw())


options(shiny.reactlog=TRUE)
options(shiny.error = browser)


startUpValues <- list(
  windfarmPars_nTurbines = 100,
  windfarmPars_Latitude = 55.8,
  windfarmPars_width = 10,
  turbPower = 6,
  numBlades = 3,
  tidalOffset = 2.5,
  windSpeed_E = 7.74,
  windSpeed_SD = 3.2,
  meanDensity = c(0.97, 1.04, 1.15, 0.48, 0.56, 0.63, 0.68, 0.64, 0.53, 1.20, 1.02, 0.99),
  sdDensity = c(0.67, 0.75, 0.78, 0.36, 0.58, 0.45, 0.47, 0.47, 0.39, 0.78, 0.61, 0.7),
  trucDensity = c(0,2),
  rotorRadius = 80,
  airGap = 26.5,
  maxBladeWdth = 5.5,
  windAvail = c(96.28, 96.53, 95.83, 92.78, 90.86, 92.22, 89.11, 89.92, 93.71, 96.14, 97.14, 96.41),
  meanDownTime = rep(6.3, 12), 
  sdDownTime = rep(2, 12),
  rotnSpeed_E = 10, 
  rotnSpeed_SD = 0.5,
  bladePitch_E = 2,
  bladePitch_SD = 0.1,
  rotationVsWind_df = tibble(windSpeed = 0:29, rotationSpeed=c(rep(0,3), rep(6.8, 5), 8.1, 9.1, 9.3, 9.4, 
                                                                   9.5, rep(9.7, 2), 9.9, rep(10.2, 14))),
  pitchVsWind_df = tibble(windSpeed = 0:29, bladePitch=c(rep(90, 3), rep(0, 8), 4, 7, 9, 11, 13, 15, 
                                                                16, 18, 19, 20, 22, 23, 24, 25, 26, 27, 28, 29, 30)),
  bodyLt_E = 0.39,
  bodyLt_SD = 0.005,
  wngSpan_E = 1.08,
  wngSpan_SD = 0.04,
  flSpeed_E = 7.26,
  flSpeed_SD = 1.5,
  noctAct_E = 0.033, 
  noctAct_SD = 0.0045,
  CRHeight_E = 0.06,
  CRHeight_SD = 0.009,
  basicAvoid_E = 0.989,
  basicAvoid_SD = 0.001,
  extAvoid_E = 0.967,
  extAvoid_SD = 0.002
  
)

# Remove output files left-over from past sessions
file.remove(list.files("shinyOutputs/inputs", full.names = TRUE))
file.remove(list.files("shinyOutputs/outputs", full.names = TRUE))


# template data sets
template_FHD <- data.frame(Height_m = 1:300, matrix(0, nrow = 300, ncol = 200, dimnames = list(NULL,  paste0("bootId_", 1:200))))

template_monthDens_summaries <- data.frame(referencePoints = c("Minimum", "2.5th %tile", "5th %tile", "10th %tile", "25th %tile", "50th %tile", "75th %tile", 
                                                               "90th %tile", "95th %tile", "97.5th %tile", "Maximum"), 
                                           matrix(0, nrow = 11, ncol = 12, dimnames = list(NULL,  month.name)))

template_monthDens_samples <- data.frame(matrix(0, nrow = 1000, ncol = 12, dimnames = list(NULL,  month.name)))

turbineOpTempTable <- data.frame(array(0, dim = c(3, 12), dimnames = list(c("Wind Availability (%)", "Mean Downtime (%)", "SD Downtime (%)"), month.name)),
                                 stringsAsFactors = FALSE)




# generate continuous Spectral pallete
Spectral_pal_cont <- colorRampPalette(rev(brewer.pal(11,"Spectral")))
YlOrRd_pal_cont <- colorRampPalette(c("white", brewer.pal(9,"YlOrRd")))
PuBuGn_pal_cont <- colorRampPalette(brewer.pal(9,"PuBuGn"))
YlOrBr_pal_cont <- colorRampPalette(c("white",brewer.pal(9,"YlOrBr")))
manual1_pal_cont <- colorRampPalette(rev(c("#8E063B", "#AB4147", "#C56551", "#DA8459", "#E99F61", "#F2B669", "#F6C971", "#F4D97B", "#EDE388", "#E2E6BD", "#e0e2cc", "#f1f2e6")))
manual2_pal_cont <- colorRampPalette(rev(c("#7D0112", "#8E2C19", "#9E4723", "#AD5F30", "#BC763E", "#C88C4F", "#D4A261", "#DEB675", "#E6C98A", "#ECDAA0", "#F1E9B8", "#F2F1E4")))


