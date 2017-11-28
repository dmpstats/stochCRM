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
  windfarmPars_targetPower = 600,
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
  rotorSpeed_E = 80,
  rotorSpeed_SD = 5,
  hubHght_E = 26.5,
  hubHght_SD = 2,
  maxBladeWdth_E = 5.5,
  maxBladeWdth_SD=0.3,
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
  basicAvoid_E = 0.9893,
  basicAvoid_SD = 0.0007,
  extAvoid_E = 0.9672,
  extAvoid_SD = 0.0018
  
)





