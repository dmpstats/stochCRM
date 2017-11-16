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

# library(dplyr)

source("helpers.r")
source("bandModel_mock.r")

species <- c("Arctic Skua", "Northern Fulmar", "Great Black backed Gull", "Common Guillemot", "Northern Gannet",
             "Black legged Kittiwake", "Lesser Black Backed Gull", "Little Auk", "Atlantic Puffin", 
             "Razorbill", "Arctic Tern", "Black headed Gull", "Black throated Diver", "Common Gull", "Common Scoter",
             "Common Tern", "Cormorant", "Eider", "European Shag", "Herring Gull", "Little Gull", "Manx Shearwater",
             "Red throated Diver", "Sandwich Tern")


# set theme for ggplot
theme_set(theme_bw())


options(shiny.reactlog=TRUE)
options(shiny.error = browser)


startUpValues <- list(
  windfarmPars_targetPower = 600,
  windfarmPars_Latitude = 55.8,
  windfarmPars_width = 10,
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
  maxBladeWdth_SD=0.3
)





