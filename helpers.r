
# modifying behaviour of qtnorm to deal with its errors when the specified parameters fall outside allowed values (e.g. when mu>upper and sd==0), returning 0
qtnorm_possibly <- possibly(qtnorm, otherwise = 0)


# Function to create icons and labels with info about parameters
label.help <- function(label, id){
   HTML(paste0(label, actionLink(id,label=NULL,icon=icon('info-circle'))))
}


# # generate input slider widget for Normal distributions
# NormSliderInput <- function(title, paramID, specID, varName, E_value=50, E_min=1, E_max=100, SD_value=50, SD_min=1, SD_max=100){
#   # wellPanel(width=5,
#   #   style = "padding: 5px;",
#   div(
#     h4(title),
#     sliderInput(inputId = paste0("sldInput_", paramID, "_E_", specID), 
#                 label = paste0("Mean ", varName, ":"), 
#                 min = E_min, max = E_max, step = 1, 
#                 value = E_value, ticks = FALSE),
#     
#     sliderInput(inputId = paste0("sldInput_", paramID, "_SD_", specID), 
#                 label = paste0("SD of ", varName, ":"), 
#                 min = SD_min, max = SD_max, step = 1, 
#                 value = SD_value, ticks = FALSE)
#   )
# }





NormNumericInput <- function(paramID, specID, varName, infoId="foo", infoText ="",
                             E_value=50, E_min=1, E_max=100, E_step=1, 
                             SD_value=50, SD_min=1, SD_max=100, SD_step=1,
                             via_InsertUI = FALSE){
  
  if(via_InsertUI==FALSE){
    
    toolTip <- bsTooltip(
      #id = paste0("lbl_", paramID),
      id = infoId,
      title = infoText,
      options = list(container = "body"),
      placement = "right", trigger = "hover")
    
  }else{
    toolTip <- NULL
  }
  
  # wellPanel(width=5,
  #   style = "padding: 5px;",
  div(
    splitLayout(
      cellWidths = "50%",
      numericInput(inputId = paste0("numInput_", paramID, "_E_", specID),
                   label = label.help(varName, infoId), #varName,
                   min = E_min, max = E_max, step = E_step,
                   value = E_value, width = '90%'),

      numericInput(inputId = paste0("numInput_",  paramID, "_SD_", specID),
                   label = paste0("SD of ", varName),
                   #label = "Std. Dev.",
                   min = SD_min, max = SD_max, step = SD_step,
                   value = SD_value, width = '90%')
    ),
    toolTip
  )
}






selectSpecies_UITabBuilder <- function(specName, tabName, specLabel, session, startUpValues){
  
  plotWidth <- 350
  plotHeight <- 200
  
  tabItem(
    tabName = tabName,
    fluidRow(
      box(
        title = paste0(specName, " parameters"),
        width = 12,
        status = "primary", 
        solidHeader = TRUE,
        #side = "right",
        
        
        # tabPanel(
        #   title = "Biometric and behaviour features",
        #   
          # fileInput(inputId = paste0("upldInput_dt_biom_", specLabel), label = h3("Upload data")),
          # hr(),
          # h4("Flight Type", tipify(actionLink(inputId = paste0("lbl_flType_", specLabel), label=NULL, icon=icon('question-circle')),
          #                          title="Type of flight", trigger = "hover", placement = "right")),
          
          fluidRow(
            column(width = 4,
                   box(width=12, 
                       selectInput(inputId = paste0("slctInput_biomPars_flType_tp_", specLabel), 
                                   label = label.help("Flight Type", paste0("lbl_flType_", specLabel)), 
                                   #label = "Flight Type",
                                   choices = list("Flapping", "Glidding"))
                   )
            ),
            column(width = 4,
                   box(width = 12,
                       NormNumericInput(paramID = "biomPars_bodyLt", specID = specLabel, 
                                        varName = "Body Length (m)",
                                        infoId = paste0("lbl_bodyLt_", specLabel),
                                        via_InsertUI = TRUE,
                                        E_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$bodyLt_E, 1), 
                                        E_min=0, E_max=5, E_step = 0.01,
                                        SD_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$bodyLt_SD, 0), 
                                        SD_min = 0, SD_step = 0.001),
                       h5(paste0("PDF of ", specName, "'s body length")),
                       plotOutput(paste0("plot_biomPars_bodyLt_", specLabel), width = plotWidth, height = plotHeight)
                       # ,
                       # textAreaInput(inputId = paste0("textInput_bioParsSrc_bodyLt_", specLabel), label = NULL, value = "", 
                       #               placeholder = "If possible, description of the source(s) for the specified parameter values", 
                       #               width = "90%", rows = 6)
                   )
            ),
            column(width = 4,
                   box(width = 12,
                       NormNumericInput(paramID = "biomPars_wngSpan", specID = specLabel, 
                                        varName = "Wing Span (m)",
                                        infoId = paste0("lbl_wngSpan_", specLabel),
                                        via_InsertUI = TRUE,
                                        E_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$wngSpan_E, 1), 
                                        E_min=0, E_step = 0.01,
                                        SD_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$wngSpan_SD, 0), 
                                        SD_min = 0, SD_step = 0.001),
                       h5(paste0("PDF of ", specName, "'s wing span")),
                       plotOutput(paste0("plot_biomPars_wngSpan_", specLabel), width = plotWidth, height = plotHeight)
                   )
            )
          ),
          br(),
          br(),
          
          fluidRow(
            column(width = 4,
                   box(width = 12,
                       NormNumericInput( paramID = "biomPars_flSpeed", specID = specLabel, 
                                        varName = "Flight Speed (m/s)",
                                        infoId = paste0("lbl_flSpeed_", specLabel),
                                        via_InsertUI = TRUE,
                                        E_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$flSpeed_E, 1), 
                                        E_min=0, E_step = 0.01,
                                        SD_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$flSpeed_SD, 0), 
                                        SD_min = 0, SD_step = 0.01),
                       h5(paste0("PDF of ", specName, "'s flight speed")),
                       plotOutput(paste0("plot_biomPars_flSpeed_", specLabel), width = plotWidth, height = plotHeight)
                   )
            ),
            column(width = 4,
                   box(width = 12, 
                       NormNumericInput(paramID = "biomPars_noctAct", specID = specLabel, 
                                        varName = "Nocturnal Activity",
                                        infoId = paste0("lbl_noctAct_", specLabel),
                                        via_InsertUI = TRUE,
                                        E_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$noctAct_E, 1), 
                                        E_min=0, E_step = 0.001,
                                        SD_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$noctAct_SD, 0), 
                                        SD_min = 0, SD_step = 0.0001),
                       h5(paste0("PDF of ", specName, "'s Nocturnal Activity")),
                       plotOutput(paste0("plot_biomPars_noctAct_", specLabel), width = plotWidth, height = plotHeight)
                   )
            ),
            column(width = 4,
                   box(width = 12, 
                       NormNumericInput(paramID = "biomPars_CRHeight", specID = specLabel, 
                                        varName = "Proportion at CRH",
                                        infoId = paste0("lbl_CRHeight_", specLabel),
                                        via_InsertUI = TRUE,
                                        E_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$CRHeight_E, 1), 
                                        E_min=0, E_step = 0.01,
                                        SD_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$CRHeight_SD, 0), 
                                        SD_min = 0, SD_step = 0.001),
                       h5(paste0("PDF of ", specName, "'s Collision Risk Height")),
                       plotOutput(paste0("plot_biomPars_CRHeight_", specLabel), width = plotWidth, height = plotHeight)
                   )
            )
          ),
          br(),
          br(),
          
          fluidRow(
            column(width = 4,
                   box(width = 12, 
                       NormNumericInput(paramID = "biomPars_basicAvoid", specID = specLabel, 
                                       varName = "Basic Avoidance",
                                       infoId = paste0("lbl_basicAvoid_", specLabel),
                                       via_InsertUI = TRUE,
                                       E_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$basicAvoid_E, 1), 
                                       E_min=0, E_step = 0.0001,
                                       SD_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$basicAvoid_SD, 0), 
                                       SD_min = 0, SD_step = 0.0001),
                       h5(paste0("PDF of ", specName, "'s basic avoidance")),
                       plotOutput(paste0("plot_biomPars_basicAvoid_", specLabel), width = plotWidth, height = plotHeight)
                   )
            ),
            column(width = 4,
                   box(width = 12, 
                       NormNumericInput(paramID = "biomPars_extAvoid", specID = specLabel, 
                                        varName = "Extended Avoidance",
                                        infoId = paste0("lbl_extAvoid_", specLabel),
                                        via_InsertUI = TRUE,
                                        E_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$extAvoid_E, 1), 
                                        E_min=0, E_step = 0.0001,
                                        SD_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$extAvoid_SD, 0), 
                                        SD_min = 0, SD_step = 0.0001),
                       h5(paste0("PDF of ", specName, "'s extended avoidance")),
                       plotOutput(paste0("plot_biomPars_extAvoid_", specLabel), width = plotWidth, height = plotHeight)

                   )
            )
          ),
          br(),
          br(),
          
          fluidRow(
            box(width = 12,
                
                tags$b(HTML(paste0("Monthly Densities", actionLink(paste0("lbl_monthOPs_", specLabel), label=NULL, icon=icon('info-circle'))))),
                br(),
                br(),
                br(),
                #label = label.help("Flight Type", paste0("lbl_flType_", specLabel)), 
                #title = tags$b("Monthly Densities"),
                rHandsontableOutput(paste0("hotInput_birdDensPars_", specLabel), width = "100%"),
                #p("Density = birds/km^2"),
                tags$style(type="text/css", paste0("#hotInput_birdDensPars_", specLabel, " th {font-weight:bold;}")),
                br(),
                br(),
                fluidRow(
                  column(width=8, offset = 2, 
                         plotOutput(paste0("plot_birdDensPars_", specLabel), width = 800, height = 350)
                  )
                )
            )
          ),
        
        # Include bsTooltips with info on parameters
        uiOutput(paste0("BiomBStoolTips_", specLabel))
      )
    )
  )
}
