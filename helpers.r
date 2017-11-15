

label.help <- function(label, id){
   HTML(paste0(label, actionLink(id,label=NULL,icon=icon('info-circle'))))
}


# generate input slider widget for Normal distributions
NormSliderInput <- function(title, paramID, specID, varName, E_value=50, E_min=1, E_max=100, SD_value=50, SD_min=1, SD_max=100){
  # wellPanel(width=5,
  #   style = "padding: 5px;",
  div(
    h4(title),
    sliderInput(inputId = paste0("sldInput_", paramID, "_E_", specID), 
                label = paste0("Mean ", varName, ":"), 
                min = E_min, max = E_max, step = 1, 
                value = E_value, ticks = FALSE),
    
    sliderInput(inputId = paste0("sldInput_", paramID, "_SD_", specID), 
                label = paste0("SD of ", varName, ":"), 
                min = SD_min, max = SD_max, step = 1, 
                value = SD_value, ticks = FALSE)
  )
}



NormNumericInput <- function(title, paramID, specID, varName, E_value=50, E_min=1, E_max=100, SD_value=50, SD_min=1, SD_max=100){
  # wellPanel(width=5,
  #   style = "padding: 5px;",
  div(
    #h4(title), 
    splitLayout(
      cellWidths = "50%",
      numericInput(inputId = paste0("numInput_", paramID, "_E_", specID), 
                   #label = paste0("Mean ", varName, ":"),
                   #label = "Mean", 
                   label = varName,
                   min = E_min, max = E_max, step = 1, 
                   value = E_value, width = '90%'),
      
      numericInput(inputId = paste0("numInput_",  paramID, "_SD_", specID), 
                   label = paste0("SD of ", varName),
                   #label = "Std. Dev.", 
                   min = SD_min, max = SD_max, step = 1, 
                   value = SD_value, width = '90%')
    )
  )
}





selectSpecies_UITabBuilder <- function(specName, tabName, specLabel, session){
  
  plotWidth <- 350
  plotHeight <- 200
  
  tabItem(
    tabName = tabName,
    fluidRow(
      tabBox(
        title = tags$b(paste0(specName, " parameters")),
        width = 12,
        side = "right",
        
        
        tabPanel(
          title = "Biometric and behaviour features",
          
          # fileInput(inputId = paste0("upldInput_dt_biom_", specLabel), label = h3("Upload data")),
          # hr(),
          
          # h4("Flight Type", tipify(actionLink(inputId = paste0("lbl_flType_", specLabel), label=NULL, icon=icon('question-circle')), 
          #                          title="Type of flight", trigger = "hover", placement = "right")),
          
          fluidRow(
            column(width = 4,
                   box(width=12, 
                       selectInput(inputId = paste0("slctInput_biomPars_flType_tp_", specLabel), label = "Flight Type",
                                   choices = list("Flapping", "Glidding"))
                   ))
          ),
          br(),
          br(),
          
          fluidRow(
            column(width = 4,
                   box(width = 12,
                       NormNumericInput(title = "Body Length", paramID = "biomPars_bodyLt", specID = specLabel, 
                                        varName = "Body Length (m)",
                                        E_value=0.39, E_min=0, E_max=5, SD_value=0.005, SD_min = 0),
                       h5(paste0("PDF of ", specName, "'s body length")),
                       plotOutput(paste0("plot_biomPars_bodyLt_", specLabel), width = plotWidth, height = plotHeight)
                   )
            ),
            column(width = 4,
                   box(width = 12,
                       NormNumericInput(title = "Wing Span", paramID = "biomPars_wngSpan", specID = specLabel, 
                                        varName = "Wing Span (m)"),
                       h5(paste0("PDF of ", specName, "'s wing span")),
                       plotOutput(paste0("plot_biomPars_wngSpan_", specLabel), width = plotWidth, height = plotHeight)
                   )
            ),
            column(width = 4,
                   box(width = 12,
                       NormNumericInput(title = "Flight Speed", paramID = "biomPars_flSpeed", specID = specLabel, 
                                        varName = "Flight Speed (m/s)"),
                       h5(paste0("PDF of ", specName, "'s flight speed")),
                       plotOutput(paste0("plot_biomPars_flSpeed_", specLabel), width = plotWidth, height = plotHeight)
                   )
            )
          ),
          br(),
          br(),
          
          fluidRow(
            column(width = 4,
                   box(width = 12, 
                       NormNumericInput(title = "Nocturnal Activity", paramID = "biomPars_noctAct", specID = specLabel, 
                                        varName = "Nocturnal Activity (propn)"),
                       h5(paste0("PDF of ", specName, "'s Nocturnal Activity")),
                       plotOutput(paste0("plot_biomPars_noctAct_", specLabel), width = plotWidth, height = plotHeight)
                   )
            ),
            column(width = 4,
                   box(width = 12, 
                       NormNumericInput(title = "Collision Risk Height", paramID = "biomPars_CRHeight", specID = specLabel, 
                                        varName = "Proportion at CRH"),
                       h5(paste0("PDF of ", specName, "'s Collision Risk Height")),
                       plotOutput(paste0("plot_biomPars_CRHeight_", specLabel), width = plotWidth, height = plotHeight)
                   )
            ),
            column(width = 4,
                   box(width = 12, 
                       NormNumericInput(title = "Basic Avoidance Rate", paramID = "biomPars_basicAvoid", specID = specLabel, 
                                       varName = "Basic Avoidance (prob)"),
                       h5(paste0("PDF of ", specName, "'s basic avoidance")),
                       plotOutput(paste0("plot_biomPars_basicAvoid_", specLabel), width = plotWidth, height = plotHeight)
                   )
            )
          ),
          br(),
          br(),
          
          fluidRow(
            column(width = 4,
                   box(width = 12, 
                       NormNumericInput(title = "Extended Avoidance Rate", paramID = "biomPars_extAvoid", specID = specLabel, 
                                        varName = "Extended Avoidance (prob)"),
                       h5(paste0("PDF of ", specName, "'s extended avoidance")),
                       plotOutput(paste0("plot_biomPars_extAvoid_", specLabel), width = plotWidth, height = plotHeight)
                   )
            )
          )
        ),

        tabPanel(
          title = "Monthly Densities and Flight Height Distribution",
          
          fluidRow(
            box(width = 12,
                title = tags$b("Monthly Densities"),
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
          
          # fluidRow(
          #   box(width = 12, 
          #       title = tags$b("Monthly Densities"),
          #       fileInput(inputId = paste0("upldInput_dt_dens_", specLabel), label = "Upload data", width = "20%"),
          #       wellPanel(#style = "padding: 5px;",
          #         splitLayout(
          #           cellWidths = "7.5%",
          #           p("Mean Density", style = "font-size: 11pt; font-weight: bold; line-height: 10px; margin-top: 35px; text-align: center;"),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Jan_E_", specLabel), label="January", value = NULL),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Feb_E_", specLabel), label="February", value = 0), 
          #           numericInput(inputId = paste0("numInput_birdDensPars_Mar_E_", specLabel), label="March", value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Apr_E_", specLabel), label="April", value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_May_E_", specLabel), label="May", value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Jun_E_", specLabel), label="June" , value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Jul_E_", specLabel), label="June" , value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Aug_E_", specLabel), label="August" , value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Sep_E_", specLabel), label="September" , value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Oct_E_", specLabel), label="October" , value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Nov_E_", specLabel), label="November" , value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Dec_E_", specLabel), label="December" , value = 0)
          #         ),
          #         splitLayout(
          #           cellWidths = "7.5%",
          #           p("SD of Density", style = "font-size: 11pt; font-weight: bold; line-height: 10px; margin-top: 12px; text-align: center;"),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Jan_SD_", specLabel), label=NULL, value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Feb_SD_", specLabel), label=NULL, value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Mar_SD_", specLabel), label=NULL, value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Apr_SD_", specLabel), label=NULL, value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_May_SD_", specLabel), label=NULL, value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Jun_SD_", specLabel), label=NULL, value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Jul_SD_", specLabel), label=NULL, value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Aug_SD_", specLabel), label=NULL, value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Sep_SD_", specLabel), label=NULL, value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Oct_SD_", specLabel), label=NULL, value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Nov_SD_", specLabel), label=NULL, value = 0),
          #           numericInput(inputId = paste0("numInput_birdDensPars_Dec_SD_", specLabel), label=NULL, value = 0)
          #         )
          #       )
          #   )
          # ),
          br(),
          fluidRow(
            box(width = 12,
                h4(tags$b("Flight Height Distribution")),
                column(width = 4, 
                       fileInput(inputId = paste0("upldInput_dt_FlgHghDst_", specLabel), label = "Upload data",  width = "60%", 
                                 accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))), 
                column(width = 7, offset = 1,
                       plotOutput(paste0("plot_FlgHghDst_", specLabel), width = 600, height = 300))
            )
          )
        )

      )
    )
  )
}
