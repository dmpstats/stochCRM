


#################################################################
# Dashboard header
#################################################################

header <- dashboardHeader(
  titleWidth =270,
  title = "Stochastic Avian CRM"
)




#################################################################
# Dashboard sidebar
#################################################################

sidebar <- dashboardSidebar(
  width = 270,
  
  tags$style(".main-sidebar{ position: fixed;}"),
  
  sidebarMenu(
    id = "tabs",
    #style = "position: fixed; overflow: visible;",

    menuItem(
      "Step 1: Turbine & Wind farm features", tabName = "tab_turbWindPars", icon = icon("tachometer")
    ),
    
    menuItem(text = "Step 2: Specie(s)", icon = icon("bullseye"),
      selectizeInput(inputId = "selectSpecs",  width = "100%", label=NULL,  # label = "Step 1: Specie(s)",
                     choices = species, multiple=TRUE,
                     selected = "Black legged Kittiwake", 
                     options = list(placeholder = "Select from list or add new species", create = TRUE)
      )
    ),
      
    menuItem(#selected = TRUE,
      "Step 3: Species features", tabName = "speciesTab", icon = icon("twitter"),
      menuItemOutput("menuSubItems_species")
      #  menuSubItem("Species 1", tabName = "tab_species1Pars", icon=icon("sliders"))
    ),
    
    
    menuItem(#selected = TRUE,
      "Step 4: Simulation & Results", tabName = "tab_simulation", icon = icon("bar-chart")
    ),#,
    # menuItem(
    #   "test Item", tabName = "tab_testTab", icon = icon("sliders")
    # ),
    
    hr(),
    
    bsAlert(anchorId = "alert")
    
  )#,

  #actionButton("console","server console")
)


#################################################################
# Dashboard body
#################################################################

body <- dashboardBody(
  
  
  #tabItems(
  div(class="tab-content", id="tabItemsEnvelope",  # required as reference to the dynamic UI tab for each species via insertUI()
    tabItem(tabName="tab_turbWindPars",
            fluidRow(
              box(title = "Wind farm features", 
                  width = 2, 
                  status = "primary", 
                  solidHeader = TRUE,
                  
                  #helpText("<Brief description of windfarm features here>"),
                  
                  # --- Turbine Power
                  numericInput(width = "85%", 
                               inputId = "numInput_windfarmPars_nTurbines", 
                               label = label.help("Number of Turbines", "lbl_windfarmnTurbines"),
                               value = startUpValues$windfarmPars_nTurbines, min = 1, step = 1),
                  bsTooltip(id = "lbl_windfarmnTurbines", 
                            title = paste0("Total number of turbines in the wind farm array.", 
                            " Used in conjunction with turbine model to calculate the target power of the wind farm."),
                            options = list(container = "body"), placement = "right", trigger = "hover"),
 
                  # ---  Latitude
                  numericInput(width = "85%", 
                               inputId = "numInput_windfarmPars_Latitude", 
                               label = label.help("Latitude (deg)", "lbl_windfarmLatitude"), #"Latitude (deg)", 
                               value = startUpValues$windfarmPars_Latitude, min = -90, max = 90, step = 0.01),
                  bsTooltip(id = "lbl_windfarmLatitude", 
                            title = paste0("Latitude of the wind farm in decimal degrees. Used to calculate day length at the site over the year."),
                            options = list(container = "body"), placement = "right", trigger = "hover"),
                  
                  # --- Width
                  numericInput(width = "85%", 
                               inputId = "numInput_windfarmPars_width", 
                               label = label.help("Width (Km)", "lbl_windfarmWidth"), #"Width (Km)", 
                               value = startUpValues$windfarmPars_width, min = 1, step = 1), 
                  bsTooltip(id = "lbl_windfarmWidth", 
                            title = paste0("The wind farm width"),
                            options = list(container = "body"), placement = "right", trigger = "hover"),
                  
                  # --- Tidal offset
                  numericInput(width = "85%", 
                               inputId = "numInput_windfarmPars_tidalOffset", 
                               label = label.help("Tidal Offset (m)", "lbl_tidalOffset"), #"Tidal Offset (m)", 
                               value = startUpValues$tidalOffset, min = 0, step = 0.1),
                  bsTooltip(id = "lbl_tidalOffset", 
                            title = paste0("Tidal offset to correct for flight heights calculated in relation to sea-level",
                                           " and turbine dimensions calculated in relation to Highest Astronomical Tide"),
                            options = list(container = "body"), placement = "right", trigger = "hover"),
                  
                  br(),
                  # --- Upwind/Downwind proportion of flights
                  # sliderInput(width = "85%", 
                  #             inputId = "sldInput_windfarmPars_upWindDownWindProp", 
                  #             label = label.help("Upwind flights (%)", "lbl_upWindDownWindProp"), #"up/downwind ratio of flights", 
                  #             value = 50, min = 0, max = 100, step = 10),
                  
                  knobInput(
                    inputId = "sldInput_windfarmPars_upWindDownWindProp",
                    label = label.help("Upwind flights (%)", "lbl_upWindDownWindProp"),
                    value = 50, min = 0, max = 100, step = 1,
                    displayPrevious = TRUE,
                    thickness = 0.4,
                    lineCap = "default", #"round",
                    inputColor = "#333",
                    fgColor = "#6B8E23",
                    angleArc = 180,
                    angleOffset = 270, 
                    width = "80%"
                    # height = "140px"
                  ),
                  
                  bsTooltip(id = "lbl_upWindDownWindProp", 
                            title = paste0("The percentage of upwind bird flights. Should be 50% unless direction of travel", 
                                           " is biased in a particular direction"),
                            options = list(container = "body"), placement = "right", trigger = "hover")
              ),
              
              box(title = "Turbine parameters", 
                  width = 10, 
                  status = "primary", 
                  solidHeader = TRUE,
                  
                  fluidRow(
                    box(width = 12, 
                      splitLayout(
                        
                        # --- Turbine power/model
                        numericInput(width = "60%",
                                     inputId = "numInput_turbinePars_turbinePower",
                                     label = label.help("Turbine Model (MW)", "lbl_turbinePower"),
                                     value = startUpValues$turbPower, min = 1),

                        # ---- Number of blades
                        numericInput(width = "60%",
                                     inputId = "numInput_turbinePars_numBlades",
                                     label =  label.help("No. of blades", "lbl_numBlades"),
                                     value = startUpValues$numBlades, min = 1),

                        # ---- Rotor radius
                        numericInput(width = "60%",
                                     inputId = "numInput_turbinePars_rotRadius",
                                     label =  label.help("Rotor Radius (m)", "lbl_rotorRadius"),
                                     value = startUpValues$rotorRadius, min = 1, step = 0.1),
                        
                        # ---- Air Gap
                        numericInput(width = "60%", 
                                     inputId = "numInput_turbinePars_airGap",
                                     label =  label.help("Air Gap (m)", "lbl_turbineAirGap"),
                                     value = startUpValues$airGap, min = 1, step = 0.5),
                        
                        # Maximum blade width
                        numericInput(width = "60%", 
                                     inputId = "numInput_turbinePars_maxBladeWdth",
                                     label =  label.help("Max blade width (m)", "lbl_maxBladeWdth"),
                                     value = startUpValues$maxBladeWdth, min = 1, step = 0.1)
                        
                      )
                    ),
                    bsTooltip(id = "lbl_turbinePower",
                              title = paste0("The power output of each turbine"),
                              options = list(container = "body"), placement = "right", trigger = "hover"),
                    bsTooltip(id = "lbl_numBlades",
                              title = paste0("Number of blades in each turbine"),
                              options = list(container = "body"), placement = "right", trigger = "hover"),
                    bsTooltip(id = "lbl_rotorRadius",
                              title = paste0("The distance from the axis of rotation to blade tip"),
                              options = list(container = "body"), placement = "right", trigger = "hover"),
                    bsTooltip(id = "lbl_turbineAirGap",
                              title = paste0("Tip clearance height, i.e. the distance between the lowest rotor tip height", 
                                             " and sealevel (measured as Highest Astronomical Tide)"),
                              options = list(container = "body"), placement = "right", trigger = "hover"),
                    bsTooltip(id = "lbl_maxBladeWdth",
                              title = paste0("The maximum width of the rotor blade"),
                              options = list(container = "body"), placement = "right", trigger = "hover")
                  ),
                  
                  br(),
                  fluidRow(
                    box(width = 12, 
                        
                        tags$b(HTML(paste0("Monthly Operation", actionLink("lbl_monthOPs", label=NULL, 
                                                                           icon=icon('info-circle'))))),
                        bsTooltip(id = "lbl_monthOPs", 
                                  title = paste0("Information on turbine activity per month: % of wind availability (treated as constant) and % of", 
                                                 " maintenance downtime (~ Normal)"),
                                  options = list(container = "body"), placement = "right", trigger = "hover"),
                        
                        br(),
                        br(),
                        
                        rHandsontableOutput("hotInput_turbinePars_monthOps", width = "100%"),
                        tags$style(type="text/css", "#hotInput_turbinePars_monthOps th {font-weight:bold;}"),
                        br(),
                        br(),
                        column(6,
                               plotOutput("plot_turbinePars_monthOps_windAvb", width = "100%", height = 250)
                               ),
                        column(6,
                               plotOutput("plot_turbinePars_monthOps_downtime", width = "100%", height = 250)
                               )
                    )
                  ),
                  br(),
                  fluidRow(
                    box(width = 12,
                        tags$b(HTML(paste0("Rotation Speed and Blade Pitch"))),
                        br(),
                        br(),
                        helpText("Choose between simulating rotor speed and pitch from probability distributions OR from a relationship with wind speed"),
                        fluidRow(
                          column(4, 
                                 radioGroupButtons(inputId = "radGrpInput_rotationAndPitchOption",
                                                   individual = TRUE,
                                                   justified = TRUE, 
                                                   label = NULL,
                                                   choices = c("Probability distributions" = "probDist",
                                                               "Wind Speed relationship" = "windSpeedReltn"),
                                                   checkIcon = list(yes = tags$i(class = "fa fa-circle",
                                                                                 style = "color: steelblue"),
                                                                    no = tags$i(class = "fa fa-circle-o",
                                                                                style = "color: steelblue")))
                          )),
                        br(),
                        br(),
                        conditionalPanel(
                          condition = "input.radGrpInput_rotationAndPitchOption == 'probDist'",
                          fluidRow(
                            column(4,
                                   box(width = 12,
                                   NormNumericInput(paramID = "turbinePars_rotnSpeed", specID = "",
                                                    varName = "Rotation (rpm)",
                                                    infoId = "lbl_rotSpeedProbDist",
                                                    infoText = paste0("Turbine rotor speed (~ Truncated Normal with lower bound at 0). ", 
                                                                      "SD should be 0 unless suitable info on rotor speed variability is available"),
                                                    E_value = startUpValues$rotnSpeed_E, SD_value = startUpValues$rotnSpeed_SD),
                                   plotOutput("plot_turbinePars_rotnSpeed", width = 310, height = 200),
                                   br(),
                                   verbatimTextOutput("qtls_turbinePars_rotnSpeed")
                                   )
                            ),
                            column(4,
                                   box(width = 12,
                                       NormNumericInput(paramID = "turbinePars_bladePitch", specID = "",
                                                        varName = "Pitch (deg)",
                                                        infoId = "lbl_bladePitchProbDist",
                                                        infoText = paste0("Blade pitch, i.e. the angle of the blade relative to rotor plane (~Truncated Normal with lower bound at 0). ", 
                                                                          "SD should be 0 unless suitable info on pitch variability is available"),
                                                        E_value = startUpValues$bladePitch_E, SD_value = startUpValues$bladePitch_SD),
                                       plotOutput("plot_turbinePars_bladePitch", width = 310, height = 200),
                                       br(),
                                       verbatimTextOutput("qtls_turbinePars_bladePitch")
                                   )
                            )
                          )
                        ),
                        conditionalPanel(
                          condition = "input.radGrpInput_rotationAndPitchOption == 'windSpeedReltn'",
                          fluidRow(
                            column(4, 
                                   rHandsontableOutput("hotInput_turbinePars_rotationVsWind", width = "100%"),
                                   tags$style(type="text/css", "#hotInput_turbinePars_rotationVsWind th {font-weight:bold;}")
                            ),
                            column(4, 
                                   rHandsontableOutput("hotInput_turbinePars_pitchVsWind", width = "100%"),
                                   tags$style(type="text/css", "#hotInput_turbinePars_pitchVsWind th {font-weight:bold;}")
                            ),
                            column(4,
                                   NormNumericInput(paramID = "miscPars_windSpeed", specID = "",
                                                    varName = "Wind Speed (m/s)",
                                                    infoId = "lbl_winSpeed",
                                                    infoText = paste0("Wind speed (~Truncated Normal with lower bound at 0)"),
                                                    E_value = startUpValues$windSpeed_E, SD_value = startUpValues$windSpeed_SD),
                                   plotOutput("plot_miscPars_windSpeed", width = 310, height = 200),
                                   verbatimTextOutput("qtls_miscPars_windSpeed")
                            )
                          )
                        )
                    )
                  )
                  # fluidRow(
                  #   column(4,
                  #          
                  #          # ---- Turbine Rotation Speed 
                  #          box(width = 12, 
                  #              #h5(tags$b("Rotation Speed")),
                  #              radioButtons(inputId = "radButtonInput_turbinePars_rotSpdInputOption", 
                  #                           label = label.help("Rotation Speed", "lbl_rotSpeed"), #"Rotation Speed", 
                  #                           inline = FALSE,
                  #                           choices = list("Probability Distribution" = "probDist",
                  #                                          "Wind Vs. rotation speed relationship"= "windVsRotation"),
                  #                           selected = "probDist"),
                  #              conditionalPanel(
                  #                condition = "input.radButtonInput_turbinePars_rotSpdInputOption == 'windVsRotation'",
                  #                rHandsontableOutput("hotInput_turbinePars_rotationVsWind", width = "100%"),
                  #                tags$style(type="text/css", "#hotInput_turbinePars_rotationVsWind th {font-weight:bold;}")
                  #              ),
                  #              conditionalPanel(
                  #                condition = "input.radButtonInput_turbinePars_rotSpdInputOption == 'probDist'",
                  #                NormNumericInput(paramID = "turbinePars_rotnSpeed", specID = "", 
                  #                                 varName = "Rotation (rpm)",
                  #                                 infoId = "lbl_rotSpeedProbDist", 
                  #                                 infoText = "Rotation speed (~Truncated Normal with lower bound at 0)",
                  #                                 E_value = startUpValues$rotnSpeed_E, SD_value = startUpValues$rotnSpeed_SD),
                  #                plotOutput("plot_turbinePars_rotnSpeed", width = 300, height = 200),
                  #                verbatimTextOutput("qtls_turbinePars_rotnSpeed")
                  #              ), 
                  #              bsTooltip(id = "lbl_rotSpeed", 
                  #                        title = paste0("Rotation speed to be specified via a probability distribution", 
                  #                                       " or a relationship between rotor speed and wind speed"),
                  #                        options = list(container = "body"), placement = "right", trigger = "hover")
                  #          )
                  #   ),
                  #   column(4,
                  #          
                  #          # ---- Turbine Blade Pitch
                  #          box(width = 12, 
                  #              #h5(tags$b("Blade Pitch")),
                  #              radioButtons(inputId = "radButtonInput_turbinePars_bldPitchInputOption", 
                  #                           label =  label.help("Blade Pitch", "lbl_bladePitch"),  #"Blade Pitch", 
                  #                           inline = FALSE,
                  #                           choices = list("Probability Distribution"= "probDist",
                  #                                          "Wind Vs. blade pitch relationship" = "windVsPitch"),
                  #                           selected = "probDist"),
                  #              conditionalPanel(
                  #                condition = "input.radButtonInput_turbinePars_bldPitchInputOption == 'windVsPitch'",
                  #                rHandsontableOutput("hotInput_turbinePars_pitchVsWind", width = "100%"),
                  #                tags$style(type="text/css", "#hotInput_turbinePars_pitchVsWind th {font-weight:bold;}")
                  #              ),
                  #              conditionalPanel(
                  #                condition = "input.radButtonInput_turbinePars_bldPitchInputOption == 'probDist'",
                  #                NormNumericInput(paramID = "turbinePars_bladePitch", specID = "", 
                  #                                 varName = "Pitch (deg)", 
                  #                                 infoId = "lbl_bladePitchProbDist", 
                  #                                 infoText = "Blade pitch (~Truncated Normal with lower bound at 0)",
                  #                                 E_value = startUpValues$bladePitch_E, SD_value = startUpValues$bladePitch_SD),
                  #                plotOutput("plot_turbinePars_bladePitch", width = 300, height = 200),
                  #                verbatimTextOutput("qtls_turbinePars_bladePitch")
                  #              ),
                  #              bsTooltip(id = "lbl_bladePitch",
                  #                        title = paste0("Angle of the blade from plane of rotation (decimal degrees).",
                  #                                       " To be specified via a probability distribution or",
                  #                                       " a relationship between rotor speed and wind speed"),
                  #                        options = list(container = "body"), placement = "right", trigger = "hover")
                  #       )
                  #   ),
                  #   conditionalPanel(
                  #     condition = "input.radButtonInput_turbinePars_bldPitchInputOption == 'windVsPitch' | input.radButtonInput_turbinePars_rotSpdInputOption == 'windVsRotation'",
                  #     column(4, 
                  #            box(width = 12, 
                  #                NormNumericInput(paramID = "miscPars_windSpeed", specID = "", 
                  #                                 varName = "Wind Speed (m/s)",
                  #                                 infoId = "lbl_winSpeed", 
                  #                                 infoText = paste0("If rotation speed and/or blade pitch are specified in relation to wind speed," , 
                  #                                                   " wind speed is randomly generated (~Truncated Normal with lower bound at 0)"),
                  #                                 E_value = startUpValues$windSpeed_E, SD_value = startUpValues$windSpeed_SD),
                  #                plotOutput("plot_miscPars_windSpeed", width = 300, height = 200),
                  #                verbatimTextOutput("qtls_miscPars_windSpeed")
                  #            )
                  #     )
                  #   )
                  # )
              )
            )
    ),
    tabItem(tabName="tab_simulation", class = "active",
            fluidRow(
              box(title = "Simulation Options", width = 2, status = "primary", solidHeader = TRUE, #background = "aqua", 
                  # checkboxInput(inputId = "chkBoxInput_simulPars_largeArrarCorr", label = tags$b("Apply large Array Correction"), 
                  #               value = TRUE),
                  tipify(
                    switchInput(inputId = "chkBoxInput_simulPars_largeArrarCorr", label = "Large Array Correction", size = "normal", 
                              onStatus = "success", offStatus = "danger", value = TRUE),
                         title = "Adjustment to the probability of bird collision to allow for depletion of bird density in later rows of windfarms with a large array of turbines", 
                         placement = "right", trigger = "hover", options = list(container = "body")),

                  hr(),
                  sliderInput(inputId = "sldInput_simulPars_numIter", label = "Number of Iterations",
                              min = 100, max = 5000, step = 100, value = 100),

                  hr(),
                  actionButton(inputId = "actButtonInput_simulPars_GO", label = tags$b("Run Simulation"), 
                               icon = icon("cogs"), width = "100%")
              ),
              
 
              
              box(title = "Model Ouputs", width = 10, status = "primary", solidHeader = TRUE,
                  #verbatimTextOutput("out_simFunctionArgs")
                  # downloadButton("downloadData", "Download"),
                  # br(),
                  uiOutput("simResults_UI")
              )
            )
    )
  ),

  

  verbatimTextOutput("out_inputs_biom"),
  verbatimTextOutput("inputRVs")

  #uiOutput("bodyUI"),

  #  verbatimTextOutput("test")

  #verbatimTextOutput("out_inputs_monthDens")
  
  #plotOutput("testPlot")
)



#dashboardPage(header, sidebar, body)


bootstrapPage(

  useShinyjs(),
  useSweetAlert(),

  # Add custom CSS & Javascript;
  tagList(tags$head(
    tags$link(rel="stylesheet", type="text/css",href="styles.css"),
    tags$script(type="text/javascript", src = "busy.js")
  )),

  dashboardPage(header, sidebar, body),

  div(class = "busy",
      tags$b(h4("Working on it...")),
      img(src="loading.gif")
      #img(src="operation-and-maintenance.gif", height = "50%", width = "50%")
      #h2(HTML('<i class="fa fa-cog fa-spin fa-2x"></i>'))
      #h2(HTML('<i  class="fa fa-spinner fa-pulse fa-2x fa-fw"></i>'))
  )
)




