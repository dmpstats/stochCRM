


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
    
    bsAlert(anchorId = "alert")
    
  )
)


#################################################################
# Dashboard body
#################################################################

body <- dashboardBody(
  
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  #   #tags$link(rel = "stylesheet", type = "text/css", href = "cyborgTheme.css")
  # ),

  #tabItems(
  div(class="tab-content", id="tabItemsEnvelope",
    tabItem(tabName="tab_turbWindPars",
            fluidRow(
              box(title = "Wind farm features", 
                  width = 2, 
                  status = "primary", 
                  solidHeader = TRUE,
                  
                  #helpText("<Brief description of windfarm features here>"),
                  
                  # --- Turbine Power
                  numericInput(width = "85%", 
                               inputId = "numInput_windfarmPars_targetPower", 
                               label = label.help("Target Power (MW)", "lbl_windfarmTargetPower"), #"Target Power (MW)", 
                               value = startUpValues$windfarmPars_targetPower, min = 0),
                  bsTooltip(id = "lbl_windfarmTargetPower", 
                            title = paste0("Target power to be generated within wind farm.", 
                            " Used in conjunction with turbine model to calculate number of turbines in the array"),
                            options = list(container = "body"), placement = "right", trigger = "hover"),
 
                  # ---  Latitude
                  numericInput(width = "85%", 
                               inputId = "numInput_windfarmPars_Latitude", 
                               label = label.help("Latitude (deg)", "lbl_windfarmLatitude"), #"Latitude (deg)", 
                               value = startUpValues$windfarmPars_Latitude, min = -90, max = 90),
                  bsTooltip(id = "lbl_windfarmLatitude", 
                            title = paste0("Latitude of the wind farm in decimal degrees. Used to calculate day length at the site over the year."),
                            options = list(container = "body"), placement = "right", trigger = "hover"),
                  
                  # --- Width
                  numericInput(width = "85%", 
                               inputId = "numInput_windfarmPars_width", 
                               label = label.help("Width (Km)", "lbl_windfarmWidth"), #"Width (Km)", 
                               value = startUpValues$windfarmPars_width, min = 0), 
                  bsTooltip(id = "lbl_windfarmWidth", 
                            title = paste0("The wind farm width"),
                            options = list(container = "body"), placement = "right", trigger = "hover"),
                  
                  # --- Tidal offset
                  numericInput(width = "85%", 
                               inputId = "numInput_windfarmPars_tidalOffset", 
                               label = label.help("Tidal Offset (m)", "lbl_tidalOffset"), #"Tidal Offset (m)", 
                               value = startUpValues$tidalOffset, min = 0, step = 0.5),
                  bsTooltip(id = "lbl_tidalOffset", 
                            title = paste0("Tidal offset to correct for flight heights calculated in relation to sea-level",
                                           " and turbine dimensions calculated in relation to Highest Astronomical Tide"),
                            options = list(container = "body"), placement = "right", trigger = "hover"),
                  
                  br(),
                  # --- Upwind/Downwind proportion of flights
                  sliderInput(width = "85%", 
                              inputId = "sldInput_windfarmPars_upWindDownWindProp", 
                              label = label.help("Upwind flights (%)", "lbl_upWindDownWindProp"), #"up/downwind ratio of flights", 
                              value = 50, min = 0, max = 100, step = 10),
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
                    column(width=4,
                           box(width = 12, 
                               numericInput(width = "70%", 
                                            inputId = "numInput_turbinePars_turbinePower", 
                                            label = label.help("Turbine Model (MW)", "lbl_turbinePower"), #Turbine Model (output in MW)", 
                                            value = 4, min = 0),
                               bsTooltip(id = "lbl_turbinePower", 
                                         title = paste0("The power output of each turbine"),
                                         options = list(container = "body"), placement = "right", trigger = "hover")
                           )
                    ),
                    column(width=4, 
                           box(width = 12,
                               numericInput(width = "70%", 
                                            inputId = "numInput_turbinePars_numBlades", 
                                            label =  label.help("No. of blades", "lbl_numBlades"), # "Number of blades", 
                                            value = 3, min = 0),
                               bsTooltip(id = "lbl_numBlades", 
                                         title = paste0("Number of blades in each turbine"),
                                         options = list(container = "body"), placement = "right", trigger = "hover")
                           )
                    )
                  ),
                  #hr(),
                  
                  fluidRow(
                    column(width = 4,
                           box(width = 12,
                               NormNumericInput(paramID = "turbinePars_rotRadius", specID = "", 
                                                varName = "Rotor Radius (m)", 
                                                infoLabel = "lbl_rotorRadius", 
                                                infoText = paste0("The distance from the axis of rotation to blade tip (~Normal)."),
                                                E_value = startUpValues$rotorSpeed_E, SD_value = startUpValues$rotorSpeed_SD),
                               h5(paste0("PDF for turbine's rotor Radius")),
                               plotOutput("plot_turbinePars_rotRadius", width = 300, height = 200)
                           )
                    ),
                    column(width = 4,
                           box(width = 12,
                               NormNumericInput(paramID = "turbinePars_hubHght", specID = "",
                                                varName = "Hub Height (m)",
                                                infoLabel = "lbl_turbineHubHeight", 
                                                infoText = paste0("Used in conjunction with rotor radius to provide the distance", 
                                                                  " between Highest Astronomical Tide and the axis of rotation (~Normal)."),
                                                E_value = startUpValues$hubHght_E, SD_value = startUpValues$hubHght_SD),
                               h5(paste0("PDF for turbine's Hub Height")),
                               plotOutput("plot_turbinePars_hubHght", width = 300, height = 200)
                           )
                    ),
                    column(width = 4,
                           box(width = 12,
                               NormNumericInput(paramID = "turbinePars_maxBladeWdth", specID = "", 
                                                varName = "Max blade width (m)",
                                                infoLabel = "lbl_maxBladeWdth", 
                                                infoText = paste0("The maximum width of the rotor blade (~Normal)."),
                                                E_value = startUpValues$maxBladeWdth_E, SD_value = startUpValues$maxBladeWdth_SD),
                               h5(paste0("PDF for turbine's maximum blade width")),
                               plotOutput("plot_turbinePars_maxBladeWdth", width = 300, height = 200)
                           )
                    )
                  ),
                  br(),
                  fluidRow(
                    box(width = 12, 
                        # #title = tags$b("Monthly Operations"),
                        # h5(tags$b("Monthly Operation")),
                        # helpText("Information on percentage of wind availability per month (treated as constant) and percentage of maintenance downtime per month (incorporating uncertainty via a Normal distribution)"),
                        # br(),
                        #fileInput(inputId = "upldInput_dt_turbOper", label = "Upload data", width = "20%"),
                        
                        tags$b(HTML(paste0("Monthly Operation", actionLink("lbl_monthOPs",label=NULL,icon=icon('info-circle'))))),
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
                    column(4,
                           box(width = 12, 
                               #h5(tags$b("Rotation Speed")),
                               radioButtons(inputId = "radButtonInput_turbinePars_rotSpdInputOption", label = "Rotation Speed", inline = FALSE,
                                            choices = list("Wind Vs. rotation speed relationship"= "windVsRotation", 
                                                           "Probability Distribution" = "probDist")),
                               conditionalPanel(
                                 condition = "input.radButtonInput_turbinePars_rotSpdInputOption == 'windVsRotation'",
                                 rHandsontableOutput("hotInput_turbinePars_rotationVsWind", width = "100%"),
                                 tags$style(type="text/css", "#hotInput_turbinePars_rotationVsWind th {font-weight:bold;}")
                               ),
                               conditionalPanel(
                                 condition = "input.radButtonInput_turbinePars_rotSpdInputOption == 'probDist'",
                                 NormNumericInput(title = "Rotation Speed (rpm)", paramID = "turbinePars_rotnSpeed", 
                                                  specID = "", varName = "Rotation (rpm)"),
                                 plotOutput("plot_turbinePars_rotnSpeed", width = 300, height = 200)
                               )
                           )
                    ),
                    column(4,
                           box(width = 12, 
                               #h5(tags$b("Blade Pitch")),
                               radioButtons(inputId = "radButtonInput_turbinePars_bldPitchInputOption", label = "Blade Pitch", inline = FALSE,
                                            choices = list("Wind Vs. blade pitch relationship" = "windVsPitch", 
                                                           "Probability Distribution"= "probDist")),
                               conditionalPanel(
                                 condition = "input.radButtonInput_turbinePars_bldPitchInputOption == 'windVsPitch'",
                                 rHandsontableOutput("hotInput_turbinePars_pitchVsWind", width = "100%"),
                                 tags$style(type="text/css", "#hotInput_turbinePars_pitchVsWind th {font-weight:bold;}")
                               ),
                               conditionalPanel(
                                 condition = "input.radButtonInput_turbinePars_bldPitchInputOption == 'probDist'",
                                 NormNumericInput(title = "Blade Pitch (degrees)", paramID = "turbinePars_bladePitch", 
                                                  specID = "", varName = "Pitch (deg)"),
                                 plotOutput("plot_turbinePars_bladePitch", width = 300, height = 200)
                               )
                        )
                    ),
                    conditionalPanel(
                      condition = "input.radButtonInput_turbinePars_bldPitchInputOption == 'windVsPitch' | input.radButtonInput_turbinePars_rotSpdInputOption == 'windVsRotation'",
                      column(4, 
                             box(width = 12, 
                                 NormNumericInput(title = "Wind Speed (m/s)", paramID = "miscPars_windSpeed", 
                                                  specID = "", varName = "Wind Speed (m/s)",
                                                  E_value = startUpValues$windSpeed_E, SD_value = startUpValues$windSpeed_SD),
                                 plotOutput("plot_miscPars_windSpeed", width = 300, height = 200)
                             )
                      )
                    )
                  )
              )
            )
    ),
    tabItem(tabName="tab_simulation", class = "active",
            fluidRow(
              box(title = "Simulation Options", width = 2, status = "primary", solidHeader = TRUE, #background = "aqua", 
                  checkboxInput(inputId = "chkBoxInput_simulPars_largeArrarCorr", label = "Apply large Array Correction", 
                                value = TRUE),
                  hr(),
                  sliderInput(inputId = "sldInput_simulPars_numIter", label = "Number of Iterations", 
                              min = 50, max = 10000, step = 500, value = 50),
                  hr(),
                  actionButton(inputId = "actButtonInput_simulPars_GO", label = tags$b("Run Simulation"), 
                               icon = icon("cogs"), width = "100%")
              ),
              box(title = "Outputs", width = 10, status = "primary", solidHeader = TRUE,
                  verbatimTextOutput("out_simFunctionArgs")
              )
            )
    )
  )
  
  # verbatimTextOutput("out_inputs_biom"),
  # verbatimTextOutput("inputRVs")
  # 
  # uiOutput("bodyUI"),
  # 
  # #  verbatimTextOutput("test")
  # 
  # verbatimTextOutput("out_inputs_monthDens")
  
  #plotOutput("testPlot")
)


#dashboardPage(header, sidebar, body)


bootstrapPage(

  useShinyjs(),

  # Add custom CSS & Javascript;
  tagList(tags$head(
    tags$link(rel="stylesheet", type="text/css",href="styles.css"),
    tags$script(type="text/javascript", src = "busy.js")
  )),

  dashboardPage(header, sidebar, body),

  div(class = "busy",
      tags$b(h4("Working...")),
      #img(src="images/OurStory_06.Wind-Turbine.gif")
      #h2(HTML('<i class="fa fa-cog fa-spin fa-2x"></i>'))
      h2(HTML('<i  class="fa fa-spinner fa-pulse fa-2x fa-fw"></i>'))
  )
)




