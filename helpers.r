
# modifying behaviour of qtnorm to deal with its errors when the specified parameters fall outside allowed values (e.g. when mu>upper and sd==0), returning 0
qtnorm_possibly <- possibly(qtnorm, otherwise = 0)

fread_possibly <- possibly(fread, otherwise = NULL)


# Function to create icons and labels with info about parameters
label.help <- function(label, id){
   HTML(paste0(label, actionLink(id,label=NULL,icon=icon('info-circle'))))
}




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
  
  plotWidth <- 330
  plotHeight <- 200
  
  tabItem(
    tabName = tabName,
    fluidRow(
      box(
        title = paste0(specName, " Parameters"),
        width = 12,
        status = "primary", 
        solidHeader = TRUE,
        #side = "right",
        
        
        fluidRow(
          column(width = 4,
                 box(width=12,
                     
                     radioGroupButtons(inputId = paste0("slctInput_biomPars_flType_tp_", specLabel), 
                                       label = label.help("Flight Type", paste0("lbl_flType_", specLabel)), 
                                       choices = c("Flapping", "Gliding"), individual = TRUE,
                                       justified = FALSE,
                                       checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                        no = icon("remove", lib = "glyphicon"))
                     )
                 )
          )
        ),
        
          fluidRow(
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
                       plotOutput(paste0("plot_biomPars_bodyLt_", specLabel), width = plotWidth, height = plotHeight),
                       column(11, 
                              verbatimTextOutput(paste0("qtls_biomPars_bodyLt_", specLabel))
                       )
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
                       plotOutput(paste0("plot_biomPars_wngSpan_", specLabel), width = plotWidth, height = plotHeight),
                       column(11, 
                              verbatimTextOutput(paste0("qtls_biomPars_wngSpan_", specLabel))
                       )
                   )
            ),
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
                       plotOutput(paste0("plot_biomPars_flSpeed_", specLabel), width = plotWidth, height = plotHeight),
                       column(11, 
                              verbatimTextOutput(paste0("qtls_biomPars_flSpeed_", specLabel))
                       )
                   )
            )
          ),
          br(),
          br(),
          
          fluidRow(
            column(width = 4,
                   box(width = 12, 
                       NormNumericInput(paramID = "biomPars_noctAct", specID = specLabel, 
                                        varName = "Nocturnal Activity",
                                        infoId = paste0("lbl_noctAct_", specLabel),
                                        via_InsertUI = TRUE,
                                        E_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$noctAct_E, 1), 
                                        E_min=0, E_step = 0.001,
                                        SD_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$noctAct_SD, 0), 
                                        SD_min = 0, SD_step = 0.001),
                       h5(paste0("PDF of ", specName, "'s Nocturnal Activity")),
                       plotOutput(paste0("plot_biomPars_noctAct_", specLabel), width = plotWidth, height = plotHeight),
                       column(11, 
                              verbatimTextOutput(paste0("qtls_biomPars_noctAct_", specLabel))
                       )
                   )
            ),
            column(width = 4,
                   box(width = 12, 
                       NormNumericInput(paramID = "biomPars_basicAvoid", specID = specLabel, 
                                        varName = "Basic Avoidance",
                                        infoId = paste0("lbl_basicAvoid_", specLabel),
                                        via_InsertUI = TRUE,
                                        E_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$basicAvoid_E, 1), 
                                        E_min=0, E_step = 0.001,
                                        SD_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$basicAvoid_SD, 0), 
                                        SD_min = 0, SD_step = 0.001),
                       h5(paste0("PDF of ", specName, "'s basic avoidance")),
                       plotOutput(paste0("plot_biomPars_basicAvoid_", specLabel), width = plotWidth, height = plotHeight),
                       column(11, 
                              verbatimTextOutput(paste0("qtls_biomPars_basicAvoid_", specLabel))
                       )
                   )
            ),
            column(width = 4,
                   box(width = 12, 
                       NormNumericInput(paramID = "biomPars_extAvoid", specID = specLabel, 
                                        varName = "Extended Avoidance",
                                        infoId = paste0("lbl_extAvoid_", specLabel),
                                        via_InsertUI = TRUE,
                                        E_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$extAvoid_E, 1), 
                                        E_min=0, E_step = 0.001,
                                        SD_value = ifelse(specLabel == "Black_legged_Kittiwake", startUpValues$extAvoid_SD, 0), 
                                        SD_min = 0, SD_step = 0.001),
                       h5(paste0("PDF of ", specName, "'s extended avoidance")),
                       plotOutput(paste0("plot_biomPars_extAvoid_", specLabel), width = plotWidth, height = plotHeight),
                       column(11, 
                              verbatimTextOutput(paste0("qtls_biomPars_extAvoid_", specLabel))
                       )
                       
                   )
            )
          ),
          br(),
          br(),
          
          fluidRow(
  
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
                       plotOutput(paste0("plot_biomPars_CRHeight_", specLabel), width = plotWidth, height = plotHeight),
                       column(11, 
                              verbatimTextOutput(paste0("qtls_biomPars_CRHeight_", specLabel))
                       )
                   )
            ),
            column(width = 8,
                   box(width=12, 
                       tags$b(HTML(paste0("Flight Height Distribution", 
                                          actionLink(paste0("lbl_FHD_", specLabel), label=NULL, 
                                                     icon=icon('info-circle'))))),
                       br(),
                       br(),
                       
                       fluidRow(
                         column(5, 
                                radioGroupButtons(inputId = paste0("slctInput_userOpts_FHD_dtSrc_", specLabel),
                                                  individual = TRUE,
                                                  justified = TRUE, 
                                                  label = "Choose data source:",
                                                  choices = c("Johnson et al (2014)" = "default",
                                                              "Other" = "other"),
                                                  checkIcon = list(yes = tags$i(class = "fa fa-circle",
                                                                                style = "color: steelblue"),
                                                                   no = tags$i(class = "fa fa-circle-o",
                                                                               style = "color: steelblue")))
                         )
                       ),

                       # Panels for default FHD data
                       conditionalPanel(
                         condition = paste0("input.slctInput_userOpts_FHD_dtSrc_", specLabel, " == 'default'"),

                         br(),
                         
                         # --- Default FHD data
                         awesomeRadio(inputId = paste0("aweRadio_userOpts_defaultFHDPlotType_", specLabel),
                                      label = "Data visualisation",
                                      choices = c("Bootstrap quantiles" = "Qtls",
                                                  "Bootstrap samples" = "bootSamp"),
                                      inline = TRUE,
                                      selected = "Qtls",
                                      status = "warning"),
                        
                         # nested conditions for panels with plots of bootstrap data
                         conditionalPanel(condition = paste0("input.aweRadio_userOpts_defaultFHDPlotType_", specLabel, " == 'Qtls'"),
                                          column(12, p(paste0("Proportion of ", specName," flying at 1m height intervals 
                                                         (medians and 95% intervals of bootstrap data) - Default Data"))),
                                          br(),
                                          plotOutput(paste0("plot_defaultFHD_QtsBoot_", specLabel), width = "100%", height = 400),
                                          br(),
                                          column(3, offset = 9,
                                                 helpText(tags$i(tags$a(href ="http://onlinelibrary.wiley.com/doi/10.1111/1365-2664.12191/full", 
                                                                        target = "_blank", "Source: Johnson et al (2014)"))))
                         ),
                         conditionalPanel(condition = paste0("input.aweRadio_userOpts_defaultFHDPlotType_", specLabel, " == 'bootSamp'"),
                                          column(12, p(paste0("100 Bootstrap samples of proportion of ", specName, " flying at 1m height 
                                                              bands (first 50m plotted; full data up to 300m) - Default Data"))),
                                          br(),
                                          d3heatmapOutput(paste0("plot_defaultFHD_allBoots_", specLabel), width = "105%", height = 430),
                                          column(3, offset = 9,
                                                 helpText(tags$i(tags$a(href ="http://onlinelibrary.wiley.com/doi/10.1111/1365-2664.12191/full",
                                                                        target = "_blank",
                                                                        "Source: Johnson et al (2014)"))))
                         )
                       ),
                       conditionalPanel(
                         condition = paste0("input.slctInput_userOpts_FHD_dtSrc_", specLabel, " == 'other'"),

                         br(),
                         uiOutput(paste0("renderUI_uploadUserFHD_", specLabel), inline = TRUE),
                         column(12,
                           # --- User FHD data
                           awesomeRadio(inputId = paste0("aweRadio_userOpts_UserFHDPlotType_", specLabel),
                                        label = "Data visualisation",
                                        choices = c("Bootstrap quantiles" = "Qtls",
                                                    "Bootstrap samples" = "bootSamp"),
                                        inline = TRUE,
                                        selected = "Qtls",
                                        status = "warning"),
                           
                           # nested conditions for panels with plots of bootstrap data
                           conditionalPanel(condition = paste0("input.aweRadio_userOpts_UserFHDPlotType_", specLabel, " == 'Qtls'"),
                                            column(12, p(paste0("Proportion of ", specName," flying at 1m height intervals 
                                                         (medians and 95% intervals of bootstrap data) - Uploaded Data"))),
                                            br(),
                                            plotOutput(paste0("plot_UserFHD_QtsBoot_", specLabel), width = "100%", height = 430)
                           ),
                           conditionalPanel(condition = paste0("input.aweRadio_userOpts_UserFHDPlotType_", specLabel, " == 'bootSamp'"),
                                            column(12, p(paste0("100 Bootstrap samples of proportion of ", specName, " flying at 1m height 
                                                         bands (only first 50m plotted) - Uploaded Data"))),
                                            br(),
                                            d3heatmapOutput(paste0("plot_UserFHD_allBoots_", specLabel), width = "105%", height = 430)
                           )
                         )
                       )
                   )
            )
          ),
        
        br(),

        fluidRow(
          box(width = 12,
              
              tags$b(HTML(paste0("Monthly Densities", actionLink(paste0("lbl_monthOPs_", specLabel), label=NULL, icon=icon('info-circle'))))),
              br(),
              br(),
              
              radioGroupButtons(inputId = paste0("slctInput_userOpts_monthDens_sampler_", specLabel),
                                individual = TRUE,
                                justified = FALSE, 
                                label = "Choose how to specify distribution of monthly bird densities",
                                choices = c("Truncated Normal" = "truncNorm",
                                            "Distribution reference points" = "pcntiles",
                                            "Distribution samples" = "reSamp"),
                                checkIcon = list(yes = tags$i(class = "fa fa-circle",
                                                              style = "color: steelblue"),
                                                 no = tags$i(class = "fa fa-circle-o",
                                                             style = "color: steelblue"))),
              br(),
              
              conditionalPanel(condition = paste0("input.slctInput_userOpts_monthDens_sampler_", specLabel, " == 'truncNorm'"),
                               helpText("Provide the means and standard deviations of monthly densities, assuming Truncated Normals bounded at 0"),
                               br(),
                               rHandsontableOutput(paste0("hotInput_birdDensPars_tnorm_", specLabel), width = "100%"),
                               tags$style(type="text/css", paste0("#hotInput_birdDensPars_tnorm_", specLabel, " th {font-weight:bold;}")),
                               br(),
                               br(),
                               br(),
                               fluidRow(
                                 column(width=8, offset = 2, 
                                        plotOutput(paste0("plot_birdDensPars_", specLabel), width = 800, height = 350)
                                 )
                               )
                               
              ),
              
              conditionalPanel(condition = paste0("input.slctInput_userOpts_monthDens_sampler_", specLabel, " == 'pcntiles'"),
                               uiOutput(paste0("renderUI_userUpload_monthDens_summaries_", specLabel), inline = TRUE),
                               br(),
                               br(),
                               fluidRow(
                                 column(12,
                                   plotOutput(paste0("plot_inputMonthDens_summaries_", specLabel), width = "100%", height = 300)
                                   )
                               ),
                               br(),
                               br(),
                               fluidRow(
                                 column(12, offset = 3,
                                        plotOutput(paste0("plot_inputMonthDens_QtlsBars_summaries_", specLabel), width = 800, height = 300)
                                 )
                               )
              ),
              conditionalPanel(condition = paste0("input.slctInput_userOpts_monthDens_sampler_", specLabel, " == 'reSamp'"),
                               uiOutput(paste0("renderUI_userUpload_monthDens_samples_", specLabel), inline = TRUE),
                               br(),
                               br(),
                               fluidRow(
                                 column(12, 
                                        plotOutput(paste0("plot_inputMonthDens_samples_", specLabel), width = "100%", height = 300)
                                 )
                               ),
                               br(),
                               br(),
                               fluidRow(
                                 column(12, offset = 3, plotOutput(paste0("plot_inputMonthDens_QtlsBars_samples_", specLabel), width = 800, height = 350))
                               )
              ),
              br(),
              br()
          )
        ),
        
        # Include bsTooltips with info on parameters
        uiOutput(paste0("BiomBStoolTips_", specLabel))
      )
    )
  )
}





results_tabPanelsBuilder <- function(specName, specLabel){
    
    #plotWidth <- 400
    #plotHeight <- 200
    
    tabPanel(title = tags$b(specName),
             fluidRow(
               box(width = 12,
                   h4(paste0("Annual number of collisions - ", specName)),
                   br(),
                   
                   column(6,
                          plotOutput(paste0(specLabel, "_plot_overallCollisions"), width = "100%")
                   ),
                   column(6,
                          align="center",
                          div(dataTableOutput(paste0(specLabel, "_summTable_overallCollisions")), 
                              style = "font-size: 90%; width: 100%")
                   )
               )
             ),
             fluidRow(
               box(width = 12,
                   h4(paste0("Monthly number of collisions by model option - ", specName)),
                   br(),
                   
                   fluidRow(
                     column(6, 
                            br(),br(),br(),br(),
                            plotOutput(paste0(specLabel, "_plot_monthCollisions_Option1"), width = "100%")
                            ),
                     column(6, 
                            align="center",
                            div(dataTableOutput(paste0(specLabel, "_summTable_monthCollisions_Option1")),
                                   style = "font-size: 90%; width: 100%")
                            )
                   ),
                   br(),
                   br(),
                   hr(),
                   
                   fluidRow(
                     column(6, 
                            br(),br(),br(),br(),
                            plotOutput(paste0(specLabel, "_plot_monthCollisions_Option2"), width = "100%")
                            ),
                     
                     column(6, 
                            align="center",
                            div(dataTableOutput(paste0(specLabel, "_summTable_monthCollisions_Option2")),
                                style = "font-size: 90%; width: 100%")
                            )
                   ),
                   br(),
                   br(),
                   hr(),
                   
                   fluidRow(
                     column(6, 
                            br(),br(),br(),br(),
                            plotOutput(paste0(specLabel, "_plot_monthCollisions_Option3"), width = "100%")
                            ),
                     column(6, 
                            align="center",
                            div(dataTableOutput(paste0(specLabel, "_summTable_monthCollisions_Option3")),
                                style = "font-size: 90%; width: 100%")
                            )
                   )
               )
             )
    )
}





divUI_monthDens_PctlsAndSample_DownButtons <- function(introText, fileInputId, fileInputPopUpText, downButtOutputId){
  fluidRow(
    column(12, helpText(introText)),
    br(),
    column(3,
           tipify(      
             fileInput(inputId = fileInputId,
                       label = "Upload file",
                       multiple = FALSE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")),
             title = fileInputPopUpText,
             placement = "left", trigger = "hover", options = list(container = "body")
           )
    ),
    column(1,
           style = "margin-top: 25px; margin-left: -10px",
           downloadButton(outputId = downButtOutputId, label = NULL, class = "butt"),
           tags$head(tags$style(".butt{background-color:#4570a5;
                                    color: #efecec}
                                    .butt:hover{
                                    background-color:#4570a5;
                                    color: #efecec}"
           ))
    ),
    
    bsTooltip(id = downButtOutputId,
              title = "Template dataset. Fill in, save locally and upload on the adjacent field",
              options = list(container = "body"),
              placement = "right", trigger = "hover")
  )
}



## ------------------------------------------------------------------------------------- ##
## ------------------------------------------------------------------------------------- ##
## -----                       Functions for Plotting                           -------- 
## ------------------------------------------------------------------------------------- ##
## ------------------------------------------------------------------------------------- ##

# DEPRECATED function for Normal density plot of model input parameters 
normDensPlotPars <- function(mu, stdev, fill="olivedrab", xlab){
  
  req(mu, stdev)
  
  data.frame(qtls = qnorm(c(0.001, 0.999), mean = mu, sd=stdev))  %>%
    ggplot(aes(qtls)) +
    stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), col = "black", size =1) +
    stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), geom="area", fill = fill, alpha = 0.3) +
    labs(y="Density", x = xlab)
}



# function for Normal density plot of model input parameters
normDens_ParsPlots <- function(mu, stdev, fill="olivedrab", xlab, refValue_E = mu, refValue_SD = stdev){
  
  req(mu, stdev)
  data.frame(qtls = qnorm(c(0.0001, 0.9999), mean = mu, sd=stdev))  %>%
    ggplot(aes(qtls)) +
    stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), col = "black", size =1) +
    stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), geom="area", fill = fill, col = "black", alpha = 0.3) +
    labs(y="Density", x = xlab) + 
    #geom_vline(aes(xintercept = 0)) +
    coord_cartesian(xlim = qnorm(c(0.000001, 0.999999), mean = refValue_E, sd = refValue_SD))
}


# function for Normal density quantiles of model input parameters
normDens_ParsQtls <- function(mu, stdev, varTag, decPlaces = 3){
  
  req(mu, stdev)
  data.frame(pctile = paste0(varTag, "|"), t(round(qnorm(p = c(0.025, 0.25, 0.5, 0.75, 0.975), mean = mu, sd = stdev), decPlaces))) %>%
    rename(`%tile|` = pctile, `2.5th` = X1, `25th` = X2, `50th` = X3, `75th` = X4, `97.5th` = X5) %>%
    mutate_at(.vars = vars(`2.5th`:`97.5th`), funs(sprintf(fmt = paste0("%.", decPlaces, "f"), .)))%>%
    mutate(dummy = "") %>%
    column_to_rownames("dummy")
}





# function for Truncated Normal density plot of model input parameters
truncNormPars_densPlots <- function(mu, stdev, lower = -Inf, upper = Inf, fill="olivedrab", xlab){
  
  req(mu, stdev)

  yaxixUpLimBands <- data.frame(start = c(0.0, 0.25, 0.5, 1, 2, 4, 6), end = c(0.25, 0.5, 1, 2, 4, 6, 10))
  
  if(stdev == 0){
    if(mu <= lower){
      NULL
    }else{
      data.frame(x = seq(mu-500, mu+500, by = 1), y = 0) %>% #%>% mutate(y = if_else(x == mu, 0.5, 0)) %>%
        ggplot(aes(x=x, y=y)) +
        geom_path() +
        geom_segment(aes(x = mu, xend = mu, y = 0, yend = Inf), size = 1) +
        coord_cartesian(xlim = c(0, mu*1.65), ylim = c(0, 0.5)) +
        labs(y="Density", x = xlab)  
    }
  }else{
    if(stdev>0){
      
      distTails <- qtnorm(c(0.00001, 0.999999), mean = mu, sd = stdev, lower = lower, upper = upper)
      xaxisUpLim <- distTails[2]*1.1
      xaxisLowLim <- ifelse(distTails[1]*0.5 > mu/2.5, distTails[1]*0.7, 0)
      
      yaxisMax <- max(dtnorm(seq(mu-500, mu+500, by = 1), mean = mu, sd = stdev, lower = lower, upper = upper))
      yaxisUpLim <- yaxixUpLimBands %>%
        mutate(yaxisMax = yaxisMax) %>%
        filter(between(yaxisMax, start, end)) %>%
        select(end)
      if(nrow(yaxisUpLim) == 0){
        yaxisUpLim <- data.frame(end = yaxisMax)
      }
      
      data.frame(qtls = qtnorm(c(0.0001, 0.9999), mean = mu, sd=stdev, lower = lower, upper = upper))  %>%
        ggplot(aes(qtls)) +
        stat_function(fun=dtnorm, args = list(mean = mu, sd = stdev, lower = lower, upper = upper), col = "black", size = 1) +
        stat_function(fun=dtnorm, args = list(mean = mu, sd = stdev, lower = lower,  upper = upper), geom="area", 
                      fill = fill, col = "black", alpha = 0.3)+
        labs(y="Density", x = xlab) +
        coord_cartesian(xlim = c(xaxisLowLim, max(0, xaxisUpLim, na.rm=TRUE)), ylim = c(0, yaxisUpLim$end))
    }else{
      NULL
    }
  }
}



# function for Beta density plot of model input parameters
betaInputPars_densPlots <- function(p, stdev, fill="olivedrab", xlab){
  
  req(p, stdev)

  yaxixUpLimBands <- data.frame(start = c(0.0, 0.5, 1, 2, 4, 6, 10), end = c(0.5, 1, 2, 4, 6, 10, 20))
  
  betaMeanVarCond <- stdev^2 < p*(1-p)
  
  if(p >= 0 & p <= 1){
    if(stdev == 0){
      
      data.frame(x = seq(0, 1, length.out = 100), y = 0) %>%
        ggplot(aes(x=x, y=y)) +
        geom_path() +
        geom_segment(aes(x = p, xend = p, y = 0, yend = Inf), size = 1) +
        coord_cartesian(xlim = c(0,1), ylim = c(0, 0.5)) +
        labs(y="Density", x = xlab)  
    }else{
      if(stdev > 0){
        if(betaMeanVarCond){
          
          eta <- p*(1-p)/stdev^2 - 1
          alpha <- eta*p
          beta <- eta*(1-p)
          
          distTails <- qbeta(c(0.00001, 0.999999), shape1 = alpha, shape2 = beta)
          xaxisUpLim <- ifelse(distTails[2] > 0.6, 1, ifelse(distTails[2] > 0.3, 0.6, 0.3))
          xaxisLowLim <- ifelse(distTails[1] < 0.6, 0, ifelse(distTails[1] < 0.85, 0.5, 0.86))
          
          yaxisMax <- max(dbeta(seq(0, 1, by = 0.001), shape1 = alpha, shape2 = beta))
          
          yaxisUpLim <- yaxixUpLimBands %>%
            mutate(yaxisMax = yaxisMax) %>%
            filter(between(yaxisMax, start, end)) %>%
            select(end)
          if(nrow(yaxisUpLim) == 0){
            yaxisUpLim <- data.frame(end = yaxisMax)
          }
          
          p1 <- data.frame(qtls = qbeta(c(0.0001, 0.9999), shape1 = alpha, shape2 = beta))  %>%
            ggplot(aes(qtls)) +
            stat_function(fun=dbeta, args = list(shape1 = alpha, shape2 = beta), col = "black", size = 1) +
            stat_function(fun=dbeta, args = list(shape1 = alpha, shape2 = beta), geom="area", 
                          fill = fill, col = "black", alpha = 0.3) +
            labs(y="Density", x = xlab)
          
          #print(is.infinite(yaxisUpLim$end))
          
          if(is.infinite(yaxisUpLim$end)){
            p1
          }else{
            #p1 + coord_cartesian(xlim = c(0, 1), ylim = c(0, yaxisUpLim$end))
            p1 + coord_cartesian(xlim = c(xaxisLowLim, xaxisUpLim), ylim = c(0, yaxisUpLim$end))
          }
        }else{
          NULL
        }
      }else{
        NULL
      }
    }
  }else{
    NULL
  }
}
  

## ------------------------------------------------------------------------------------- ##
## ------------------------------------------------------------------------------------- ##
## -----               Functions for outputing quantiles                          -------- 
## ------------------------------------------------------------------------------------- ##
## ------------------------------------------------------------------------------------- ##


# function for Truncated Normal density quantiles of model input parameters
truncNormPars_qtlTbl <- function(mu, stdev, lower = -Inf, upper = Inf, varTag, decPlaces = 3){
  
  req(mu, stdev)
  
  if(stdev == 0 & mu <= lower){
    #NULL
    invisible()
  }else{
    if(stdev >= 0){
      data.frame(pctile = paste0(varTag, "|"), t(round(qtnorm(p = c(0.025, 0.25, 0.5, 0.75, 0.975), mean = mu, sd = stdev, lower = lower, 
                                                              upper = upper), decPlaces))) %>%
        rename(`%tile|` = pctile, `2.5th` = X1, `25th` = X2, `50th` = X3, `75th` = X4, `97.5th` = X5) %>%
        mutate_at(.vars = vars(`2.5th`:`97.5th`), funs(sprintf(fmt = paste0("%.", decPlaces, "f"), .))) %>%
        mutate(dummy = "") %>%
        column_to_rownames("dummy")   
    }else{
      invisible()
    }
  }
  
}



# function for beta quantiles of model input parameters
betaInputPars_qtlTbl <- function(p, stdev, varTag, decPlaces = 3){

  req(p, stdev)

  eta <- p*(1-p)/stdev^2 - 1
  alpha <- eta*p
  beta <- eta*(1-p)

  betaMeanVarCond <- stdev^2 < p*(1-p)

  if(p >= 0 & p <= 1){
    if(stdev == 0){   # if sd == 0, fixe sampled values to chosen p/mu (default qbeta generates fixed 0.5, as alpha = beta = Inf => p = 0.5)
      data.frame(pctile = paste0(varTag, "|"), X1 = p, X2 = p, X3 = p, X4 = p, X5 = p) %>%
        rename(`%tile|` = pctile, `2.5th` = X1, `25th` = X2, `50th` = X3, `75th` = X4, `97.5th` = X5) %>%
        mutate_at(.vars = vars(`2.5th`:`97.5th`), funs(sprintf(fmt = paste0("%.", decPlaces, "f"), .))) %>%
        mutate(dummy = "") %>%
        column_to_rownames("dummy")
    }else{
      if(stdev > 0){
        if(betaMeanVarCond){
          data.frame(pctile = paste0(varTag, "|"), t(round(qbeta(p = c(0.025, 0.25, 0.5, 0.75, 0.975), shape1 = alpha, shape2 = beta), decPlaces))) %>%
            rename(`%tile|` = pctile, `2.5th` = X1, `25th` = X2, `50th` = X3, `75th` = X4, `97.5th` = X5) %>%
            mutate_at(.vars = vars(`2.5th`:`97.5th`), funs(sprintf(fmt = paste0("%.", decPlaces, "f"), .))) %>%
            mutate(dummy = "") %>%
            column_to_rownames("dummy")
        }else{
          invisible()
        }
      }else{
        invisible()
      }
    }
  }else{
    invisible()
  }
}



## ------------------------------------------------------------------------------------- ##
## ------------------------------------------------------------------------------------- ##
## -----             Functions for Alerts on Parameter Validation               -------- 
## ------------------------------------------------------------------------------------- ##
## ------------------------------------------------------------------------------------- ##

# General function for alerts for parameters with truncated normal
tnormParamsAlert <- function(expVal, stdv, varName, varTag, session){
  
  c_alertId_E <- paste0("alertInput_", varTag, "_E")
  c_alertId_SD <- paste0("alertInput_", varTag, "_SD")
  
  if(!is.na(expVal) & !is.na(stdv)){
    if(stdv == 0 & expVal <= 0){
      createAlert(session, anchorId = "alert", alertId = c_alertId_E, title = "Oops",
                  content = paste0("<b>", varName, ":</b> needs Mean > 0<br/> when SD = 0"), append = TRUE, style = "danger")
    }else{
      closeAlert(session, alertId = c_alertId_E)
    }
  }
  
  if(!is.na(stdv)){
    if(stdv < 0){
      createAlert(session, anchorId = "alert", alertId = c_alertId_SD, title = "Oops",
                  content = paste0("<b>", varName, ":</b> needs SD >= 0"), append = TRUE, style = "danger")
    }else{
      closeAlert(session, alertId = c_alertId_SD)
    }
  }
}



# General function for alerts for validation of Beta distributed parameters
betaParamsAlert <- function(p, stdv, varName, varTag, session){
  
  c_alertId_E <- paste0("alertInput_", varTag, "_E")
  c_alertId_SD <- paste0("alertInput_", varTag, "_SD")
  
  #if(p == 0.2 & stdv == 0.2){ browser() }  
  
  betaMeanVarCond <- stdv^2 < p*(1-p)
  
  # input validation for p
  if(!is.na(p)){
    if(p < 0 | p > 1){
      createAlert(session, anchorId = "alert", alertId = c_alertId_E, title = "Oops",
                  content = paste0("<b>", varName, ":</b> needs Mean <br/> between 0 and 1"), append = TRUE, style = "danger")
    }else{
      closeAlert(session, alertId = c_alertId_E)
    }
  }
  
  # input validation for SD 
  if(!is.na(stdv) & !is.na(p)){
    if(p >= 0 & p <= 1){
      if(stdv > 0){
        if(p == 0 | p == 1){
          createAlert(session, anchorId = "alert", alertId = c_alertId_SD, title = "Oops",
                      content = paste0("<b>", varName, ":</b> <br/> needs SD = 0 when Mean is 0 or 1"), append = TRUE, style = "danger")
        }else{
          closeAlert(session, alertId = c_alertId_SD)
        }
        
        if(!betaMeanVarCond){
          condLimit <- round(sqrt(p*(1-p)), digits = 3)
          createAlert(session, anchorId = "alert", alertId = c_alertId_SD, title = "Oops",
                      content = paste0("<b>", varName, ":</b> <br/> needs SD < ", condLimit, " for the <br/> Mean p = ", round(p, digits = 3)), append = TRUE, style = "danger")
        }else{
          closeAlert(session, alertId = c_alertId_SD)    
        }
      }else{
        if(stdv < 0){
          createAlert(session, anchorId = "alert", alertId = c_alertId_SD, title = "Oops",
                      content = paste0("<b>", varName, ":</b> needs SD >= 0"), append = TRUE, style = "danger")
        }else{
          closeAlert(session, alertId = c_alertId_SD)
        }
      }
    }
  }

}


  


## ------------------------------------------------------------------------------------- ##
## ------------------------------------------------------------------------------------- ##
## -----      Functions for customized output from distribution functions         -------- 
## ------------------------------------------------------------------------------------- ##
## ------------------------------------------------------------------------------------- ##


# generate random samples from a beta distribution, parameterized as mean and sd, and returning NAs if conditions are not met
rbeta_dmp <- function(n, p, sd){
  
  eta <- p*(1-p)/sd^2 - 1
  alpha <- eta*p
  beta <- eta*(1-p)
  
  betaMeanVarCond <- sd^2 < p*(1-p)
  
  if(is.na(p) | is.na(sd)){
    
    out <- rep(NA, n)
    warning("NA values for p and/or stdev - NAs produced")
    
  }else{
    
    if(p >= 0 & p <= 1){
      
      if(sd < 0){
        out <- rep(NA, n)
        warning("stdev < 0 - NAs produced")
      }
      
      if(sd == 0){
        out <- rep(p, n)
      }
      
      if(sd > 0){
        if(betaMeanVarCond){
          out <- rbeta(n, shape1 = alpha, shape2 = beta)
        }else{
          out <- rep(NA, n)
          warning("condition var < p*(1 - p) not met - NAs produced")
        }
      }
    }else{
      out <- rep(NA, n)
      warning("p < 0 | p > 1 - NAs produced")
    }
  }
  return(out)
}



# generate random samples from a truncated normal, returning NAs if conditions are not met
rtnorm_dmp <- function(n, mean=0, sd=1, lower=-Inf, upper=Inf){
  
  if(is.na(mean)|is.na(sd)){
    out <- rep(NA, n)
    warning("NA values for mu and/or stdev - NAs produced")
  }else{
    if(sd >= 0){
      if(sd == 0 & mean == lower){
        out <- rep(lower, n)
      }
      if(sd == 0 & mean < lower){
        out <- rep(NA, n)
        warning("mu < lower & SD = 0 - NAs produced")
      }
      if(sd == 0 & mean > lower){
        #out <- qtnorm(p, mean = mean, sd = sd, lower = lower, upper = upper)
        out <- rep(mean, n)
      }
      if(sd > 0){
        out <- rtnorm(n, mean = mean, sd = sd, lower = lower, upper = upper)
      }
    }else{
      warning("SD < 0 - NAs produced")
      out <- rep(NA, n)
    }
  }
  
  return(out)
}



qtnorm_dmp <- function(p, mean=0, sd=1, lower=-Inf, upper=Inf){
  
  if(is.na(mean)|is.na(sd)){
    out <- rep(NA, length(p))
    warning("NA values for mu and/or stdev - NAs produced")
  }else{
    if(sd >= 0){
      if(sd == 0 & mean == lower){
        out <- rep(lower, length(p))
      }
      if(sd == 0 & mean < lower){
        out <- rep(NA, length(p))
        warning("mu < lower & SD = 0 - NAs produced")
      }
      if(sd == 0 & mean > lower){
        #out <- qtnorm(p, mean = mean, sd = sd, lower = lower, upper = upper)
        out <- rep(mean, length(p))
      }
      if(sd > 0){
        out <- qtnorm(p, mean = mean, sd = sd, lower = lower, upper = upper)
      }
    }else{
      warning("SD < 0 - NAs produced")
      out <- rep(NA, length(p))
    }
  }
  return(out)
}




