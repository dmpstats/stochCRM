

function(input, output, session) {
  
  # Global Variables
  prevSlctSpecs <- c()
  inputs_BiomParsPlotted <-  NULL
  birdDensPars_plotted <- list(NULL)
  
  turbineOpTempTable <- data.frame(array(0, dim = c(3, 12), 
                                         dimnames = list(c("Wind Availability (%)", "Mean Downtime (%)", "SD Downtime (%)"), month.name)),
               stringsAsFactors = FALSE)
    
  
  # reactive variables
  rv <- reactiveValues(
    addedSpec = NULL,
    biomParsInputs_ls = NULL,
    FlgtHghDstInputs_ls = NULL,
    densParsInputs_ls = NULL
    #inputs_BiometricPars_rv = NULL,
    #rctvBlck_FltHghDstInput = NULL # reactivity blocker for flight Height distributions fileInputs
  )
  
  
  
  
  #' -----------------------------------------------------------------------
  #  ----      Dynamic UI builders based on user selected species       ----
  #' -----------------------------------------------------------------------
  
  
  # ---- Sidebar menuSubItem builder for selected species
  output$menuSubItems_species <- renderMenu({
    
    cSelSpec <- slctSpeciesTags()
    
    if(!is.null(cSelSpec)){
      species_SubItemsList <- cSelSpec %>%
        select(species, specTabNames) %>%
        rename(text = species, tabName = specTabNames) %>%
        pmap(menuSubItem, icon = icon("sliders"))
    }else{
      species_SubItemsList <- NULL
    }
    sidebarMenu(.list = species_SubItemsList)
  })
  
  
  
  #' ---- Logic to pair-up with UI generation of species parameters tabs - get a first-time selected species, returning 
  #' empty character if currently selected species have been previously selected
  observe({
    
    selSpec <- input$selectSpecs
    rv$addedSpec <- selSpec[!selSpec %in% prevSlctSpecs]
    prevSlctSpecs <<- c(prevSlctSpecs, rv$addedSpec)
    
  })
  
  
  # Include UI holding tab with widgets for a species parameters when species is selected for the first time
  observeEvent(rv$addedSpec, {
    
    req(rv$addedSpec)
    
    cSpecTags <- slctSpeciesTags() %>%
      filter(species == rv$addedSpec)
    
    insertUI(
      selector = "#tabItemsEnvelope",
      where = "beforeEnd",
      ui = selectSpecies_UITabBuilder(specName = cSpecTags$species, 
                                      tabName = cSpecTags$specTabNames, 
                                      specLabel = cSpecTags$specLabel, session,
                                      startUpValues = startUpValues)
    )
  })
  
  
  
  
  # Add bsTooltips of pop-ups with info on biometric parameters - needs to be species-specific
  observeEvent(rv$addedSpec,{

    req(rv$addedSpec)

    cSpecTags <- slctSpeciesTags() %>%
      filter(species == rv$addedSpec)

    outTag <- paste0("BiomBStoolTips_", cSpecTags$specLabel)

    output[[outTag]] <- renderUI({

      tagList(
        bsTooltip(id = paste0("lbl_bodyLt_", cSpecTags$specLabel),
                  title = paste0("Bird body length ~ Normal"),
                  options = list(container = "body"),
                  placement = "right", trigger = "hover"),

        bsTooltip(id = paste0("lbl_flType_", cSpecTags$specLabel),
                  title = paste0("Predominant type of flight"),
                  options = list(container = "body"),
                  placement = "right", trigger = "hover"),

        bsTooltip(id = paste0("lbl_wngSpan_", cSpecTags$specLabel),
                  title = paste0("Bird wing spa ~ Normal)"),
                  options = list(container = "body"),
                  placement = "right", trigger = "hover"),
        
        bsTooltip(id = paste0("lbl_flSpeed_", cSpecTags$specLabel),
                  title = paste0("Flight Speed ~ Normal"),
                  options = list(container = "body"),
                  placement = "right", trigger = "hover"),
        
        bsTooltip(id = paste0("lbl_noctAct_", cSpecTags$specLabel),
                  title = paste0("Proportion of nocturnal", 
                                 " activity ~ Normal"),
                  options = list(container = "body"),
                  placement = "right", trigger = "hover"),
        
        bsTooltip(id = paste0("lbl_CRHeight_", cSpecTags$specLabel),
                  title = paste0("Proportion at colision risk height ~ Normal. Required for the basic model (option 1)"),
                  options = list(container = "body"),
                  placement = "right", trigger = "hover"),
        
        bsTooltip(id = paste0("lbl_basicAvoid_", cSpecTags$specLabel),
                  title = paste0("The probability that a bird on a collision course with a turbine will take evading", 
                                 " action to avoid collision (~Normal). Required for the basic model (option 1)"),
                  options = list(container = "body"),
                  placement = "right", trigger = "hover"),
        
        bsTooltip(id = paste0("lbl_extAvoid_", cSpecTags$specLabel),
                  title = paste0("The probability that a bird on a collision course with a turbine will take evading", 
                                 " action to avoid collision (~Normal). Required for the extended model (option 3)"),
                  options = list(container = "body"),
                  placement = "right", trigger = "hover"),
        
        bsTooltip(id = paste0("lbl_monthOPs_", cSpecTags$specLabel),
                  title = paste0("The number of birds in flight during daytime per km2 in each month (~ Truncated Normal bounded by 0 and 2)"),
                  options = list(container = "body"),
                  placement = "right", trigger = "hover")
        
        
      )
    })
  })
  
  


  
    
  
  #' -----------------------------------------------------------------
  #  ----                  Inputs Management                      ----
  #' -----------------------------------------------------------------
  
  # --- Store selected species
  slctSpeciesTags <- reactive({
    
    cSelSpec <- input$selectSpecs
    
    if(!is.null(cSelSpec)){
      tibble(species = input$selectSpecs) %>%
        mutate(specLabel = gsub(" ", "_", species),
               specTabNames = paste0("tab_SpecPars_", specLabel)) 
    }else{
      return(NULL)
    }
  })

  
  #' --- Store current input values of biometric hyperparameters (per species), flight dists inputs 
  #' and monthly densities in reactiveValues
  observeEvent(reactiveValuesToList(input), {
    
    RVs_inputs_ls <- reactiveValuesToList(input)
    
    # Biometric hyperparameters
    rv$biomParsInputs_ls <- RVs_inputs_ls[grep("biomPars", names(RVs_inputs_ls))]
  
    # Monthly density hyperparameters
    rv$birdDensParsInputs_ls <- map(RVs_inputs_ls[grep("birdDensPars", names(RVs_inputs_ls))], hot_to_r)
    
    # Flight heigth distributions locations
    rv$FlgtHghDstInputs_loc <- RVs_inputs_ls[grep("upldInput_dt_FlgHghDst", names(RVs_inputs_ls))]
    
  })
  
  
   
  # observe({
  #   #print(str(reactiveValuesToList(input), max.level = 2, list.len = 5))
  #   print(rv$birdDensParsInputs_ls)
  # })

  

  # --- Call and store flight height distributions of selected species in elements of a list
  flgtHghDstInputs_ls <- eventReactive(rv$FlgtHghDstInputs_loc, {
    
    req(length(rv$FlgtHghDstInputs_loc))
    
    # input list elements of fileInputs are NULL until associate file is uploaded
    # Get rid of null elements (i.e. when a species has been selected but file hasn't been uploaded)
    FHDInputs_locations_ls_uploaded <- discard(rv$FlgtHghDstInputs_loc, is.null)
    
    req(FHDInputs_locations_ls_uploaded)
    
    FHDInputs_locations_ls_uploaded %>%
      map(~read.csv(.$datapath, header = TRUE))  # returns a list with each element holding the uploaded flight height distribution data for the selected species
  })
  
  
  
 
  # --- Create input table for turbine monthly operation parameters
  output$hotInput_turbinePars_monthOps <- renderRHandsontable({

    data.frame(matrix(c(startUpValues$windAvail, startUpValues$meanDownTime, startUpValues$sdDownTime), nrow = 3, ncol = 12, byrow = TRUE,
                       dimnames = list(c("Wind Availability (%)", "Mean Downtime (%)", "SD Downtime (%)"), month.name)),
               stringsAsFactors = FALSE) %>%
      rhandsontable(rowHeaderWidth = 140) %>%
      hot_cols(colWidths = 85) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })

  
  
  # --- Create input table for turbine rotation speed vs windspeed relationship
  output$hotInput_turbinePars_rotationVsWind <- renderRHandsontable({

    # data.frame(array(0, dim = c(10, 2),
    #                  dimnames = list(NULL, c("windSpeed", "rotationSpeed"))),
    #            stringsAsFactors = FALSE) %>%
    startUpValues$rotationVsWind_df %>%
      bind_rows(data.frame(windSpeed = NA, rotationSpeed=NA)) %>%
      rhandsontable(rowHeaders=NULL, colHeaders = c("Wind speed (m/s)", "Rotation speed (rpm)")) %>%
      hot_cols(colWidths = 150) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })



  # --- Create input table for turbine rotation speed vs windspeed relationship
  output$hotInput_turbinePars_pitchVsWind <- renderRHandsontable({

    # data.frame(array(0, dim = c(10, 2),
    #                  dimnames = list(NULL, c("windSpeed", "bladePitch"))),
    #            stringsAsFactors = FALSE) %>%
    startUpValues$pitchVsWind_df %>%
      bind_rows(data.frame(windSpeed = NA, bladePitch=NA)) %>%
      rhandsontable(rowHeaders=NULL, colHeaders = c("Wind speed (m/s)", "Blade Pitch (deg)")) %>%
      hot_cols(colWidths = 150) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })



  
  observeEvent(rv$addedSpec, {
    
    req(rv$addedSpec)
    
    cSpecTags <- slctSpeciesTags() %>%
      filter(species == rv$addedSpec)
    
    hotTag <- paste0("hotInput_birdDensPars_", cSpecTags$specLabel)
    
    walk(hotTag, function(x){
      
      output[[x]] <- renderRHandsontable({
        
        data.frame(matrix(c(startUpValues$meanDensity, startUpValues$sdDensity), nrow = 2, ncol = 12, byrow = TRUE,
                          dimnames = list(c("meanDensity", "sdDensity"), month.name)),
                   stringsAsFactors = FALSE) %>%
          rhandsontable(rowHeaderWidth = 160, 
                        rowHeaders = c("Mean birds/km^2", "SD of birds/km^2")) %>%
          hot_cols(colWidths = 90) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      })  
      
    })
  })
  
  
  # observe({
  # 
  #   req(input$hotInput_turbinePars_pitchVsWind)
  #   df <- hot_to_r(input$hotInput_turbinePars_pitchVsWind)
  #   print(df)
  # 
  # })
  
  
  
  
  
  #' -----------------------------------------------------------------
  #  ----                  Outputs Management                     ----
  #' -----------------------------------------------------------------

  # --- Generates plots for biometric variables based on input values of their hyperparameters
  observeEvent(rv$biomParsInputs_ls, {

    req(length(rv$biomParsInputs_ls)>0)

    inputs_currBiomPars_df <- rv$biomParsInputs_ls %>%
      ldply(function(x){data.frame(Value = as.character(x))}, .id = "inputTags") %>%
      mutate(inputTags_split = str_split(inputTags, "_")) %>%
      mutate(specLabel = map_chr(inputTags_split, function(x) paste(x[-c(1:4)], collapse = "_")),
             par = map_chr(inputTags_split, function(x) x[3]),
             hyper = map_chr(inputTags_split, function(x) x[4]),
             par_hyper = paste(par, hyper, sep = "_"))

    inputs_currBiomParsPlottable <- inputs_currBiomPars_df %>%
      filter(hyper %in% c("E", "SD")) %>%
      select(-inputTags_split) %>%
      mutate(plotTag = paste0("plot_biomPars_", par, "_", specLabel),
             Value = as.numeric(as.character(Value)))

    if(!is.null(inputs_BiomParsPlotted)){
      inputs_currBiomParsPlottable %<>% mutate(inputTags = as.character(inputTags))
      inputs_BiomParsPlotted %<>% mutate(inputTags = as.character(inputTags))
      inputs_BiomParsChanged <- setdiff(inputs_currBiomParsPlottable, inputs_BiomParsPlotted)
      inputs_BiomParsToPlot <- inputs_currBiomParsPlottable %>% filter(plotTag %in% inputs_BiomParsChanged$plotTag)
      # print(inputs_BiomParsChanged)
    }else{
      inputs_BiomParsToPlot <- inputs_currBiomParsPlottable
    }

      #print(inputs_BiomParsToPlot)

      plotTagsToPlot <- unique(inputs_BiomParsToPlot$plotTag)

      if(length(plotTagsToPlot)>0){
        for(i in 1:length(plotTagsToPlot)){
          local({
            c_tag <- plotTagsToPlot[i]
            cPlotData <- inputs_BiomParsToPlot %>% filter(plotTag == c_tag)
            #print(cPlotData)

            mu <- as.numeric(as.character(filter(cPlotData, hyper == "E")$Value))
            stdev <- as.numeric(as.character(filter(cPlotData, hyper == "SD")$Value))

            output[[c_tag]] <- renderPlot({
              #print(c_tag)
              data.frame(qtls = qnorm(c(0.001, 0.999), mean = mu, sd=stdev))  %>%
                ggplot(aes(qtls)) +
                stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), col = "black", size =1.2) +
                stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), geom="area", fill = "darkorange", alpha = 0.3) +
                labs(y="Density", #title=c_tag,
                     x = unique(cPlotData$par))
              #plot(1:10)
            })
          })
        }
        # update values of currently plotted biometric parameters
        inputs_BiomParsPlotted <<- inputs_currBiomParsPlottable
      }
  })

  
  # --- generates plots of the flight height distributions uploaded by the user
  observeEvent(flgtHghDstInputs_ls(), {
    
    req(length(flgtHghDstInputs_ls())>0)
    
    #print(names(flgtHghDstInputs_ls()))
    
    walk2(flgtHghDstInputs_ls(), names(flgtHghDstInputs_ls()), function(x, y){
      
      names(x) <- c("Height", "Proportion")
      specLabel <- map_chr(str_split(y, "_"), function(x) paste(x[-c(1:3)], collapse = "_"))
      plotTag <- paste0("plot_FlgHghDst_", specLabel)
      
      output[[plotTag]] <-  renderPlot({
        x %>%
          filter(Proportion > 0.00001) %>%
          ggplot(aes(Height, Proportion)) +
          geom_col(fill = "orange", alpha=0.7) +
          #coord_flip()+
          labs(y = "Relative Frequency", x = "Height (m)", title = specLabel)
      })
    })
  })
  
  
  
  # --- Generates plots for Turbine parameters
  output$plot_turbinePars_rotRadius <- renderPlot({
    mu <- input$numInput_turbinePars_rotRadius_E_
    stdev <- input$numInput_turbinePars_rotRadius_SD_
    
    data.frame(qtls = qnorm(c(0.001, 0.999), mean = mu, sd=stdev))  %>%
      ggplot(aes(qtls)) +
      stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), col = "black", size =1.2) +
      stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), geom="area", fill = "olivedrab", alpha = 0.3) +
      labs(y="Density", #title=c_tag,
           x = "Rotation Radius (m)")
    })
  
  
  output$plot_turbinePars_hubHght <- renderPlot({
    mu <- input$numInput_turbinePars_hubHght_E_
    stdev <- input$numInput_turbinePars_hubHght_SD_
    
    data.frame(qtls = qnorm(c(0.001, 0.999), mean = mu, sd=stdev))  %>%
      ggplot(aes(qtls)) +
      stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), col = "black", size =1.2) +
      stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), geom="area", fill = "olivedrab", alpha = 0.3) +
      labs(y="Density", #title=c_tag,
           x = "Hub Height (m)")
  })
  
  
  output$plot_turbinePars_maxBladeWdth <- renderPlot({
    mu <- input$numInput_turbinePars_maxBladeWdth_E_
    stdev <- input$numInput_turbinePars_maxBladeWdth_SD_
    
    data.frame(qtls = qnorm(c(0.001, 0.999), mean = mu, sd=stdev))  %>%
      ggplot(aes(qtls)) +
      stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), col = "black", size =1.2) +
      stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), geom="area", fill = "olivedrab", alpha = 0.3) +
      labs(y="Density", #title=c_tag,
           x = "Maximum Blade Width (m)")
  })
  
  
  
  output$plot_turbinePars_rotnSpeed <- renderPlot({
    mu <- input$numInput_turbinePars_rotnSpeed_E_
    stdev <- input$numInput_turbinePars_rotnSpeed_SD_
    
    data.frame(qtls = qnorm(c(0.001, 0.999), mean = mu, sd=stdev))  %>%
      ggplot(aes(qtls)) +
      stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), col = "black", size =1.2) +
      stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), geom="area", fill = "olivedrab", alpha = 0.3) +
      labs(y="Density", #title=c_tag,
           x = "Rotation Speed (rpm)")
  })
  
  
  output$plot_turbinePars_bladePitch <- renderPlot({
    mu <- input$numInput_turbinePars_bladePitch_E_
    stdev <- input$numInput_turbinePars_bladePitch_SD_
    
    data.frame(qtls = qnorm(c(0.001, 0.999), mean = mu, sd=stdev))  %>%
      ggplot(aes(qtls)) +
      stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), col = "black", size =1.2) +
      stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), geom="area", fill = "olivedrab", alpha = 0.3) +
      labs(y="Density", #title=c_tag,
           x = "Blade Pitch (degrees)")
  })
  
  
  
  
  output$plot_turbinePars_monthOps_windAvb <- renderPlot({
    
    req(input$hotInput_turbinePars_monthOps)
    
    hot_to_r(input$hotInput_turbinePars_monthOps) %>% rownames_to_column(var="Variable") %>% slice(1) %>%
      gather(month, windAvb, -Variable) %>% select(-Variable) %>%
      mutate(month = factor(month.abb, levels = month.abb)) %>%
      ggplot(aes(x=month, y=windAvb)) +
      geom_col(fill= "olivedrab", alpha = 0.7, width = 0.4) +
      labs(x="", y = "Wind Availability (%)", title = "Monthly wind availability")
    
  })

  
  
  output$plot_turbinePars_monthOps_downtime <- renderPlot({
    
    req(input$hotInput_turbinePars_monthOps)
  
    hot_to_r(input$hotInput_turbinePars_monthOps) %>% 
      rownames_to_column(var="Variable") %>% 
      slice(2:3) %>%
      gather(month, windAvb, -Variable) %>%
      mutate(month = factor(month, levels=unique(month), labels = month.abb)) %>%
      spread(Variable, windAvb) %>%
      rename(meanDwnTm = `Mean Downtime (%)`, sdDwnTm = `SD Downtime (%)`) %>%
      mutate(lwBound = qnorm(p=0.025, mean = meanDwnTm, sd = sdDwnTm),
             upBound = qnorm(p=c(0.975), mean = meanDwnTm, sd = sdDwnTm)) %>%
      ggplot(aes(x=month, y = meanDwnTm, group=month)) +
      geom_pointrange(aes(ymin=lwBound, ymax=upBound), col = "olivedrab", size =0.8) +
      labs(y = "Downtime (%)", x = "", title = "Monthly turbine downtime (Means & 95% CIs)")
  })  

  
  
  output$plot_miscPars_windSpeed <- renderPlot({
    
    mu <- input$numInput_miscPars_windSpeed_E_
    stdev <- input$numInput_miscPars_windSpeed_SD_
    
    data.frame(qtls = qtnorm(c(0.001, 0.999), mean = mu, sd=stdev, lower = 0))  %>%
      ggplot(aes(qtls)) +
      stat_function(fun=dtnorm, args = list(mean = mu, sd = stdev, lower = 0), col = "black", size =1.2) +
      stat_function(fun=dtnorm, args = list(mean = mu, sd = stdev, lower = 0), geom="area", fill = "olivedrab", alpha = 0.3) +
      labs(y="Density", #title=c_tag,
           x = "Wind Seed (m/s)")
    
  })
  
  
  
  
  
  observeEvent(rv$birdDensParsInputs_ls, {
    
    req(length(rv$birdDensParsInputs_ls)>0)
    
    tabChanged_ls <- sapply(names(rv$birdDensParsInputs_ls), function(x){
      !identical(rv$birdDensParsInputs_ls[[x]], birdDensPars_plotted[[x]])
    })

    birdDensPars_toPlot <- rv$birdDensParsInputs_ls[c(which(tabChanged_ls == TRUE))]
    
    # print(birdDensPars_plotted)
    # print(rv$birdDensParsInputs_ls)
    # print(tabChanged_ls)
    # print(birdDensPars_toPlot)
    
    walk2(birdDensPars_toPlot, names(birdDensPars_toPlot), function(x, y){
      
      specLabel <- map_chr(str_split(y, "_"), function(x) paste(x[-c(1:2)], collapse = "_"))
      specName <- gsub("_", " ", specLabel)
      plotTag <- paste0("plot_birdDensPars_", specLabel)
  
      # print(plotTag)
      
      output[[plotTag]] <- renderPlot({

        #browser()
        
        as.data.frame(t(x)) %>% 
          rownames_to_column(var = "month") %>%
          mutate(month = factor(month, levels = month)) %>%
          group_by(month) %>%
          mutate(med = qtnorm_possibly(p=0.5, mean = meanDensity, sd = sdDensity + 1e-10, 0, 2),       # Truncated Normal bounded by 0 and 2 - ajustment on SD to workaround the issue of qtnorm error when mu==0 & sd==0
                 lwBound = qtnorm_possibly(p=0.025, mean = meanDensity, sd = sdDensity + 1e-10, 0, 2),   
                 upBound = qtnorm_possibly(p=0.975, mean = meanDensity, sd = sdDensity + 1e-10, 0, 2)) %>%
        # upBound = qnbinom(p=0.025, mu = meanDensity, size = meanDensity/(phiDensity - 1)),  # QuasiPoisson hack, phiDensity must be >=1
        # lwBound = qnbinom(p=0.975, mu = meanDensity, size = meanDensity/(phiDensity - 1)),
        # upBound = if_else(is.na(upBound), 0, upBound),
        # lwBound = if_else(is.na(lwBound), 0, lwBound)) %>%
        ggplot(aes(x=month, y = meanDensity, group=month)) +
          #geom_pointrange(aes(ymin=lwBound, ymax=upBound), col = "blue") +
          geom_errorbar(aes(ymin=lwBound, ymax=upBound), col = "darkorange", width = 0.2, size = 1) +
          geom_point(col="darkorange", size = 3) +
          labs(y = "Number of birds per km2", x = "", title = paste0(specName, " (Mean and 95% CI)"))
      })
    })
    
    birdDensPars_plotted <<- rv$birdDensParsInputs_ls # birdDensPars_toPlot 
  })
  
  
  
  
  #' --------------------------------------------------------------------------------
  #  ----         Inputs Validation System with feedback to UI                   ----
  #' --------------------------------------------------------------------------------
  
  observe({

    if(req(input$numInput_windfarmPars_targetPower) <= 0){
      
      createAlert(session, anchorId = "alert", alertId = "exampleAlert", title = "Oops",
                  content = "Target Power must be positive", append = TRUE, style = "danger")
    }else{
      closeAlert(session, alertId = "exampleAlert")
    }
  })
  
  
  
  
  
  #' -----------------------------------------------------------------------
  #  ----            Collision Risk Simulation Model                    ----
  #' -----------------------------------------------------------------------

#   observe({
#     #print(str(reactiveValuesToList(input), max.level = 2, list.len = 5))
#     #print(rv$biomParsInputs_ls)
#     print(rv$biomParsInputs_ls[grep(pattern = "basicAvoid_E", names(rv$biomParsInputs_ls))])
#     print(slctSpeciesTags()$specLabel)
#   })
  
  

  observeEvent(input$actButtonInput_simulPars_GO, {


    #--- step 1: gather and arrange all the current input value to run in simulation function ----- # 

    # -- bird biometric data 
    birdData <- rv$biomParsInputs_ls %>%
      ldply(function(x){data.frame(Value = as.character(x))}, .id = "inputTags") %>%
      mutate(inputTags_split = str_split(inputTags, "_")) %>%
      mutate(specLabel = map_chr(inputTags_split, function(x) paste(x[-c(1:4)], collapse = "_")),
             par = map_chr(inputTags_split, function(x) x[3]),
             hyper = map_chr(inputTags_split, function(x) x[4]),
             par_hyper = paste(par, hyper, sep = "_")) %>%
      select(-c(inputTags, inputTags_split, par, hyper)) %>% 
      spread(par_hyper, Value) %>%
      rename(Species = specLabel, AvoidanceBasic = basicAvoid_E, AvoidanceBasicSD = basicAvoid_SD, 
             AvoidanceExtended = extAvoid_E, AvoidanceExtendedSD = extAvoid_SD, Body_Length = bodyLt_E, Body_LengthSD = bodyLt_SD,
             Wingspan = wngSpan_E,  WingspanSD = wngSpan_SD, Flight_Speed  = flSpeed_E, Flight_SpeedSD = flSpeed_SD,
             Nocturnal_Activity = noctAct_E, Nocturnal_ActivitySD = noctAct_SD, Flight = flType_tp, 
             Prop_CRH_Obs = CRHeight_E, Prop_CRH_ObsSD = CRHeight_SD) %>%
      select(Species, AvoidanceBasic, AvoidanceBasicSD, AvoidanceExtended, AvoidanceExtendedSD, Body_Length, Body_LengthSD,
             Wingspan, WingspanSD, Flight_Speed, Flight_SpeedSD, Nocturnal_Activity, Nocturnal_ActivitySD, Flight, 
             Prop_CRH_Obs, Prop_CRH_ObsSD)
    
    
    # -- turbine data 
    turbineData_Operation <- hot_to_r(input$hotInput_turbinePars_monthOps) %>%  
      rownames_to_column(var="Variable") %>%
      gather(month, Value, -Variable) %>%
      mutate(Variable = gsub(" \\(%\\)", "", Variable),
             Variable = gsub(" ", "_", Variable)) %>%
      unite(Variable, month, col = "Variable") %>%
      mutate(TurbineModel = input$numInput_turbinePars_turbinePower, 
             VariableMasden = paste0(rep(month.abb, each=3), "Op", c("", "Mean", "SD")),
             VariableMasden = factor(VariableMasden, levels = VariableMasden)
      ) %>%
      select(-Variable) %>%
      spread(VariableMasden, Value)
    
    #' managing the option of prodist vs windspeed - relationship for rotation speed and blade pitch 
    #' Model function expects NAs on associated hyperparameters when the latter in optioned
    rotSpeed_E <- ifelse(input$radButtonInput_turbinePars_rotSpdInputOption == 'probDist',   
                           input$numInput_turbinePars_rotnSpeed_E_, NA)
    rotSpeed_SD <- ifelse(input$radButtonInput_turbinePars_rotSpdInputOption == 'probDist', 
          input$numInput_turbinePars_rotnSpeed_SD_, NA)
    
    pitch_E <- ifelse(input$radButtonInput_turbinePars_bldPitchInputOption == 'probDist', 
           input$numInput_turbinePars_bladePitch_E_, NA)
    
    pitch_SD <- ifelse(input$radButtonInput_turbinePars_bldPitchInputOption == 'probDist', 
           input$numInput_turbinePars_bladePitch_SD_, NA)
    
    # merging turbine data
    turbineData <- tibble(
      TurbineModel = input$numInput_turbinePars_turbinePower,
      Blades = input$numInput_turbinePars_numBlades,
      RotationSpeed = rotSpeed_E,
      RotationSpeedSD = rotSpeed_SD,
      RotorRadius	= input$numInput_turbinePars_rotRadius_E_,
      RotorRadiusSD	= input$numInput_turbinePars_rotRadius_SD_,
      HubHeightAdd = input$numInput_turbinePars_hubHght_E_,
      HubHeightAddSD	= input$numInput_turbinePars_hubHght_SD_,
      BladeWidth	= input$numInput_turbinePars_maxBladeWdth_E_,
      BladeWidthSD = input$numInput_turbinePars_maxBladeWdth_SD_,
      Pitch = pitch_E,
      PitchSD = pitch_SD
    ) %>%
      left_join(., turbineData_Operation, by = "TurbineModel")
  
    
    # -- birds counts data
    if(length(rv$birdDensParsInputs_ls)>0){
      countData <- rv$birdDensParsInputs_ls %>%
        map2_df(., names(.), function(x,y){
          x %>%
            rownames_to_column(var="hyperPar") %>%
            add_column(., inputTags = y, .before = 1)
        }) %>%
        mutate(inputTags_split = str_split(inputTags, "_")) %>%
        mutate(specLabel = map_chr(inputTags_split, function(x) paste(x[-c(1:2)], collapse = "_"))) %>%
        select(specLabel, hyperPar:December) %>%
        gather(month, Value, -c(specLabel, hyperPar)) %>%
        group_by(specLabel) %>%
        mutate(VariableMasden = factor(paste0(rep(month.abb, each=2), c("", "SD"))),
               VariableMasden = factor(VariableMasden, levels = VariableMasden)
        ) %>%
        select(-c(hyperPar:month)) %>%
        spread(VariableMasden, Value)
    }else{
      countData <- NULL
    }


    # --- rotor speed and pitch vs windspeed
    windPowerData <- left_join(hot_to_r(input$hotInput_turbinePars_rotationVsWind), # produces "Warning in asMethod(object) : NAs introduced by coercion"
                               hot_to_r(input$hotInput_turbinePars_pitchVsWind),    # produces "Warning in asMethod(object) : NAs introduced by coercion"
                               by = "windSpeed") %>%
      rename(Wind = windSpeed, Rotor = rotationSpeed, Pitch = bladePitch) %>%
      drop_na()

    # model function expects a csv file named according to the turbine model/power - change code to accept a dataframe directly
    write.csv(windPowerData, file = paste0("data/windpower_", input$numInput_turbinePars_turbinePower, ".csv"), row.names = FALSE)

    
    # ----- step 2: Set progress bar ----- #
    
    # Create a Progress objects for species and iterations within each species
    progress_Spec <- shiny::Progress$new()
    progress_Iter <- shiny::Progress$new()
    
    progress_Spec$set(message = "Processing Species", value = 0)
    progress_Iter$set(message = "Working through iterations", value = 0)
    
    on.exit({
      progress_Iter$close()
      progress_Spec$close()
      })
    #on.exit()
    

    #' callback functions to update progress on species.
    updateProgress_Spec <- function(value = NULL, detail = NULL) {
      progress_Spec$set(value = value, detail = detail)
    }
    
    #' callback functions to update progress on iterations. Each time updateProgress_Iter() is called, 
    #' it moves the bar 1/nth of the total distance.
    # updateProgress_Iter <- function(detail = NULL, n = NULL) {
    #   progress_Iter$inc(amount = 1/n, detail = detail)
    # }
    updateProgress_Iter <- function(value = NULL, detail = NULL) {
      progress_Iter$set(value = value, detail = detail)
    }
    
        

    # ----- step 3: run simulation function ----- # 
    
    if(1){
      stochasticBand(
        workingDirectory,
        results_folder = "results",
        BirdDataFile = birdData,
        TurbineDataFile = turbineData,
        CountDataFile = countData,
        FlightDataFile = "data/FlightHeight.csv",
        iter = input$sldInput_simulPars_numIter, 
        CRSpecies = slctSpeciesTags()$specLabel, #CRSpecies = c("Black_legged_Kittiwake"),
        TPower = input$numInput_windfarmPars_targetPower, #600
        LargeArrayCorrection = ifelse(input$chkBoxInput_simulPars_largeArrarCorr==TRUE, "yes", "no"), # "yes",
        WFWidth = input$numInput_windfarmPars_width,
        Prop_Upwind = input$sldInput_windfarmPars_upWindDownWindProp/100, # convert % (user input) to proportion (expected by model)
        Latitude = input$numInput_windfarmPars_Latitude,
        TideOff = input$numInput_windfarmPars_tidalOffset,
        windSpeedMean = input$numInput_miscPars_windSpeed_E_, 
        windSpeedSD = input$numInput_miscPars_windSpeed_SD_,
        windPowerData = windPowerData,
        updateProgress_Spec,  # pass in the updateProgress function so that it can update the progress indicator.
        updateProgress_Iter
      )
    }
    
    
     output$out_simFunctionArgs <- renderPrint({
      isolate(
        print(list(
          BirdDataFile = birdData,
          TurbineDataFile = turbineData,
          CountDataFile = countData,
          iter = input$sldInput_simulPars_numIter,
          CRSpecies = slctSpeciesTags()$specLabel,
          TPower = input$numInput_windfarmPars_targetPower,
          WFWidth = input$numInput_windfarmPars_width,
          LargeArrayCorrection = ifelse(input$chkBoxInput_simulPars_largeArrarCorr==TRUE, "yes", "no"),
          rop_Upwind = input$sldInput_windfarmPars_upWindDownWindProp,
          Latitude = input$numInput_windfarmPars_Latitude,
          TideOff = input$numInput_windfarmPars_tidalOffset,
          windSpeedMean = input$numInput_miscPars_windSpeed_E_,
          windSpeedSD = input$numInput_miscPars_windSpeed_SD_,
          windPowerData = windPowerData
        ))
      )
    })
     
  })

  
  

  
  
  
  
  #' ------------------------------------------------------------------
  #  ----         Debugging and value checking tools            ----
  #' ------------------------------------------------------------------
   
  output$inputRVs <- renderPrint({
    str(reactiveValuesToList(input), max.level = 3)
  })
  
  output$out_inputs_biom <- renderPrint({
    str(reactiveValuesToList(rv), max.level = 3)
  })

  # output$out_inputs_monthDens <- renderPrint({
  #   print(inputs_monthDensPars())
  # })

  observe(label="console",{
    if(input$console != 0) {
      options(browserNLdisabled=TRUE)
      saved_console<-".RDuetConsole"
      if (file.exists(saved_console)) load(saved_console)
      isolate(browser())
      save(file=saved_console,list=ls(environment()))
    }
  })
    
}




