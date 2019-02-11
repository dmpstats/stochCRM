

function(input, output, session) {
  
  
  #' ----------------------------------------------------
  #  ----         setting session specifics          ----
  #' ----------------------------------------------------
  
  # --- session's "global" Variables
  prevSlctSpecs <- c()
  inputs_BiomParsPlotted <-  NULL
  birdDensPars_plotted <- list(NULL)
  flgtHghDstInputs_ls_Latest <- list(NULL)
  restoringStage <- FALSE
  
  
  # --- Initiate session's reactive variables
  rv <- reactiveValues(
    addedSpec = NULL,
    biomParsInputs_ls = NULL,
    birdDensParsInputs_ls = NULL,
    summaryTables_ls = NULL,
    sCRM_output_ls = NULL,
    turbinePars_monthOps_df = data.frame(
      matrix(
        c(startUpValues$turbinePars$windAvail, startUpValues$turbinePars$meanDownTime, startUpValues$turbinePars$sdDownTime),
        nrow = 3, ncol = 12, byrow = TRUE, 
        dimnames = list(c("Wind Availability (%)", "Mean Downtime (%)", "SD Downtime (%)"), month.name)
      ),
      stringsAsFactors = FALSE
    ),
    pitchVsWind_df = startUpValues$turbinePars$pitchVsWind_df,
    rotationVsWind_df = startUpValues$turbinePars$rotationVsWind_df,
    startSpecMonthDens_df = map(startUpValues$speciesPars, ~data.frame(meanDensity = .$"meanDensity", sdDensity = .$"sdDensity")),
    selectSpecRestored = 0
  )
  
  
  
  # --- Generate temporary paths and folders for storing session's specific data 
  sessTempOutFolder <- getTempFolderName()
  path2ShinyOut_Inputs <- file.path("shinyOutputs", sessTempOutFolder, "inputs")
  path2ShinyOut_Outputs <- file.path("shinyOutputs", sessTempOutFolder, "outputs")
  path2Outputs_results <- file.path("results", sessTempOutFolder)
  
  dir.create(path2ShinyOut_Inputs, recursive = TRUE)
  dir.create(path2ShinyOut_Outputs, recursive = TRUE)
  dir.create(path2Outputs_results, recursive = TRUE)
  
  

  
  
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
  
  
  
  #' ---- Logic to pair-up with UI generation of species parameters tabs:
  #' Get a first-time selected species, returning empty character if currently 
  #' selected species have been previously selected
  observe({
    
    selSpec <- input$selectSpecs
    rv$addedSpec <- selSpec[!selSpec %in% prevSlctSpecs]
    prevSlctSpecs <<- c(prevSlctSpecs, rv$addedSpec)
    
  })
  

  
  # Include UI holding tab with widgets for a species parameters when species is selected for the first time
  observeEvent(rv$addedSpec, {
    
    req(rv$addedSpec)
    
    walk(rv$addedSpec, function(x, specTags = slctSpeciesTags()){
    
      #browser()
      
      cSpecTags <- specTags %>% filter(species == x)
      
      insertUI(
        selector = "#tabItemsEnvelope",
        where = "beforeEnd",
        ui = selectSpecies_UITabBuilder(session = session,
                                        specName = cSpecTags$species, 
                                        tabName = cSpecTags$specTabNames, 
                                        specLabel = cSpecTags$specLabel,
                                        specStartVals = startUpValues$speciesPars[[cSpecTags$specLabel]]
                                        )
      )
      
    })
  })
  
  
  
  
  # Add bsTooltips for pop-ups with info on biometric parameters as new species are selected/added - needs to be added a-posteriori
  observeEvent(rv$addedSpec,{
    
    req(rv$addedSpec)
    
    walk(rv$addedSpec, function(x, slctSpecies = slctSpeciesTags()){
      
      cSpecTags <- slctSpecies %>% filter(species == x)
      outTag <- paste0("BiomBStoolTips_", cSpecTags$specLabel)
      
      output[[outTag]] <- renderUI({
        
        tagList(
          bsTooltip(id = paste0("lbl_bodyLt_", cSpecTags$specLabel),
                    title = paste0("Bird body length (~ Truncated Normal with lower bound at 0)"),
                    options = list(container = "body"),
                    placement = "right", trigger = "hover"),
          
          bsTooltip(id = paste0("lbl_flType_", cSpecTags$specLabel),
                    title = paste0("Predominant type of flight"),
                    options = list(container = "body"),
                    placement = "right", trigger = "hover"),
          
          bsTooltip(id = paste0("lbl_wngSpan_", cSpecTags$specLabel),
                    title = paste0("Bird wing span (~ Truncated Normal with lower bound at 0)"),
                    options = list(container = "body"),
                    placement = "right", trigger = "hover"),
          
          bsTooltip(id = paste0("lbl_flSpeed_", cSpecTags$specLabel),
                    title = paste0("Flight Speed (~ Truncated Normal with lower bound at 0)"),
                    options = list(container = "body"),
                    placement = "right", trigger = "hover"),
          
          bsTooltip(id = paste0("lbl_noctAct_", cSpecTags$specLabel),
                    title = paste0("Proportion (0-1) of nocturnal", 
                                   " activity (~ Beta)"),
                    options = list(container = "body"),
                    placement = "right", trigger = "hover"),
          
          bsTooltip(id = paste0("lbl_CRHeight_", cSpecTags$specLabel),
                    title = paste0("Proportion (0-1) at colision risk height (~ Beta). Required for the basic model (option 1)"),
                    options = list(container = "body"),
                    placement = "right", trigger = "hover"),
          
          bsTooltip(id = paste0("lbl_basicAvoid_", cSpecTags$specLabel),
                    title = paste0("The probability (0-1) that a bird on a collision course with a turbine will take evading", 
                                   " action to avoid collision (~ Beta). Required for the basic model (option 1)"),
                    options = list(container = "body"),
                    placement = "right", trigger = "hover"),
          
          bsTooltip(id = paste0("lbl_extAvoid_", cSpecTags$specLabel),
                    title = paste0("The probability (0-1) that a bird on a collision course with a turbine will take evading", 
                                   " action to avoid collision (~ Beta). Required for the extended model (option 3)"),
                    options = list(container = "body"),
                    placement = "right", trigger = "hover"),
          
          bsTooltip(id = paste0("lbl_monthOPs_", cSpecTags$specLabel),
                    title = paste0("The number of birds in flight during daytime per km2 in each month"),
                    options = list(container = "body"),
                    placement = "right", trigger = "hover"),
          
          bsTooltip(id = paste0("lbl_FHD_", cSpecTags$specLabel),
                    title = paste0("Bootstrap samples of the proportion of birds flying within 1m height bands, between 1m and 300m. Required for model Options 2 & 3."),
                    options = list(container = "body"),
                    placement = "right", trigger = "hover")
        )
      })
    })
  })
  
  
  # --- render UI with upload button for User's FHD data - needs to be done via renderUI for tipify to work - fussy!!!!
  observeEvent(rv$addedSpec, {
    
    walk(rv$addedSpec, function(x){
      
      specLabel <- slctSpeciesTags() %>% filter(species == x) %>% pull(specLabel) #gsub("\\s|-", "_", x)
      uiTag <- paste0("renderUI_uploadUserFHD_", specLabel)
      
      output[[uiTag]] <- renderUI({
        
        div(
          column(4,
                 tipify(      
                   fileInput(inputId = paste0("upldInput_FHD_userDt_", specLabel),
                             label = "Upload File",
                             multiple = FALSE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),
                   title = "Model expects dataset with the first column comprising 1m height bands and remaining columns with bootstrap samples of the specie`s FHD. Using the provided template is highly recommended",
                   placement = "left", trigger = "hover", options = list(container = "body")
                 )
          ),
          column(1,
                 style = "margin-top: 25px; margin-left: -10px",
                 downloadButton(outputId = paste0("dwnld_template_FHD_", specLabel), label = NULL, class = "butt"),
                 tags$head(tags$style(".butt{background-color:#4570a5;
                                  color: #efecec}
                                  .butt:hover{
                                  background-color:#4570a5;
                                  color: #efecec}"
                 ))
          ),
          
          bsTooltip(id = paste0("dwnld_template_FHD_", specLabel),
                    title = "Template dataset. Fill in, save locally and upload on the adjacent field",
                    options = list(container = "body"),
                    placement = "right", trigger = "hover")
        )
      })  
    })
  })
  
  
  # --- render UI with upload buttons for user's reference points AND samples of monthly densities - needs to be done via renderUI for tipify to work - fussy!!!!
  observeEvent(rv$addedSpec, {
    
    walk(rv$addedSpec, function(x){
    
      specLabel <- slctSpeciesTags() %>% filter(species == x) %>% pull(specLabel) #gsub(" ", "_", x)
      
      # upload button and template for distribution reference points for monthly densities
      uiTag_summ <- paste0("renderUI_userUpload_monthDens_summaries_", specLabel)
      
      output[[uiTag_summ]] <- renderUI({
        
        divUI_monthDens_PctlsAndSample_DownButtons(
          introText = paste0("Provide reference points of the distributions of monthly densities of ", x), 
          fileInputId = paste0("upldInput_monthDens_userDt_summaries_", specLabel), 
          fileInputPopUpText = "Data with reference points of distributions of monthly bird densities. Provide at least Min, 2.5th, 50th, 97.5th percentiles & Max (blank cells for no data). Downloading & using the adjacent template is highly recommended",
          downButtOutputId = paste0("dwnld_template_monthDens_summaries_", specLabel))
        
      })
      
      # upload button and template for samples for monthly densities
      uiTag_sample <- paste0("renderUI_userUpload_monthDens_samples_", specLabel)
      
      output[[uiTag_sample]] <- renderUI({
        
        divUI_monthDens_PctlsAndSample_DownButtons(
          introText = paste0("Provide random samples from the distributions of monthly densities of ", x), 
          fileInputId = paste0("upldInput_monthDens_userDt_samples_", specLabel), #x), 
          fileInputPopUpText = "Data with random samples from distributions of monthly bird densities. Provide at least 1000 draws. Downloading & using the adjacent template is highly recommended",
          downButtOutputId = paste0("dwnld_template_monthDens_samples_", specLabel))
      })
      
    })
    
  })
  

  
  
  # --- render UI section for model outputs dynamically, according to the selected species.
  output$simResults_UI <- renderUI({
    
    req(rv$sCRM_output_ls)
    
    modelOutputs <- rv$sCRM_output_ls
    
    cSelSpec <- isolate(slctSpeciesTags())
    
    tabs <- map2(cSelSpec$species, cSelSpec$specLabel, results_tabPanelsBuilder)
    
    tagList(
      column(2, align = "right", offset = 10, 
             tipify(downloadButton("dwnld_ModelOuts", "Download Outputs"), 
                    title = "Download zip file with plots and tables presented below", 
                    placement = "left", trigger = "hover", options = list(container = "body"))),
      br(),
      br(),
      invoke(tabBox, tabs,  width = 12) #, height = "250px")
    )
  })
  
  


  
    
  
  #' -----------------------------------------------------------------
  #  ----                  Inputs Management                      ----
  #' -----------------------------------------------------------------
  
  # --- Store selected species
  slctSpeciesTags <- reactive({
    
    cSelSpec <- input$selectSpecs
    
    if(!is.null(cSelSpec)){
      tibble(species = input$selectSpecs) %>%
        mutate(specLabel = gsub("\\s|-", "_", species),
               specTabNames = paste0("tab_SpecPars_", specLabel)) 
    }else{
      return(NULL)
    }
  })

  
  #' --- Store current input values of biometric hyperparameters (per species), flight dists inputs 
  #' and monthly densities in reactiveValues
  observeEvent(reactiveValuesToList(input), {
    
    #browser()
    
    RVs_inputs_ls <- reactiveValuesToList(input)
    
    # Biometric hyperparameters
    rv$biomParsInputs_ls <- RVs_inputs_ls[grep("biomPars", names(RVs_inputs_ls))]
  
    # Monthly density hyperparameters
    rv$birdDensParsInputs_ls <- map(RVs_inputs_ls[grep("hotInput_birdDensPars_tnorm", names(RVs_inputs_ls))], hot_to_r)
    
    # user uploaded monthly densities reference points file location
    rv$mthDens_userDtSumm_loc <- RVs_inputs_ls[grep("upldInput_monthDens_userDt_summaries_", names(RVs_inputs_ls))]
    
    # user uploaded monthly densities random samples file location 
    rv$mthDens_userDtSample_loc <- RVs_inputs_ls[grep("upldInput_monthDens_userDt_samples_", names(RVs_inputs_ls))]
    
    # User options for monthly density data 
    rv$mthDens_userOptions <- RVs_inputs_ls[grep("slctInput_userOpts_monthDens_sampler_", names(RVs_inputs_ls))]
    
    # Flight heigth distributions locations
    rv$FlgtHghDstInputs_loc <- RVs_inputs_ls[grep("FHD_userDt_", names(RVs_inputs_ls))]
    
  })
  
 
#   observe({
#     #print(str(reactiveValuesToList(input), max.level = 2, list.len = 5))
#     #print(rv$birdDensParsInputs_ls)
#     #print(rv$mthDens_userDtSample_loc)
#     #print(rv$mthDens_userOptions)
#     #print(rv$FlgtHghDstInputs_loc)
#     #print(rv$monthDens_userDt_sample_loc)
#     #print(rv$FlgtHghDstInputs_loc)
#   })


  
  # --- Call and store flight height distributions of selected species in elements of a list
  flgtHghDstInputs_ls <- eventReactive(rv$FlgtHghDstInputs_loc, {

    req(length(rv$FlgtHghDstInputs_loc)>0)
    
    # input list elements of fileInputs are NULL until associate file is uploaded
    # Get rid of null elements (i.e. when a species has been selected but file hasn't been uploaded)
    FHDInputs_locations_ls_uploaded <- discard(rv$FlgtHghDstInputs_loc, is.null)
    
    req(length(FHDInputs_locations_ls_uploaded)>0)
    
    flgtHghDstInputs_ls <- FHDInputs_locations_ls_uploaded %>%
      map(~fread(.$datapath))  # returns a list with each element holding the uploaded flight height distribution data for the selected species
    
    return(flgtHghDstInputs_ls)
    
  })
  

  
  # --- Call and store User's data for monthly densities reference points
  monthDens_userDt_summs_ls <- eventReactive(rv$mthDens_userDtSumm_loc, {
    
    req(length(rv$mthDens_userDtSumm_loc) > 0)

    # Get rid of null elements (i.e. when a species has been selected but file hasn't been uploaded)
    uploadedLocations_ls <- discard(rv$mthDens_userDtSumm_loc, is.null)

    req(length(uploadedLocations_ls) > 0)

    uploadedData_ls <- uploadedLocations_ls %>%
      map(~read.csv(.$datapath, header = TRUE))  # returns a list with each element holding the uploaded data for the selected species
    
    return(uploadedData_ls)
  })

  
  
  # --- Call and store User's data for monthly densities reference points
  monthDens_userDt_sample_ls <- eventReactive(rv$mthDens_userDtSample_loc, {
    
    req(length(rv$mthDens_userDtSample_loc)>0)
    
    # Get rid of null elements (i.e. when a species has been selected but file hasn't been uploaded)
    uploadedLocations_ls <- discard(rv$mthDens_userDtSample_loc, is.null)
    
    req(length(uploadedLocations_ls) > 0)
    
    uploadedData_ls <- uploadedLocations_ls %>%
      map(~read.csv(.$datapath, header = TRUE))  # returns a list with each element holding the uploaded data for the selected species
    
    return(uploadedData_ls)
  })
  
  

  

  
  # --- Create input table for turbine monthly operation parameters
  output$hotInput_turbinePars_monthOps <- renderRHandsontable({

    rv$turbinePars_monthOps_df %>%
      rhandsontable(rowHeaderWidth = 140) %>%
      hot_cols(colWidths = 85, 
               renderer = "function (instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.NumericRenderer.apply(this, arguments);
               if (value.length == 0) {
               td.style.background = 'pink';
               }}") %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })
  
  

  # --- Create input table for turbine rotation speed vs windspeed relationship
  output$hotInput_turbinePars_rotationVsWind <- renderRHandsontable({

    rv$rotationVsWind_df %>%
      bind_rows(data.frame(windSpeed = NA, rotationSpeed=NA)) %>%
      rhandsontable(rowHeaders=NULL, colHeaders = c("Wind speed (m/s)", "Rotation speed (rpm)")) %>%
      hot_cols(colWidths = 150) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })



  # --- Create input table for turbine rotation speed vs windspeed relationship
  output$hotInput_turbinePars_pitchVsWind <- renderRHandsontable({
    
    rv$pitchVsWind_df %>%
      bind_rows(data.frame(windSpeed = NA, bladePitch=NA)) %>%
      rhandsontable(rowHeaders=NULL, colHeaders = c("Wind speed (m/s)", "Blade Pitch (deg)")) %>%
      hot_cols(colWidths = 150) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })



  
  
  # --- Create input table for bird monthly densities
  observe({
    
    req(rv$addedSpec)
    
    walk(rv$addedSpec, function(x, slctSpecTags = isolate(slctSpeciesTags()), startSpecMonthDens = rv$startSpecMonthDens_df){
      
      cSpecTags <- slctSpecTags %>% filter(species == x)
      
      # input table for truncated normal parameters
      hotTag_truncNorm <- paste0("hotInput_birdDensPars_tnorm_", cSpecTags$specLabel)  
      cStartSpecMonthDens <- startSpecMonthDens[[cSpecTags$specLabel]]
      
      output[[hotTag_truncNorm]] <- renderRHandsontable({
        
        if(is_empty(cStartSpecMonthDens)){
          cStartSpecMonthDens <- data.frame(meanDensity = rep(1, 12), sdDensity = rep(0.001, 12))
        }
        
        data.frame(cStartSpecMonthDens) %>%
          mutate(month = month.name) %>%
          column_to_rownames(var = "month") %>%
          t() %>%
          rhandsontable(rowHeaderWidth = 160, 
                        rowHeaders = c("Mean birds/km^2", "SD of birds/km^2")) %>%
          hot_cols(colWidths = 90, 
                   renderer = "function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (value.length == 0) {
              td.style.background = 'pink';
             }
           }") %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
        
      })
      
    })
  })
  

  
  
  #' -------------------------------------------------------------
  #  ----     Storing and restoring input values              ----
  #' -------------------------------------------------------------
  #'
  #' NOTE: data is stored locally in the user's browser
  
  # --- Storing currently selected input values when user clicks store
  observeEvent(input$saveInputs_btt, ignoreInit = TRUE, {
    
    # species
    updateStore(session, "selectSpecs", input$selectSpecs)
    
    # windfarm and turbine parameters - non-tabulated
    str_subset(names(input), pattern = "(?<!hotInput_)turbinePars|windfarmPars|windSpeed") %>%
      walk(function(x){
        updateStore(session, x, input[[x]])
      })
      

    # Species-specific parameters - non-tabulated
    str_subset(names(input), pattern = "biomPars|slctInput_userOpts_") %>%
      walk(function(x){
        updateStore(session, x, input[[x]])
      })

    
    # All rHandsontables (i.e. monthly operations, pitch/bladeSpeed vs WindSpeed & monthly densities)
    str_subset(names(input), pattern = "hotInput_") %>%
      walk(function(x){
        
        #browser()
        
        input[[x]]$data %>%   # rHandsontables input objects return data in a list, on a by-row conversion
          flatten %>%
          map(function(z){ ifelse(is.null(z), "", z)}) %>%  # required to solve issue with NULLs generated when storing from (restored) empty table cells, which would cause errors
          unlist %>%
          updateStore(session, x, .)

        #updateStore(session, x, input[[x]]$data)
      })
    
    
    # show modal confirming inputs have been stored
    sendSweetAlert(session = session, title = "Parameters stored!", 
                   text = "Current parameter values stored locally", 
                   type = "success")
  })
  
  
  

  # --- Restoring latest saved input values - part 1: selected species, winfarm and turbine parameters
  #
  observeEvent(input$restoreInputs_btt, {
    
    #browser()
    
    if(length(input$store)>0){
    
    # ---- Selected species ---- #
    updateSelectizeInput(session, inputId = "selectSpecs", selected = input$store$selectSpecs, 
                         choices = unique(c(species, input$store$selectSpecs)))
    
     
    # -----  Windfarm and turbine parameters ---- #
    # - numeric inputs
    str_subset(names(input), pattern = "numInput_turbinePars|numInput_windfarmPars|numInput_miscPars_windSpeed") %>%
      walk(function(x){
        updateNumericInput(session, inputId = x, value = input$store[[x]])
      })
    
    # - knob inputs
    updateKnobInput(session, inputId = "sldInput_windfarmPars_upWindDownWindProp", 
                    value = input$store$sldInput_windfarmPars_upWindDownWindProp)
    
    # - Handsontable inputs
    rv$turbinePars_monthOps_df[1, ] <- as.numeric(input$store$hotInput_turbinePars_monthOps[1:12]) 
    rv$turbinePars_monthOps_df[2, ] <- as.numeric(input$store$hotInput_turbinePars_monthOps[13:24])
    rv$turbinePars_monthOps_df[3, ] <- as.numeric(input$store$hotInput_turbinePars_monthOps[25:36])
    
    rv$rotationVsWind_df <- input$store$hotInput_turbinePars_rotationVsWind %>%  
      #unlist %>% 
      matrix(ncol=2, byrow = TRUE) %>%
      as.tibble() %>%
      transmute(windSpeed = as.numeric(V1), rotationSpeed = as.numeric(V2)) %>%
      filter(!(is.na(windSpeed) & is.na(rotationSpeed)))
    
    
    rv$pitchVsWind_df <- input$store$hotInput_turbinePars_pitchVsWind %>%
      #unlist %>% 
      matrix(ncol=2, byrow = TRUE) %>%
      as.tibble() %>%
      transmute(windSpeed = as.numeric(V1), bladePitch = as.numeric(V2)) %>%
      filter(!(is.na(windSpeed) & is.na(bladePitch)))
    
    # - radio buttons inputs
    updateRadioGroupButtons(session, inputId = "radGrpInput_turbinePars_rotationAndPitchOption", 
                            selected = input$store$radGrpInput_turbinePars_rotationAndPitchOption)
    
    
    # start logic and chain events for the species specific parameters
    # when restoring for the same previously stored species, force an input update to trigger the next module to perform the restoring
    # This step is necessary to generate the required parameter names, as update* functions only update the inputs 
    # after the remaining dependent reactive modules finish running
    if(identical(input$selectSpecs, input$store$selectSpecs)){
      defaultSpecLabel <- str_replace_all(string = defaultSpecies, pattern = "\\s|-", replacement = "_")
      updateNumericInput(session, inputId = paste0("numInput_biomPars_bodyLt_E_", defaultSpecLabel), value = NA)
    }

    restoringStage <<- TRUE  # set the restoring status, crucial for the next chained module

    }else{
      # show modal alerting that there are no input values stored
      sendSweetAlert(session = session, title = "Oops!", 
                     text = "Sorry, no parameter values currently stored",
                     type = "error")
    }
    
  })
  
  
  # Restoring latest saved input values - part 2: species specific parameters. 
  #
  # Defined as a chain reaction of the module above. Gets triggered when the rv with species parameter 
  # is invalidated, which occurs when the parameter names for new species are generated or, if restoring for
  # the same species, when the forced update above is performed
  observeEvent(rv$biomParsInputs_ls, {
  
    if(restoringStage){
      
      #browser()
      
      storedSlctSpecLabel <- str_replace_all(input$store$selectSpecs, pattern = "\\s|-", replacement = "_") 
      
      for(spec in storedSlctSpecLabel){
      
        # numeric inputs
        names(input$store) %>% str_subset(pattern = paste0("numInput_biomPars_.+", spec)) %>%
          walk(function(x){
            updateNumericInput(session, inputId = x, value = input$store[[x]])
          })
        
        # radio buttons inputs
        names(input$store) %>% str_subset(pattern = paste0("slctInput_.+", spec)) %>%
          walk(function(x){
            updateRadioGroupButtons(session, inputId = x, selected = input$store[[x]])  
          })
          
        #str_view_all(names(input$store), pattern = paste0("slctInput_biomPars_.+", spec))
        
        # monthly densities parameters
        cSpecStoredMonthDens_DFName <- names(input$store) %>% str_subset(pattern = paste0("hotInput_.+", spec))
        
        if(!is_empty(cSpecStoredMonthDens_DFName)){
          
          rv$startSpecMonthDens_df[[spec]] <- input$store[[cSpecStoredMonthDens_DFName]] %>%
            #unlist() %>%
            matrix(nrow = 2, byrow = TRUE) %>%
            t() %>%
            as.tibble() %>%
            transmute(meanDensity = as.numeric(V1), sdDensity = as.numeric(V2))
        }
      }
      
      # Show modal confirming stored input values have been restored
      sendSweetAlert(session = session, title = "Parameters restored!", 
                     text = "Previously stored parameter values have been restored",
                     type = "success")
      
      restoringStage <<- FALSE
    }
      
  })
    
  
 
  
  
  #' -----------------------------------------------------------------
  #  ----          Data Templates Management                      ----
  #' -----------------------------------------------------------------
  
  
  # --- Manages downloads of data templates
  observeEvent(rv$addedSpec, {
    
    walk(rv$addedSpec, function(x){
      
      specLabel <- slctSpeciesTags() %>% filter(species == x) %>% pull(specLabel) #gsub(" ", "_", x)
      
      # FHD boostrap data
      downlTag_FHD <- paste0("dwnld_template_FHD_", specLabel)
      
      output[[downlTag_FHD]] <- downloadHandler(
        filename = function() {
          "FHD_bootstrapData_template.csv"
        },
        content = function(file) {
          write.csv(template_FHD, file, row.names = FALSE)
        }
      )
      
      
      # Monthly bird densities reference points stats
      downlTag_monthDens_summ <- paste0("dwnld_template_monthDens_summaries_", specLabel)
      
      output[[downlTag_monthDens_summ]] <- downloadHandler(
        filename = function() {
          "monthDensities_distRefPoints_template.csv"
        },
        content = function(file) {
          write.csv(template_monthDens_summaries, file, row.names = FALSE)
        }
      )
      
      
      # Monthly bird densities 
      downlTag_monthDens_samp <- paste0("dwnld_template_monthDens_samples_", specLabel)
      
      output[[downlTag_monthDens_samp]] <- downloadHandler(
        filename = function() {
          "monthDensities_distSamples_template.csv"
        },
        content = function(file) {
          write.csv(template_monthDens_samples, file, row.names = FALSE)
        }
      )
    })
  })
  
  
  

  
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
             par_hyper = paste(par, hyper, sep = "_"), 
             parName = case_when(
               par == "bodyLt" ~ "Body length (m)",
               par == "wngSpan" ~ "Wing Span (m)",
               par == "flSpeed" ~ "Flight Speed (m/s)",
               par == "noctAct" ~ "Nocturnal Activity (proportion)",
               par == "basicAvoid" ~ "Basic avoidance (probability)",
               par == "extAvoid" ~ "Extended avoidance (probability)",
               par == "CRHeight" ~ "CRH (proportion)"
             ))

    inputs_currBiomParsPlottable <- inputs_currBiomPars_df %>%
      filter(hyper %in% c("E", "SD")) %>%
      select(-inputTags_split) %>%
      mutate(plotTag = paste0("plot_biomPars_", par, "_", specLabel),
             qtTag =  paste0("qtls_biomPars_", par, "_", specLabel),
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
            c_qtTag <- unique(cPlotData$qtTag)
            c_par <- unique(cPlotData$par)

            mu <- as.numeric(as.character(filter(cPlotData, hyper == "E")$Value))
            stdev <- as.numeric(as.character(filter(cPlotData, hyper == "SD")$Value))

            output[[c_tag]] <- renderPlot({
              #print(c_tag)
              if(c_par %in% c("bodyLt", "wngSpan", "flSpeed")){
                truncNormPars_densPlots(mu = mu, stdev = stdev, lower = 0, fill="darkorange", xlab = unique(cPlotData$parName))
              }else{
                if(c_par %in% c("noctAct", "basicAvoid", "extAvoid", "CRHeight")){
                  betaInputPars_densPlots(p = mu, stdev = stdev, fill="darkorange", xlab = unique(cPlotData$parName))
                }else{
                  normDens_ParsPlots(mu = mu, stdev = stdev, fill="darkorange", xlab = unique(cPlotData$parName), refValue_SD = stdev*1.5)  
                }
              }
            })
            
            output[[c_qtTag]] <- renderPrint({
              if(c_par %in% c("bodyLt", "wngSpan", "flSpeed")){
                truncNormPars_qtlTbl(mu = mu, stdev = stdev, lower = 0, varTag = c_par, decPlaces = 4) 
              }else{
                if(c_par %in% c("noctAct", "basicAvoid", "extAvoid", "CRHeight")){
                  betaInputPars_qtlTbl(p = mu, stdev = stdev, varTag = c_par, decPlaces = 4)
                }else{
                  normDens_ParsQtls(mu = mu, stdev = stdev, varTag = c_par, decPlaces = 4) 
                }
              }
            })
          })
        }
        # update values of currently plotted biometric parameters
        inputs_BiomParsPlotted <<- inputs_currBiomParsPlottable
      }
  })

  
  
  # --- Plots of the bootstrap samples of FHD uploaded by the user
  observeEvent(flgtHghDstInputs_ls(), {
    
    req(length(flgtHghDstInputs_ls())>0)
    
    walk2(flgtHghDstInputs_ls(), names(flgtHghDstInputs_ls()), function(x, y){

      specLabel <- map_chr(str_split(y, "_"), function(x) paste(x[-c(1:3)], collapse = "_"))
      plotTagBoot_user <- paste0("plot_UserFHD_allBoots_", specLabel)
      plotTagBootQts_user <- paste0("plot_UserFHD_QtsBoot_", specLabel)
      
      
      # data prep: rename height column and drop all 0s columns
      x %<>% rename(Height = Height_m) %>%
        select_if(colSums(., na.rm = TRUE) > 0)
      
  
      output[[plotTagBoot_user]] <-  renderD3heatmap({
        x %>%
          select(1:min(100, ncol(.))) %>%
          slice(1:50) %>%
          arrange(desc(Height)) %>%
          select(-Height) %>%
          d3heatmap(colors = manual2_pal_cont(40), dendrogram = 'none', labRow = paste0(nrow(.):1, " m"),
                    xaxis_font_size= "5pt", labCol = paste0("bootID ", 1:ncol(.)))
        
      })
      
      output[[plotTagBootQts_user]] <- renderPlot({
        x %>%
          gather(bootId, Prop, -Height) %>%
          group_by(Height) %>%
          filter(!is.na(Prop)) %>%
          summarise(
            perc2.5 = quantile(Prop, probs = 0.025),
            perc50 = quantile(Prop, probs = 0.5),
            perc97.5 = quantile(Prop, probs = 0.975)) %>%
          #filter(perc97.5 > 0.00005) %>%
          ggplot(aes(y = perc50, x = Height)) +
          geom_pointrange(aes(ymin = perc2.5, ymax = perc97.5), col = "darkorange", size = 0.3) +
          #geom_line(size = 0.8) +
          labs(y = "Proportion", x = "Height (m)")
      })

    })
  })
  
  
  # --- generates plots to vizualize default bootstrap data of FHD
  observeEvent(rv$addedSpec, {

    req(rv$addedSpec)
    
    walk(rv$addedSpec, function(x){
      
      specLabel <- slctSpeciesTags() %>% filter(species == x) %>% pull(specLabel) #gsub(" ", "_", x)
      
      plotTagBoot <- paste0("plot_defaultFHD_allBoots_", specLabel)
      plotTagBootQts <- paste0("plot_defaultFHD_QtsBoot_", specLabel)
      
      addedSpec_defFHDBootDt <- fread_possibly(paste0("data/", specLabel, "_ht_dflt.csv"))

      output[[plotTagBoot]] <- renderD3heatmap({
        
        validate(
          need(!is.null(addedSpec_defFHDBootDt),
               paste0("Warning: Default FHD data for ", x, " not available. ",
                      "Select 'Other' to upload data. ",
                      "Missing data will lead to invalid results for model Options 2 & 3"
               )),
          errorClass = "valErrorMsgClass_2"
        )
        
        addedSpec_defFHDBootDt %>%
          select(1:100) %>%
          slice(1:50) %>%
          mutate(Height = 1:nrow(.)) %>%
          arrange(desc(Height)) %>%
          select(-Height) %>%
          d3heatmap(colors = manual2_pal_cont(40), dendrogram = 'none', labRow = paste0(nrow(.):1, " m"),
                    xaxis_font_size= "5pt", labCol = paste0("bootID ", 1:ncol(.)))
        
      })
      
      
      output[[plotTagBootQts]] <- renderPlot({
        
        validate(
          need(!is.null(addedSpec_defFHDBootDt),
               paste0("Warning: Default FHD data for ", x, " not available. ",
                      "Select 'Other' to upload data. ",
                      "Missing data will lead to invalid results for model Options 2 & 3"
               )),
          errorClass = "valErrorMsgClass_2"
        )
        
        addedSpec_defFHDBootDt %>%
          mutate(Height = 1:nrow(.)) %>%
          gather(bootId, Prop, -Height) %>%
          group_by(Height) %>%
          filter(!is.na(Prop)) %>%
          summarise(
            perc2.5 = quantile(Prop, probs = 0.025),
            perc50 = quantile(Prop, probs = 0.5),
            perc97.5 = quantile(Prop, probs = 0.975)) %>%
          filter(perc97.5 > 0.000001) %>%
          ggplot(aes(y = perc50, x = Height)) +
          geom_pointrange(aes(ymin = perc2.5, ymax = perc97.5), col = "darkorange", size = 0.3) +
          #geom_line(size = 0.8) +
          labs(y = "Proportion", x = "Height (m)")
        
      })
      
    })
  })
  
  

  ## --- Generates plots for Turbine parameters
  observe({
    mu <- input$numInput_turbinePars_rotnSpeed_E_
    stdev <- input$numInput_turbinePars_rotnSpeed_SD_

    output$plot_turbinePars_rotnSpeed <- renderPlot({
      
      #sdRef <- ifelse(startUpValues$rotnSpeed_SD/stdev < 0.25, startUpValues$rotnSpeed_SD*10, startUpValues$rotnSpeed_SD)
      truncNormPars_densPlots(mu = mu, stdev = stdev, lower = 0, fill = "olivedrab", xlab = "Rotation Speed (rpm)")
    })

    output$qtls_turbinePars_rotnSpeed <- renderPrint({
      truncNormPars_qtlTbl(mu = mu, stdev = stdev, lower = 0, varTag = "RotnSpeed")
    })
  })

  
  observe({
    mu <- input$numInput_turbinePars_bladePitch_E_
    stdev <- input$numInput_turbinePars_bladePitch_SD_
    
    output$plot_turbinePars_bladePitch <- renderPlot({
      #sdRef <- ifelse(startUpValues$bladePitch_SD/stdev < 0.25, startUpValues$bladePitch_SD*10, startUpValues$bladePitch_SD)
      truncNormPars_densPlots(mu = mu, stdev = stdev, lower = 0, fill = "olivedrab", xlab = "Blade Pitch (degrees)")
    })
    
    output$qtls_turbinePars_bladePitch <- renderPrint({
      truncNormPars_qtlTbl(mu = mu, stdev = stdev, lower = 0, varTag = "bladePitch")
    })
  }) 
  
  
  
  observe({
    mu <- input$numInput_miscPars_windSpeed_E_
    stdev <- input$numInput_miscPars_windSpeed_SD_
    
    output$plot_miscPars_windSpeed <- renderPlot({
      sdRef <- ifelse(startUpValues$turbinePars$windSpeed_SD/stdev < 0.25, startUpValues$turbinePars$windSpeed_SD*5, 
                      startUpValues$turbinePars$windSpeed_SD)
      truncNormPars_densPlots(mu = mu, stdev = stdev, lower = 0, fill = "olivedrab", xlab = "Wind Speed (m/s)")
    })
    
    output$qtls_miscPars_windSpeed <- renderPrint({
      truncNormPars_qtlTbl(mu = mu, stdev = stdev, lower = 0, varTag = "WindSpeed")
    })
    
  })
  
  
  # -- Turbine Monthly Operations - wind availability
  output$plot_turbinePars_monthOps_windAvb <- renderPlot({
    
    req(input$hotInput_turbinePars_monthOps)
    
    hot_to_r(input$hotInput_turbinePars_monthOps) %>% rownames_to_column(var="Variable") %>% slice(1) %>%
      gather(month, windAvb, -Variable) %>% select(-Variable) %>%
      mutate(month = factor(month.abb, levels = month.abb)) %>%
      ggplot(aes(x=month, y=windAvb)) +
      geom_col(fill= "olivedrab", alpha = 0.7, width = 0.4) +
      labs(x="", y = "Wind Availability (%)", title = "Monthly wind availability")
    
  })

  
  # -- Turbine Monthly Operations - Downtime
  output$plot_turbinePars_monthOps_downtime <- renderPlot({
    
    req(input$hotInput_turbinePars_monthOps)
    
    #browser()
  
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

  
  
  ### -- Generate plots for random samples of monthly densities
  observeEvent(monthDens_userDt_sample_ls(), {
    
    req(length(monthDens_userDt_sample_ls())>0)
    
    walk2(monthDens_userDt_sample_ls(), names(monthDens_userDt_sample_ls()), function(x, y){
      
      #browser()
  
      specLabel <- str_extract(y, "(?<=upldInput_monthDens_userDt_samples_).*")
      specName <- str_replace_all(string = specLabel, "_", " ")
      plotTag<- paste0("plot_inputMonthDens_samples_", specLabel)
      plotTagQtlsBars <- paste0("plot_inputMonthDens_QtlsBars_samples_", specLabel)
      
      output[[plotTag]] <- renderPlot({
        
        validate(
          need(ncol(x) == 12 & nrow(x) >= 1000,
               paste0("Warning: the dimension of the uploaded data with samples of monthly densities of ", specName, " does not conform with size requirements",
                      " (12 columns by a minimum of 1000 rows)."
               )),
          errorClass = "valErrorMsgClass"
        )
        
        x %>%
          gather(month, density)  %>%
          mutate(month = factor(month, levels = unique(month))) %>%
          ggplot(aes(x = density)) +
          geom_histogram(fill="darkorange", col = "black", bins = 50) +
          labs(x = "Number of birds per km2", y = "Frequency", title = paste0("Uploaded draws from the distribution of monthly densities of ", specName)) +
          facet_wrap(~month, ncol = 6)
          #coord_flip() +
          #facet_grid(~month)
      })
      
      
      output[[plotTagQtlsBars]] <- renderPlot({
        
        if(ncol(x) == 12 & nrow(x) >= 1000){
          x %>%
            gather(month, density)  %>%
            mutate(month = factor(month, levels = unique(month))) %>%
            group_by(month) %>%
            summarise(
              lowBound = quantile(density, 0.025), 
              median = quantile(density, 0.5), 
              uppBound = quantile(density, 0.975)
            ) %>%
            ggplot(aes(x = month, y = median)) +
            geom_errorbar(aes(ymin = lowBound, ymax = uppBound), col = "darkorange", width = 0.2, size = 1) +
            geom_point(col="darkorange", size = 2) +
            labs(y = "Number of birds per km2", x = "", title = paste0(specName, " - Median and 95% PI from uploaded data"))
        }else{
          NULL
        }
      })
    })
  })

  
  ### -- Generate plots for reference points of monthly densities
  observeEvent(monthDens_userDt_summs_ls(), {
    
    req(length(monthDens_userDt_summs_ls())>0)
    
    walk2(monthDens_userDt_summs_ls(), names(monthDens_userDt_summs_ls()), function(x, y){
      
      #browser()
      
      specLabel <- str_extract(y, "(?<=upldInput_monthDens_userDt_summaries_).*")
      specName <- str_replace_all(string = specLabel, "_", " ")
      plotTag<- paste0("plot_inputMonthDens_summaries_", specLabel)
      plotTagQtlsBars<- paste0("plot_inputMonthDens_QtlsBars_summaries_", specLabel)
    
      output[[plotTag]] <- renderPlot({
        
        validate(
          need(ncol(x) == 13 & nrow(x) >= 5,
               paste0("Error: the dimension of the uploaded data with reference points for monthly densities of ", specName, " does not conform with size requirements",
                      " (expects 13 columns and a minimum of 5 rows.)"
               )),
          errorClass = "valErrorMsgClass"
        )
        
        dt <- x %>% 
          mutate(
            referencePointsNum = case_when(
              str_detect(referencePoints, "(M|m)in(|imum)") ~ 0.000001,
              str_detect(referencePoints, "(M|m)ax(|imum)") ~ 99.99999,
              str_detect(referencePoints, "th") ~ as.numeric(str_extract(referencePoints, pattern = "\\d+\\.?\\d*"))
            ),
            referenceProbs = referencePointsNum/100) %>% 
          select(-c(referencePoints, referencePointsNum)) %>% 
          gather(month, birdDensities, -referenceProbs) %>%
          mutate(month = factor(month, levels = unique(month)))
        
        dt_nest <- dt %>%
          group_by(month) %>%
          nest() %>%
          mutate(cdf = map(data, function(x){
            x_intPoints <- runif(2500, min(x$referenceProbs), max(x$referenceProbs))
            y_intPoints <- interp1(x$referenceProbs, x$birdDensities, xi = x_intPoints, method = "cubic")

            data.frame(prob = x_intPoints, birdDensity = y_intPoints) %>%
              arrange(prob)
          }))

        dt_nest %>% select(-data) %>%
          unnest() %>%
          ggplot(aes(y = prob, x = birdDensity)) +
          geom_path(col = "darkorange") +
          geom_point(aes(x = birdDensities, y = referenceProbs), data = dt, col = "darkorange", size = 2) +
          facet_grid(~month) +
          labs(x = "Number of birds per km2", y = "Frequency", title = "Empirical cumulative probability based on uploaded data")
      })
      
      
      output[[plotTagQtlsBars]] <- renderPlot({
        
        if(ncol(x) == 13 & nrow(x) >= 5){
          x %>% 
            mutate(
              referencePointsNum = case_when(
                str_detect(referencePoints, "(M|m)in(|imum)") ~ 0.000001,
                str_detect(referencePoints, "(M|m)ax(|imum)") ~ 99.99999,
                str_detect(referencePoints, "th") ~ as.numeric(str_extract(referencePoints, pattern = "\\d+\\.?\\d*"))
              ),
              referenceProbs = referencePointsNum/100) %>% 
            select(-c(referencePoints, referencePointsNum)) %>% 
            gather(month, birdDensities, -referenceProbs) %>%
            mutate(month = factor(month, levels = unique(month))) %>%
            filter(referenceProbs %in% c(0.025, 0.5, 0.975)) %>%
            spread(key = referenceProbs, value = birdDensities) %>%
            rename(lowBound = "0.025", med = "0.5", uppBound = "0.975") %>%
            ggplot(aes(x = month, y = med)) +
            geom_errorbar(aes(ymin = lowBound, ymax = uppBound), col = "darkorange", width = 0.2, size = 1) +
            geom_point(col="darkorange", size = 2) +
            labs(y = "Number of birds per km2", x = "", title = paste0(specName, " - Median and 95% interval from updloaded data"))
          
        }else{
          NULL
        }
      })
    })
  })  
  
  
  
  
  
  observeEvent(rv$birdDensParsInputs_ls, {
    
    req(length(rv$birdDensParsInputs_ls)>0)
    
    tabChanged_ls <- sapply(names(rv$birdDensParsInputs_ls), function(x){
      !identical(rv$birdDensParsInputs_ls[[x]], birdDensPars_plotted[[x]])
    })

    birdDensPars_toPlot <- rv$birdDensParsInputs_ls[c(which(tabChanged_ls == TRUE))]

    walk2(birdDensPars_toPlot, names(birdDensPars_toPlot), function(x, y){
      
      specLabel <- str_extract(y, "(?<=hotInput_birdDensPars_tnorm_).+")
      specName <- gsub("_", " ", specLabel)
      plotTag <- paste0("plot_birdDensPars_", specLabel)
  
      # print(plotTag)
      
      output[[plotTag]] <- renderPlot({

        as.data.frame(t(x)) %>% 
          rownames_to_column(var = "month") %>%
          rename(meanDensity = V1, sdDensity = V2) %>%
          mutate(month = factor(month, levels = month)) %>%
          group_by(month) %>%
          mutate(med = qtnorm_dmp(p=0.5, mean = meanDensity, sd = sdDensity, lower = 0),
                 lwBound = qtnorm_dmp(p=0.025, mean = meanDensity, sd = sdDensity, lower = 0),   
                 upBound = qtnorm_dmp(p=0.975, mean = meanDensity, sd = sdDensity, lower = 0)) %>%
          ggplot(aes(x=month, y = med, group=month)) +
          geom_errorbar(aes(ymin=lwBound, ymax=upBound), col = "darkorange", width = 0.2, size = 1) +
          geom_point(col="darkorange", size = 3) +
          labs(y = "Number of birds per km2", x = "", title = paste0(specName, " (Median and 95% CI)"))
      })
    })
    
    birdDensPars_plotted <<- rv$birdDensParsInputs_ls # birdDensPars_toPlot 
  })
  

  
  
  
  
  #' --------------------------------------------------------------------------------
  #  ----        On-the-fly inputs Validation System with feedback to UI         ----
  #' --------------------------------------------------------------------------------
  
  observe({
    source("inputParsValidations.r", local = TRUE)
  })


  
  
  #' --------------------------------------------------------------------------------------------------
  #  ---- Prepare & check input data for simulation, triggered when user pushes go        ----
  #' --------------------------------------------------------------------------------------------------

  
  rv$birdata_model <- data.frame()
  rv$monthDensData_model <- data.frame()
  rv$monthDensData_model <- data.frame()
  rv$monthDensOpt_model <- data.frame()
  rv$turbineData_model <- data.frame()
  rv$windfarmData_model <- data.frame()
  rv$simCodeTrigger <- 0
  
  rv$densityDataPresent <- NULL
  rv$NAsFreeData <- NULL
  rv$FHD_acceptable <- NULL
  
  
  
  observeEvent(input$actButtonInput_simulPars_GO, {

    missingValues <- list()
    

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # -------  bird biometric data

    # -- gather data
    birdBiomData <- rv$biomParsInputs_ls %>%
      ldply(function(x){data.frame(Value = as.character(x))}, .id = "inputTags") %>%
      mutate(inputTags_split = str_split(inputTags, "_")) %>%
      mutate(specLabel = map_chr(inputTags_split, function(x) paste(x[-c(1:4)], collapse = "_")),
             par = map_chr(inputTags_split, function(x) x[3]),
             hyper = map_chr(inputTags_split, function(x) x[4]),
             par_hyper = paste(par, hyper, sep = "_")) %>%
      select(-c(inputTags, inputTags_split, par, hyper))


    # -- Check for NAs and save affected parameters
    missingValues[["birdBiom"]] <- birdBiomData %>%
      filter(is.na(Value)) %>%
      arrange(specLabel) %>%
      select(-Value) %>%
      mutate(specName = str_replace_all(specLabel, "_", " "))


    # Data manipulation for model
    rv$birdata_model <- birdBiomData %>%
      spread(par_hyper, Value) %>%
      rename(Species = specLabel, AvoidanceBasic = basicAvoid_E, AvoidanceBasicSD = basicAvoid_SD,
             AvoidanceExtended = extAvoid_E, AvoidanceExtendedSD = extAvoid_SD, Body_Length = bodyLt_E, Body_LengthSD = bodyLt_SD,
             Wingspan = wngSpan_E,  WingspanSD = wngSpan_SD, Flight_Speed  = flSpeed_E, Flight_SpeedSD = flSpeed_SD,
             Nocturnal_Activity = noctAct_E, Nocturnal_ActivitySD = noctAct_SD, Flight = flType_tp,
             Prop_CRH_Obs = CRHeight_E, Prop_CRH_ObsSD = CRHeight_SD) %>%
      select(Species, AvoidanceBasic, AvoidanceBasicSD, AvoidanceExtended, AvoidanceExtendedSD, Body_Length, Body_LengthSD,
             Wingspan, WingspanSD, Flight_Speed, Flight_SpeedSD, Nocturnal_Activity, Nocturnal_ActivitySD, Flight,
             Prop_CRH_Obs, Prop_CRH_ObsSD)



    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # -- birds FHD
    #
    # FHD data source chosen for each species
    RVs_inputs_ls <- reactiveValuesToList(input)
    FHD_UserOptions_ls <- RVs_inputs_ls[grep("userOpts_FHD_dtSrc", names(RVs_inputs_ls))]

    # Check for missing FHD data for each selected species
    FHD_dataStatus <- FHD_UserOptions_ls %>%
      map2_dfr(., names(.), function(x, y){
        
        specLabel <- map_chr(str_split(y, "_"), ~ paste(.[-c(1:4)], collapse = "_"))
        
        if(x == "default"){
          tibble(specLabel = specLabel) %>%
            mutate(missingFHD = ifelse(file.exists(paste0("data/", specLabel, "_ht_dflt.csv")), FALSE, TRUE))
      
        }else{
          if(x == "other"){
            cUserFHD_LsIndice <- str_which(safe_names(flgtHghDstInputs_ls()), specLabel)
            tibble(specLabel = specLabel) %>%
              mutate(missingFHD = ifelse(length(cUserFHD_LsIndice) > 0, FALSE, TRUE))
          }
        }
      })
    
    
    # Logic to deal with missing FHD - if missing for any species, invalidade simulation and launch confirmation window 
    # (while also preparing data accordingly)
    FHD_dataStatus %>%
      split(.$missingFHD) %>%
      walk(function(x){
        if(unique(x$missingFHD) == TRUE){
          
          x$specLabel %>% 
            walk(~fwrite(template_FHD, file=paste0("data/", ., "_ht.csv"), row.names = FALSE))
          
          confirmSweetAlert(
            session = session,
            inputId = "confirmModal_missingFHD",
            title = "FHD data not found",
            text = div(
              style = "font-size: 15px; text-align: left",
              hr(),
              p("Missing flight height distribution data for:"),
              tags$ul(
                map(x$specLabel, function(y){
                  tags$li(str_replace_all(y, "_", " "))
                })
              ), 
              #br(),
              p("Simulation results for model Option 2 and Option 3 will be invalid. Option 1 results not affected by missing FHD data."),
              p(tags$b("Do you wish to proceed with the simulation?"),  style = "text-align: center")
            ),
            type = "warning",
            danger_mode = TRUE,
            btn_labels = c("No, Go Back", "Yes, Proceed"),
            html = TRUE
          )
          
          rv$FHD_acceptable <- FALSE
        }else{
          
          x$specLabel %>% 
            walk(~file.copy(from = paste0("data/", ., "_ht_dflt.csv"),to = paste0("data/", ., "_ht.csv"), 
                    overwrite = TRUE))
          
          rv$FHD_acceptable <- TRUE
        }
      })
    
     

    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # # --------- bird density data

    # --- get user options for each species
    if(length(rv$mthDens_userOptions)>0){

      rv$monthDensOpt_model <- rv$mthDens_userOptions %>%
        map2_df(., names(.), function(x,y){
          tibble(userOptionTag = as.character(y), userOption = as.character(x))
        }) %>%
        mutate(
          specLabel = str_replace(userOptionTag, "slctInput_userOpts_monthDens_sampler_", replacement = ""),
          specName = str_replace_all(specLabel, "_", " ")
        )

      # -- check if there is missing data for any of the options
      densDataMissing <- rv$monthDensOpt_model %>%
        split(.$specLabel) %>%
        map_dfr(function(x){
          if(x$userOption == "truncNorm"){
            mutate(x, densDataMissing = ifelse(any(str_detect(safe_names(rv$birdDensParsInputs_ls), pattern = specLabel)), FALSE, TRUE))
          }else{
            if(x$userOption == "pcntiles"){
              mutate(x, densDataMissing = ifelse(any(str_detect(safe_names(monthDens_userDt_summs_ls()), pattern = specLabel)), FALSE, TRUE))
            }else{
              if(x$userOption == "reSamp"){
                mutate(x, densDataMissing = ifelse(any(str_detect(safe_names(monthDens_userDt_sample_ls()), pattern = specLabel)), FALSE, TRUE))
              }
            }
          }
        }) %>%
        filter(densDataMissing == TRUE)


      # --- Launch error message and block simulation if any dens data is missing; else, prepare data for model
      if(nrow(densDataMissing) > 0){

        # flagging absence of data
        rv$densityDataPresent <- FALSE

        # Modal describing error
        sendSweetAlert(
          session = session,
          title = "Bird density data not found",
          text = span(
            style = "font-size: 15px; text-align: left",
            hr(),
            p("Missing density data for the following species"),
            tags$ul(
              map2(densDataMissing$specName, densDataMissing$userOption, function(x,y){

                if(y == "truncNorm"){
                  tags$li(paste0(x, " - truncated normal parameters for monthly densities not found"))
                }else{
                  if(y == "pcntiles"){
                    tags$li(paste0(x, " - reference points of monthly densities not uploaded"))
                  }else{
                    if(y == "reSamp"){
                      tags$li(paste0(x, " - random samples of monthly densities not uploaded"))
                    }}}
                })
            ), 
            br(),
            tags$b("Please upload/fill-in missing data before proceeding to simulation", style = "text-align: center")
            ),
          type = "error",
          html = TRUE
        )

      }else{
        
        # flagging presence of data
        rv$densityDataPresent <- TRUE
        
        rv$monthDensData_model <- rv$monthDensOpt_model %>%
          group_by(userOption) %>%
          mutate(option = userOption) %>%
          nest(.key = "metadata") %>%
          mutate(data = map(metadata, function(x){

            #browser()

            if(unique(x$option) == "truncNorm"){

              countData <- rv$birdDensParsInputs_ls %>%
                map2_df(., names(.), function(x,y){
                  as.data.frame(x) %>%
                    rownames_to_column(var="hyperPar") %>%
                    add_column(., inputTags = y, .before = 1)
                }) %>%
                mutate(inputTags_split = str_split(inputTags, "_")) %>%
                mutate(Species = map_chr(inputTags_split, function(x) paste(x[-c(1:3)], collapse = "_"))) %>%
                select(Species, hyperPar:December) %>%
                gather(month, Value, -c(Species, hyperPar)) %>%
                group_by(Species) %>%
                mutate(VariableMasden = factor(paste0(rep(month.abb, each=2), c("", "SD"))),
                       VariableMasden = factor(VariableMasden, levels = VariableMasden)
                ) %>%
                select(-c(hyperPar:month)) %>%
                spread(VariableMasden, Value) %>%
                filter(Species %in% x$specLabel)

              out <- countData
            }


            if(unique(x$option) == "reSamp"){
              out <- rbindlist(monthDens_userDt_sample_ls(), idcol = TRUE) %>%
                mutate(specLabel = str_replace_all(.id, "upldInput_monthDens_userDt_samples_", "")) %>%
                select(specLabel, January:December)
            }

            if(unique(x$option) == "pcntiles"){

              out <- rbindlist(monthDens_userDt_summs_ls(), idcol = TRUE) %>%
                mutate(specLabel = str_replace_all(.id, "upldInput_monthDens_userDt_summaries_", "")) %>%
                select(specLabel, referencePoints:December) %>%
                mutate(
                  referencePointsNum = case_when(
                    str_detect(referencePoints, "(M|m)in(|imum)") ~ 0.000001,
                    str_detect(referencePoints, "(M|m)ax(|imum)") ~ 99.99999,
                    str_detect(referencePoints, "th") ~ as.numeric(str_extract(referencePoints, pattern = "\\d+\\.?\\d*"))
                  ),
                  referenceProbs = referencePointsNum/100) %>%
                select(-c(referencePoints, referencePointsNum))
            }
            
            return(out)
          }))

        
        #browser()
        
        # -- Check for NAs in truncated normal parameters and store affected parameters
        monthDensData_truncNorm <- rv$monthDensData_model %>% 
          filter(userOption == "truncNorm") %>% unnest()
        
        if(nrow(monthDensData_truncNorm) > 0){
          
          missingValues[["birdMonthDens"]] <- monthDensData_truncNorm %>%
            select(-c(userOption, userOptionTag, option, Species)) %>%
            gather(par_hyper, Value, -c(specLabel, specName)) %>%
            filter(is.na(Value)) %>%
            select(-Value)  
        }
      }
    }

    
    
    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # # --------- turbine data 
    
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
    rotSpeed_E <- ifelse(input$radGrpInput_turbinePars_rotationAndPitchOption == 'probDist',   
                         input$numInput_turbinePars_rotnSpeed_E_, NA)
    rotSpeed_SD <- ifelse(input$radGrpInput_turbinePars_rotationAndPitchOption == 'probDist', 
                          input$numInput_turbinePars_rotnSpeed_SD_, NA)
    
    pitch_E <- ifelse(input$radGrpInput_turbinePars_rotationAndPitchOption == 'probDist', 
                      input$numInput_turbinePars_bladePitch_E_, NA)
    
    pitch_SD <- ifelse(input$radGrpInput_turbinePars_rotationAndPitchOption == 'probDist', 
                       input$numInput_turbinePars_bladePitch_SD_, NA)
    
    
    # merging turbine data
    rv$turbineData_model <- tibble(
      TurbineModel = input$numInput_turbinePars_turbinePower,
      Blades = input$numInput_turbinePars_numBlades,
      RotorRadius	= input$numInput_turbinePars_rotRadius,
      RotorRadiusSD	= 0, #input$numInput_turbinePars_rotRadius_SD_,
      HubHeightAdd = input$numInput_turbinePars_airGap,
      HubHeightAddSD	= 0, #input$numInput_turbinePars_hubHght_SD_,
      BladeWidth	= input$numInput_turbinePars_maxBladeWdth,
      BladeWidthSD = 0, #input$numInput_turbinePars_maxBladeWdth_SD_,
      RotorSpeedAndPitch_SimOption = input$radGrpInput_turbinePars_rotationAndPitchOption,
      RotationSpeed = rotSpeed_E,
      RotationSpeedSD = rotSpeed_SD,
      Pitch = pitch_E,
      PitchSD = pitch_SD,
      windSpeedMean = input$numInput_miscPars_windSpeed_E_, 
      windSpeedSD = input$numInput_miscPars_windSpeed_SD_
    ) %>%
      left_join(., turbineData_Operation, by = "TurbineModel")
    
    
    # check and identify NAs in Turbine data - if wind-speed reltn chosen, NAs in rotation and pitch parameters need to be ignored
    if(input$radGrpInput_turbinePars_rotationAndPitchOption == "windSpeedReltn"){
      missingValues[["Turbine"]] <- rv$turbineData_model %>% 
        select(-c(RotationSpeed, RotationSpeedSD, Pitch, PitchSD)) %>%
        gather(par_hyper, Value, -c(TurbineModel)) %>%
        filter(is.na(Value)) %>%
        select(-c(TurbineModel, Value))
    }else{
      missingValues[["Turbine"]] <- rv$turbineData_model %>% 
        gather(par_hyper, Value, -c(TurbineModel)) %>%
        filter(is.na(Value)) %>%
        select(-c(TurbineModel, Value))
    }
      

    
    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # # --------- wind farm parameters
    
    rv$windfarmData_model <- tibble(
      targetPower_MW = input$numInput_windfarmPars_nTurbines * input$numInput_turbinePars_turbinePower,
      nTurbines = input$numInput_windfarmPars_nTurbines,
      latitude_deg = input$numInput_windfarmPars_Latitude,
      width_km = input$numInput_windfarmPars_width,
      TidalOffset_m = input$numInput_windfarmPars_tidalOffset,
      upwindFlights_prop = input$sldInput_windfarmPars_upWindDownWindProp
    )
    
    #browser()
    
    # check and identify NAs in windfarm pars
    missingValues[["windfarm"]] <- rv$windfarmData_model %>%
      mutate(dummy = 1) %>%
      gather(par_hyper, Value, -dummy) %>%
      filter(is.na(Value)) %>%
      select(-Value)
    
    
    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # # --------- Handling NAs in data

    # Resume missing values in all data
    missingValues %<>% bind_rows(.id = "dataType") %>%
      mutate(dataMainType = case_when(
        dataType %in% c("birdBiom", "birdMonthDens") ~ "speciesFeatures",
        dataType %in% c("Turbine") ~ "turbineFeatures",
        dataType %in% c("windfarm") ~ "turbineFeatures"
      ))

    if(nrow(missingValues) > 0){

      # flag presence of NAs in data
      rv$NAsFreeData <- FALSE

      # Modal describing error to the user
      sendSweetAlert(
        session = session,
        title = "Found missing values in inputs",
        text = span(
          style = "font-size: 15px; text-align: left",
          hr(),
          p("NAs assigned to inputs in the following sections"),
          tags$ul(
          missingValues %>%
            split(.$dataMainType) %>%
            map(function(x){
              if(unique(x$dataMainType) == "speciesFeatures"){
                tags$li(tags$u("Species features"),
                        tags$ul(
                          map(unique(x$specName), tags$li)
                        ), br())
              }else{
                if(unique(x$dataMainType) == "turbineFeatures"){
                  tags$li(tags$u("Turbine and/or wind farm features"))
                }
              }
            })
          ),
          br(),
          tags$b("Empty fields (highlighted in red) must be filled in before proceeding to simulation", style = "font-size: 15px; text-align: center")
        ),
        type = "error",
        html = TRUE
      )
      
      #browser()
      
    }else{
      # flag data is free off NAs
      rv$NAsFreeData <- TRUE
    }

    # Flick the trigger 
    rv$simCodeTrigger <- rv$simCodeTrigger + 1
  })
  

  
  ## -------------------------------------------------------------------------- ## 
  ## --- intermediary step to deal with user confirmation on missing FHD data -- ##
  
  observeEvent(input$confirmModal_missingFHD, {
    
    if(input$confirmModal_missingFHD == TRUE){
      rv$FHD_acceptable <- TRUE
    }else{
      rv$FHD_acceptable <- FALSE
    }
    
    rv$simCodeTrigger <- rv$simCodeTrigger + 1
  })
  
  
  
  
  
  #' -----------------------------------------------------------------------
  #  ----            Collision Risk Simulation Model                    ----
  #' -----------------------------------------------------------------------
  
  #observeEvent(input$actButtonInput_simulPars_GO, {
  observeEvent(rv$simCodeTrigger, ignoreInit = TRUE, {
    
    #browser()
    
    #--- step 1: safety check if the various types on input data are valid for proceeding to simulation ------ #
    req(rv$densityDataPresent, 
        rv$NAsFreeData,
        rv$FHD_acceptable)
  
    
    # --- step 2: house-cleanig - remove output files left from the previous run in the current session ------ #
    
    file.remove(list.files(path2ShinyOut_Inputs, full.names = TRUE))
    file.remove(list.files(path2ShinyOut_Outputs, full.names = TRUE))
    file.remove(list.files(path2Outputs_results, full.names = TRUE, recursive = TRUE))
    
    
    
    #--- step 3: write out data in files as required for the model  ----- # 

      rv$monthDensData_model %>%
        mutate(export = walk2(userOption, data, function(x, y){
          
          if(x == "truncNorm"){
            fwrite(y, "data/CountData.csv")
            fwrite(y, file.path(path2ShinyOut_Inputs, "birdDensityData_truncnorm.csv"))
          }
          
          if(x == "reSamp"){
            fwrite(y, "data/birdDensityData_samples.csv")
            fwrite(y, file.path(path2ShinyOut_Inputs, "birdDensityData_samples.csv"))
          }
          
          if(x == "pcntiles"){
            fwrite(y, "data/birdDensityData_refPoints.csv")
            fwrite(y, file.path(path2ShinyOut_Inputs, "birdDensityData_refPoints.csv"))
          }
          
        }))

    
    # --- rotor speed and pitch vs windspeed
    windPowerData <- left_join(hot_to_r(input$hotInput_turbinePars_rotationVsWind), # produces "Warning in asMethod(object) : NAs introduced by coercion"
                               hot_to_r(input$hotInput_turbinePars_pitchVsWind),    # produces "Warning in asMethod(object) : NAs introduced by coercion"
                               by = "windSpeed") %>%
      rename(Wind = windSpeed, Rotor = rotationSpeed, Pitch = bladePitch) %>%
      drop_na()

    
    # output$out_simFunctionArgs <- renderPrint({
    #   isolate(
    #     print(list(
    #       BirdData= birdData,
    #       TurbineData = turbineData,
    #       CountData = countData,
    #       iter = input$sldInput_simulPars_numIter,
    #       CRSpecies = slctSpeciesTags()$specLabel,
    #       TPower = input$numInput_windfarmPars_targetPower,
    #       WFWidth = input$numInput_windfarmPars_width,
    #       LargeArrayCorrection = ifelse(input$chkBoxInput_simulPars_largeArrarCorr==TRUE, "yes", "no"),
    #       rop_Upwind = input$sldInput_windfarmPars_upWindDownWindProp,
    #       Latitude = input$numInput_windfarmPars_Latitude,
    #       TideOff = input$numInput_windfarmPars_tidalOffset,
    #       windSpeedMean = input$numInput_miscPars_windSpeed_E_,
    #       windSpeedSD = input$numInput_miscPars_windSpeed_SD_,
    #       windPowerData = windPowerData
    #     ))
    #   )
    # })
    
    
    # --- simulation Options
    simOptions <- tibble(
      iterations = input$sldInput_simulPars_numIter, 
      largeArrayCorrection = ifelse(input$chkBoxInput_simulPars_largeArrarCorr==TRUE, "yes", "no")
    )

    
    #' model function expects data to be provided in csv files, so saving them out for now to avoid code inconsistencies 
    #' Should change model function to expect data.frames instead of files once it's final version is established
    fwrite(rv$birdata_model, "data/BirdData.csv")
    fwrite(rv$turbineData_model, "data/TurbineData.csv")
    fwrite(windPowerData, paste0("data/windpower_", input$numInput_turbinePars_turbinePower, ".csv"))
    
    
    #' save inputs to the user's downloadable folder - repeating the previous step (except the changes to the turbine data) makes 
    #' code a bit inefficient, but the previous step should be temporary
    fwrite(rv$birdata_model, file.path(path2ShinyOut_Inputs, "BirdData.csv"))
    rv$turbineData_model %>% select(-c(RotorRadiusSD, HubHeightAddSD, BladeWidthSD)) %>% rename(airGap = HubHeightAdd) %>%
      fwrite(., file = file.path(path2ShinyOut_Inputs, "TurbineData.csv"))
    fwrite(windPowerData, file.path(path2ShinyOut_Inputs, paste0("windpower_", input$numInput_turbinePars_turbinePower, ".csv")))
    fwrite(rv$windfarmData_model, file.path(path2ShinyOut_Inputs, "windfarmData.csv"))
    fwrite(simOptions, file.path(path2ShinyOut_Inputs, "simOptions.csv"))
    
    
    # ----- step 4: Set progress bar ----- #
    
    # Create a Progress objects for species and iterations within each species
    progress_Spec <- Progress$new()
    progress_Iter <- Progress$new()
    
    progress_Spec$set(message = "Processing ", value = 0)
    progress_Iter$set(message = "Simulating...", value = 0)  # "Going through iterations"
    
    on.exit({
      progress_Iter$close()
      progress_Spec$close()
      })
    

    #' callback functions to update progress on species.
    updateProgress_Spec <- function(value = NULL, detail = NULL) {
      progress_Spec$set(value = value, detail = detail)
    }
    
    
    #' callback functions to update progress on iterations. Each time updateProgress_Iter() is called, 
    #' it moves the bar 1/nth of the total distance.
    updateProgress_Iter <- function(value = NULL, detail = NULL) {
      progress_Iter$set(value = value, detail = detail)
    }
    
    
    # ----- step 5: run simulation function ----- # 
    
    if(1){
      rv$sCRM_output_ls <- stochasticBand(
        workingDirectory="sCRM/",
        results_folder = path2Outputs_results,
        BirdDataFile = "data/BirdData.csv",
        TurbineDataFile = "data/TurbineData.csv",
        CountDataFile = "data/CountData.csv",
        FlightDataFile = "data/FlightHeight.csv",
        iter = input$sldInput_simulPars_numIter, 
        CRSpecies = slctSpeciesTags()$specLabel, 
        TPower = rv$windfarmData_model$targetPower_MW, #rv$windfarmData_model$nTurbines*input$numInput_turbinePars_turbinePower,
        LargeArrayCorrection = ifelse(input$chkBoxInput_simulPars_largeArrarCorr==TRUE, "yes", "no"), 
        WFWidth = rv$windfarmData_model$width_km,
        Prop_Upwind = rv$windfarmData_model$upwindFlights_prop/100, # convert % (user input) to proportion (expected by model function)
        Latitude = rv$windfarmData_model$latitude_deg,
        TideOff = rv$windfarmData_model$TidalOffset_m,
        windSpeedMean = input$numInput_miscPars_windSpeed_E_, 
        windSpeedSD = input$numInput_miscPars_windSpeed_SD_,
        #windPowerData = windPowerData,
        updateProgress_Spec,  # pass in the updateProgress function so that it can update the progress indicator.
        updateProgress_Iter,
        DensityOpt = rv$monthDensOpt_model # pass in the user options for bird density data
      )
    }
  })

  
  
  
  #' -----------------------------------------------------------------------
  #  ----            Compute and display model outputs                  ----
  #' -------------------------------------------------------+----------------
  
  # Arrange results data into a data.frame
  sCRM_outputDF <- eventReactive(rv$sCRM_output_ls, {
    
    req(rv$sCRM_output_ls)

    listLevels <- expand.grid(option = names(rv$sCRM_output_ls), specLabel = names(rv$sCRM_output_ls[[1]]), 
                              turbineModel =  names(rv$sCRM_output_ls[[1]][[1]]))
    
    pmap(list(x=as.character(listLevels$option), y=as.character(listLevels$specLabel), z=as.character(listLevels$turbineModel)), 
         function(x, y, z) {
           data.frame(option = x, specLabel = y, turbineModel = z, rv$sCRM_output_ls[[x]][[y]][[z]]) %>%
             mutate(option = str_replace(option, "monthCollsnReps_opt", "Option "),
                    turbineModel = str_replace(turbineModel, "turbModel", ""), 
                    iter = 1:nrow(.))
         }
    ) %>%
      bind_rows()
  })
  
  
  
  
  observe({
    
    df <- sCRM_outputDF()
    
    req(nrow(df) > 0)
    
    # Set-up progress bar for the construction of plots and tables from model outputs
    progress_genModelOuts <- Progress$new()
    on.exit(progress_genModelOuts$close())
    progress_genModelOuts$set(message = "Generating outputs", value = 0)
    pbarNSteps <- 6
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~  boxplots and summary tables of collisons per month, for each option and species  ~~~ #
    
    df_monthlyColl <- df %>%
      gather(Month, Collisions, -c(option, specLabel, turbineModel, iter)) %>%
      mutate(Month = factor(Month, levels = unique(Month)),
             specLabel = as.character(specLabel)) %>%
      group_by(specLabel, option, turbineModel) %>%
      nest()
    
    # update pbar
    progress_genModelOuts$inc(1/pbarNSteps)
    
    # --- plots
    df_monthlyColl %>%
      mutate(plot = pmap(list(dt = data, spec = specLabel, opt = option), function(dt, spec, opt){

        dt %<>% mutate(opt = opt)
        plotTag <- paste0(spec, "_plot_monthCollisions_", str_replace(opt, " ", ""))
        # print(plotTag)
        
        p <- ggplot(dt) +
          geom_boxplot(aes(x = Month, y = Collisions), fill = "mediumpurple1", alpha = 0.8) +
          facet_wrap(~opt) +
          theme(legend.position="none") +
          labs(y="Number of Collisions", x = "") +
          theme(strip.background=element_rect(fill="grey95"))

        output[[plotTag]] <- renderPlot(p)
        
        # save plot externally
        p2 <- p+labs(title=str_replace_all(specLabel, "_", " "))
        ggsave(paste0(plotTag, ".png"), p2, path = path2ShinyOut_Outputs, width = 19, height = 12, units = "cm") 
        
      }))
    
    # update pbar
    progress_genModelOuts$inc(2/pbarNSteps)
    
    
    # --- summary tables
    df_monthlyColl %>%
      mutate(sumTable = pmap(list(dt = data, spec = specLabel, opt = option), function(dt, spec, opt){
        
        dt %<>% 
          group_by(Month) %>%
          summarise(Mean = mean(Collisions), 
                    SD = sd(Collisions), CV = SD/Mean, 
                    Median = median(Collisions), 
                    #IQR = IQR(Collisions), 
                    `2.5%` = quantile(Collisions, 0.025), 
                    `25%` = quantile(Collisions, 0.25),
                    `75%` = quantile(Collisions, 0.75),
                    `97.5%` = quantile(Collisions, 0.975)
                    ) %>%
          mutate_at(.vars = vars(Mean:`97.5%`), funs(round), 3) %>%
          mutate_at(.vars = vars(Mean:`97.5%`), funs(sprintf(fmt = "%.3f", .)))
        
        
        #print(dt)
        
        sumTableTag <- paste0(spec, "_summTable_monthCollisions_", str_replace(opt, " ", ""))
        #print(sumTableTag)
        
        output[[sumTableTag]] <- renderDataTable({
          datatable(dt, rownames = FALSE, 
                    caption = paste0("Model ", opt), 
                    options = list(pageLength = 12, dom = 't'))
        })
        
        fwrite(dt, file.path(path2ShinyOut_Outputs, paste0(sumTableTag, ".csv")))
        
      }))
    
    # update pbar
    progress_genModelOuts$inc(3/pbarNSteps)
    
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~ density plots and summary tables of overall collisons, for each option and species   ~~~ #
    
    df_overallColl <- df %>%
      gather(Month, Collisions, -c(option, specLabel, turbineModel, iter)) %>%
      mutate(Month = factor(Month, levels = unique(Month)),
             specLabel = as.character(specLabel)) %>%
      group_by(option, specLabel, turbineModel, iter) %>%
      summarise(overalCollisions = sum(Collisions)) %>%
      group_by(specLabel, turbineModel) %>%
      nest()
    
    
    # update pbar
    progress_genModelOuts$inc(4/pbarNSteps)
    
    
    # -- Plots
    df_overallColl %>%
      mutate(plot=pmap(list(dt = data, spec = specLabel), function(dt, spec){
        
        plotTag <- paste0(spec, "_plot_overallCollisions")
        #print(plotTag)
        
        p <- ggplot(dt) +
          geom_density(aes(x = overalCollisions, colour = option, fill = option), alpha = 0.2) +
          labs(x="Number of Collisions", y = "Probability Density") +
          scale_colour_brewer(palette = "Set1") +
          scale_fill_brewer(palette = "Set1") +
          theme(legend.title=element_blank(), legend.position="top") 
        
        output[[plotTag]] <- renderPlot(p)
        
        # save plot externally
        p2 <- p+labs(title=str_replace_all(specLabel, "_", " "))
        ggsave(paste0(plotTag, ".png"), p2, path = path2ShinyOut_Outputs, width = 19, height = 12, units = "cm")
      }))
      
    
    # update pbar
    progress_genModelOuts$inc(5/pbarNSteps)
    

    # -- Summary tables
    df_overallColl %>%
      mutate(sumPlot = pmap(list(dt = data, spec = specLabel, turb = turbineModel), function(dt, spec, turb){
        
        dt %<>%
          mutate(Turbine = turb) %>%
          rename(Option = option) %>%
          group_by(Turbine, Option) %>%
          summarise(Mean = mean(overalCollisions), 
                    SD = sd(overalCollisions), CV = SD/Mean, Median = median(overalCollisions), 
                    #IQR = IQR(overalCollisions), 
                    `2.5%` = quantile(overalCollisions, 0.025), 
                    `25%` = quantile(overalCollisions, 0.25),
                    `75%` = quantile(overalCollisions, 0.75),
                    `97.5%` = quantile(overalCollisions, 0.975)) %>%
          mutate_at(.vars = vars(Mean:`97.5%`), funs(round), digits = 3) %>%
          mutate_at(.vars = vars(Mean:`97.5%`), funs(sprintf(fmt = "%.3f", .))) %>%
          ungroup() %>% select(-Turbine)  # leave turbine model out of the table for now - current version with only one turbine model per simulation
        
        #print(dt)
        
        sumTableTag <- paste0(spec, "_summTable_overallCollisions")
        #print(sumTableTag)
        
        output[[sumTableTag]] <- renderDataTable({
          datatable(dt, rownames = FALSE, 
                    options = list(
                      #autoWidth = TRUE,
                      pageLength = 12, 
                      dom = 't'))
        })
        
        fwrite(dt, file.path(path2ShinyOut_Outputs, paste0(sumTableTag, ".csv")))
      }))
    
    # update pbar
    progress_genModelOuts$inc(6/pbarNSteps)
    
  })
  
  
  
  
  #' --------------------------------------------------------------------------------------------------------
  #  ----   Massage and relocate summaries of randomly generated parameter values for user's Download    ----
  #' --------------------------------------------------------------------------------------------------------

  observeEvent(rv$sCRM_output_ls, {
  
    slctSpecLabels <- slctSpeciesTags()$specLabel
    
    # Turbine parameters
    turbSampFiles <- list.files(path = file.path(path2Outputs_results, "tables/"), pattern = "_sampledTurbineParameters")
    
    walk(turbSampFiles, function(x){
    
      cSpecLabel <- slctSpecLabels[str_which(string = x, slctSpecLabels)]
      
      cSpecTurbSampledData <- fread(paste0(path2Outputs_results, "/tables/", x)) %>%
        select(-V1) %>%
        mutate_at(.vars = vars(Mean:IQR), funs(round), digits = 4) %>%
        mutate_at(.vars = vars(Mean:IQR), funs(sprintf(fmt = "%.4f", .)))
      
      fwrite(cSpecTurbSampledData, file = file.path(path2ShinyOut_Outputs, paste0(cSpecLabel, "_sampledTurbineParameters.csv")))
    })
    
    
    # Bird parameters
    birdSampFiles <- list.files(path =file.path(path2Outputs_results, "tables/"), pattern = "_sampledBirdParameters")
    
    walk(birdSampFiles, function(x){
      
      cSpecLabel <- slctSpecLabels[str_which(string = x, slctSpecLabels)]
      
      cSpecBirdSampledData <- fread(paste0(path2Outputs_results, "/tables/", x)) %>%
        select(-V1) %>%
        mutate_at(.vars = vars(Mean:IQR), funs(round), digits = 4) %>%
        mutate_at(.vars = vars(Mean:IQR), funs(sprintf(fmt = "%.4f", .)))
      
      fwrite(cSpecBirdSampledData, file = file.path(path2ShinyOut_Outputs, paste0(cSpecLabel, "_sampledBirdParameters.csv")))
    })
    
  })
  
  

  
  
  #' ----------------------------------------------------------------
  #  ----              Download model outputs                    ----
  #' ----------------------------------------------------------------
  
  output$dwnld_ModelOuts <- downloadHandler(
    filename = function() {
      "modelOutputs.zip"
    },
    content = function(file) {
      basedir <- getwd()
      setwd(file.path("shinyOutputs", sessTempOutFolder))
      fs <- list.files()
      zip(zipfile = file, files = fs)
      setwd(basedir)
    },
    contentType = "application/zip"
  )

  
  
  
  
  
  #' ---------------------------------------------------------
  #  ----       Session's house-cleaning                  ----
  #' ---------------------------------------------------------
  
  # --- Delete temporary folders (& files) created during the current session
  onStop(function(){
    #cat("Session stopped\n")
    unlink(file.path("shinyOutputs", sessTempOutFolder), recursive = TRUE)
    unlink(path2Outputs_results, recursive = TRUE)
  })
  
  
  
  

  #' ----------------------------------------------------------------
  #  ----              Miscellaneous  Stuff                    ----
  #' ----------------------------------------------------------------
  
  # Version updates - describing latest developments/updates implemented in the app
  observeEvent(input$appvrsn, {
    showModal(
      modalDialog(size = "l",
                  title = h3("Release Notes"),
                  h4("v2.3.1 - February, 2019"),
                  p("Added user-assistance features and fixed issues raised since previous release"),
                  tags$ul(
                    tags$li(tags$b("Additions & Updates"), 
                            tags$ul(
                              tags$li("Consortium's logos added"),
                              tags$li("Added default body length, wing span and flight speeds for most of the default species"),
                              tags$li("Implemented facility to save and restore input parameter values, allowing user to recover previously saved app status e.g. if the app disconnects."),
                              tags$li("Final check on missing values and data before proceeding to simulation. Simulation voided and warnings issued if key data absent"),
                              tags$li("Bugs fixed, including: fault on re-ordering in plots of monthly bird densities; error due to previous maximum height of 300m in FHD data (addressed by padding FHD data with zeros from 300m to 500m)")
                            )),
                    tags$li(tags$b("To-Do List"),
                            tags$ul(
                              tags$li("Allow user to choose a tag for the scenario under simulation, to be used as a prefix in the output file names"),
                              tags$li("Add option to upload data into input tables (e.g. turbine's 'Monthly Operation' table)")
                            ))
                  ),
                  h4("v2.2.1 - March 20th, 2018"),
                  p("Major release based on feedback and discussions with the steering group after the first release. The following lists are not exhaustive, i.e. only the most relevant changes mentioned"),
                  tags$ul(
                    tags$li(tags$b("Additions & Updates"), 
                            tags$ul(
                              tags$li("Wind farm's 'Target Power' replaced with 'Number of Turbines'"),
                              tags$li("Turbine parameters 'Rotor Radius', 'Air Gap' and 'Maximum Blade Witdh' made fixed (i.e. stochasticity present in previous release was removed)"),
                              tags$li("Turbine's 'Rotation Speed' and 'Blade Pitch' are now simulated from Truncated Normals (bounded at 0) by default. Also, bug found by MacArthur Green has been fixed, i.e. the model does not override this option in favour of a windpseed relationship"),
                              tags$li("The option of simulating 'Rotation Speed' and 'Blade Pitch' in relation to 'Wind Speed' (now simulated from a Truncated Normal bounded at 0) was kept in case data from developers is made available"),
                              tags$li("Biometric parameters 'Body Length', 'Wing Span' and 'Flight Speed' are now simulated as Truncated Normals bounded at 0"),
                              tags$li("Biometric parameters 'Nocturnal Activity', 'Basic Avoidance', 'Extended Avoidance' and 'Proportion at CRH' are now simulated as Beta distributions"),
                              tags$li("Implemented facility to upload user-defined bootstrap samples of flight heights distributions (FHD), either for listed species or new species"),
                              tags$li("Added set of options to specify bird monthly densities: (a) Truncated Normal bounded at 0; (b) Reference points (min, max and percentiles) & (c) Random samples"),
                              tags$li("Implemented on-the-fly validation feature for inputs sense-check, flagging up nonsensical values (e.g. negative SDs, decimal 'Number of Blades, etc)"),
                              tags$li("Sampled parameter values are now included in the model output .zip file"),
                              tags$li("Implemented an online user support facility (GitHub) for users to e.g. submit issues found access the User's manual")
                            )),
                    tags$li(tags$b("To-Do List"),
                            tags$ul(
                              tags$li("Add logos of Marine Scotland, HiDef & DMP"),
                              tags$li("Implement Bookmark feature to save current app status, e.g. allowing chosen model inputs to be recycled for future simulations"),
                              tags$li("Allow user to choose a tag for the scenario under simulation, to be used as a prefix in the output file names"),
                              tags$li("Add option to upload data into input tables (e.g. turbine's 'Monthly Operation' table)")
                            ))
                  ),
                  easyClose = TRUE
      ))
  }, priority = 10)
  
  
  
  
  
  #' ------------------------------------------------------------------
  #  ----         Debugging and value checking tools            ----
  #' ------------------------------------------------------------------

  # output$inputRVs <- renderPrint({
  #   str(reactiveValuesToList(input), max.level = 3)
  # })
  # 
  # output$out_inputs_biom <- renderPrint({
  #   str(reactiveValuesToList(rv), max.level = 3)
  # })

  # output$out_inputs_monthDens <- renderPrint({
  #   print(inputs_monthDensPars())
  # })

  # observe(label="console",{
  #   if(input$console != 0) {
  #     options(browserNLdisabled=TRUE)
  #     saved_console<-".RDuetConsole"
  #     if (file.exists(saved_console)) load(saved_console)
  #     isolate(browser())
  #     save(file=saved_console,list=ls(environment()))
  #   }
  # })

      
}



