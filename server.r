

function(input, output, session) {
  
  # Global Variables
  prevSlctSpecs <- c()
  inputs_BiomParsPlotted <-  NULL
  birdDensPars_plotted <- list(NULL)
  flgtHghDstInputs_ls_Latest <- list(NULL)

  # reactive variables
  rv <- reactiveValues(
    addedSpec = NULL,
    biomParsInputs_ls = NULL,
    densParsInputs_ls = NULL,
    summaryTables_ls = NULL,
    sCRM_output_ls = NULL
    
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
  
  
  
  
  # Add bsTooltips for pop-ups with info on biometric parameters as new species are selected/added - needs to be added a-posteriori
  observeEvent(rv$addedSpec,{

    req(rv$addedSpec)

    cSpecTags <- slctSpeciesTags() %>%
      filter(species == rv$addedSpec)

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
  
  
  # --- render UI with upload button for User's FHD data - needs to be done via renderUI for tipify to work - fussy!!!!
  observeEvent(rv$addedSpec, {
    
    specLabel <- gsub(" ", "_", rv$addedSpec)
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
  
  
  # --- render UI with upload buttons for user's summaries AND samples of monthly densities - needs to be done via renderUI for tipify to work - fussy!!!!
  observeEvent(rv$addedSpec, {
    
    specLabel <- gsub(" ", "_", rv$addedSpec)
    
    # upload button and template for distribution summaries for monthly densities
    uiTag_summ <- paste0("renderUI_userUpload_monthDens_summaries_", specLabel)
    
    output[[uiTag_summ]] <- renderUI({
      
      divUI_monthDens_PctlsAndSample_DownButtons(
        introText = paste0("Provide reference points of the distributions of monthly densities of ", rv$addedSpec), 
        fileInputId = paste0("upldInput_monthDens_userDt_summaries_", specLabel), 
        fileInputPopUpText = "Data with reference points of distributions of monthly bird densities. Provide at least Min, 2.5th, 50th, 97.5th percentiles & Max (blank cells for no data). Downloading & using the adjacent template is highly recommended",
        downButtOutputId = paste0("dwnld_template_monthDens_summaries_", specLabel))
      
    })
  
    
    # upload button and template for samples for monthly densities
    uiTag_sample <- paste0("renderUI_userUpload_monthDens_samples_", specLabel)
    
    output[[uiTag_sample]] <- renderUI({
      
      divUI_monthDens_PctlsAndSample_DownButtons(
        introText = paste0("Provide random samples from the distributions of monthly densities of ", rv$addedSpec), 
        fileInputId = paste0("upldInput_monthDens_userDt_samples_", specLabel), 
        fileInputPopUpText = "Data with random samples from distributions of monthly bird densities. Provide at least 1000 draws. Downloading & using the adjacent template is highly recommended",
        downButtOutputId = paste0("dwnld_template_monthDens_samples_", specLabel))
      
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
  


  # --- Call and store flight height distributions of selected species in elements of a list
  flgtHghDstInputs_ls <- eventReactive(rv$FlgtHghDstInputs_loc, {

    #browser()
    
    req(length(rv$FlgtHghDstInputs_loc)>0)

    # input list elements of fileInputs are NULL until associate file is uploaded
    # Get rid of null elements (i.e. when a species has been selected but file hasn't been uploaded)
    FHDInputs_locations_ls_uploaded <- discard(rv$FlgtHghDstInputs_loc, is.null)

    req(length(FHDInputs_locations_ls_uploaded)>0)

    flgtHghDstInputs_ls <- FHDInputs_locations_ls_uploaded %>%
      map(~read.csv(.$datapath, header = TRUE))  # returns a list with each element holding the uploaded flight height distribution data for the selected species

    # make reactive object to only become invalid if the data change, therefore limiting unecessary chain reaction on dependent elements (e.g rendering of the associate plots)
    if(!identical(flgtHghDstInputs_ls, flgtHghDstInputs_ls_Latest)){
      flgtHghDstInputs_ls_Latest <<- flgtHghDstInputs_ls
      return(flgtHghDstInputs_ls)
    }

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

    startUpValues$rotationVsWind_df %>%
      bind_rows(data.frame(windSpeed = NA, rotationSpeed=NA)) %>%
      rhandsontable(rowHeaders=NULL, colHeaders = c("Wind speed (m/s)", "Rotation speed (rpm)")) %>%
      hot_cols(colWidths = 150) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })



  # --- Create input table for turbine rotation speed vs windspeed relationship
  output$hotInput_turbinePars_pitchVsWind <- renderRHandsontable({

    startUpValues$pitchVsWind_df %>%
      bind_rows(data.frame(windSpeed = NA, bladePitch=NA)) %>%
      rhandsontable(rowHeaders=NULL, colHeaders = c("Wind speed (m/s)", "Blade Pitch (deg)")) %>%
      hot_cols(colWidths = 150) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })



  # --- Create input table for bird monthly densities
  observeEvent(rv$addedSpec, {
    
    req(rv$addedSpec)
    
    cSpecTags <- slctSpeciesTags() %>%
      filter(species == rv$addedSpec)
    
    # input table for truncated normal parameters
    hotTag_truncNorm <- paste0("hotInput_birdDensPars_tnorm_", cSpecTags$specLabel)
    walk(hotTag_truncNorm, function(x){
      
      output[[x]] <- renderRHandsontable({
        
        if(cSpecTags$specLabel == "Black_legged_Kittiwake"){
          initDF <-  data.frame(matrix(c(startUpValues$meanDensity, startUpValues$sdDensity), nrow = 2, ncol = 12, byrow = TRUE,
                                       dimnames = list(c("meanDensity", "sdDensity"), month.name)),
                                stringsAsFactors = FALSE)
        }else{
          initDF <- data.frame(matrix(c(rep(1, 12), rep(0.0001, 12)), nrow = 2, ncol = 12, byrow = TRUE,
                                      dimnames = list(c("meanDensity", "sdDensity"), month.name)),
                               stringsAsFactors = FALSE)
        }
        
        initDF %>%
          rhandsontable(rowHeaderWidth = 160, 
                        rowHeaders = c("Mean birds/km^2", "SD of birds/km^2")) %>%
          hot_cols(colWidths = 90) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      })  
    })
    
  })
  
  
  
  #' -----------------------------------------------------------------
  #  ----          Data Templates Management                      ----
  #' -----------------------------------------------------------------
  
  
  # --- Manages downloads of data templates
  observeEvent(rv$addedSpec, {
    
    specLabel <- gsub(" ", "_", rv$addedSpec)
    
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
          filter(perc97.5 > 0.00005) %>%
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
    
    #browser()

    specLabel <- gsub(" ", "_", rv$addedSpec)

    plotTagBoot <- paste0("plot_defaultFHD_allBoots_", specLabel)
    plotTagBootQts <- paste0("plot_defaultFHD_QtsBoot_", specLabel)

    addedSpec_defFHDBootDt <- fread_possibly(paste0("data/", specLabel, "_ht_dflt.csv"))
      
    output[[plotTagBoot]] <- renderD3heatmap({

      validate(
        need(!is.null(addedSpec_defFHDBootDt),
             paste0("Warning: Default FHD data for ", rv$addedSpec, " not available. ",
                    "Select 'Other' to upload data. ",
                    "Missing data will lead to erraneous results for model Options 2 & 3"
             )),
        errorClass = "valErrorMsgClass"
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
             paste0("Warning: Default FHD data for ", rv$addedSpec, " not available. ",
                    "Select 'Other' to upload data. ",
                    "Missing data will lead to erraneous results for model Options 2 & 3"
                    )),
        errorClass = "valErrorMsgClass"
      )

      addedSpec_defFHDBootDt %>%
        mutate(Height = 1:nrow(.)) %>%
        gather(bootId, Prop, -Height) %>%
        group_by(Height) %>%
        summarise(
          perc2.5 = quantile(Prop, probs = 0.025),
          perc50 = quantile(Prop, probs = 0.5),
          perc97.5 = quantile(Prop, probs = 0.975)) %>%
        filter(perc97.5 > 0.0001) %>%
        ggplot(aes(y = perc50, x = Height)) +
        geom_pointrange(aes(ymin = perc2.5, ymax = perc97.5), col = "darkorange", size = 0.3) +
        #geom_line(size = 0.8) +
        labs(y = "Proportion", x = "Height (m)")

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
      sdRef <- ifelse(startUpValues$windSpeed_SD/stdev < 0.25, startUpValues$windSpeed_SD*5, startUpValues$windSpeed_SD)
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
          mutate(month = factor(month, labels = unique(month))) %>%
          ggplot(aes(x = density)) +
          geom_histogram(fill="darkorange", col = "black", bins = 50) +
          labs(x = "Number of birds per km2", y = "Frequency", title = paste0("Uploaded draws from the distribution of monthly densities of ", specName)) +
          coord_flip() +
          facet_grid(~month)
      })
      
      
      output[[plotTagQtlsBars]] <- renderPlot({
        
        if(ncol(x) == 12 & nrow(x) >= 1000){
          x %>%
            gather(month, density)  %>%
            mutate(month = factor(month, labels = unique(month))) %>%
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
               paste0("Warning: the dimension of the uploaded data with reference points for monthly densities of ", specName, " does not conform with size requirements",
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
          mutate(month = factor(month, labels = unique(month)))
        
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
            mutate(month = factor(month, labels = unique(month))) %>%
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

        #browser()
        
        as.data.frame(t(x)) %>% 
          rownames_to_column(var = "month") %>%
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
  #  ----         Inputs Validation System with feedback to UI                   ----
  #' --------------------------------------------------------------------------------
  
  observe({

    source("inputParsValidations.r", local = TRUE)

  })

  
  
  #' -----------------------------------------------------------------------
  #  ----            Collision Risk Simulation Model                    ----
  #' -----------------------------------------------------------------------

  observeEvent(input$actButtonInput_simulPars_GO, {

    # --- step 1: house-cleanig - remove output files left from previous runs ------ #
    file.remove(list.files("shinyOutputs/inputs", full.names = TRUE))
    file.remove(list.files("shinyOutputs/outputs", full.names = TRUE))
    file.remove(list.files("results/tables/", full.names = TRUE))
    

    #--- step 2: gather and arrange all the current input value to run in simulation function ----- # 

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
    
    
    
    # -- birds FHD
    #
    # FHD data source chosen for each species
    RVs_inputs_ls <- reactiveValuesToList(input)         
    FHD_UserOptions_ls <- RVs_inputs_ls[grep("userOpts_FHD_dtSrc", names(RVs_inputs_ls))]
    
    # save data to use in model as per user choice
    FHD_UserOptions_ls %>%
      walk2(., names(.), function(x, y){
        
        specLabel <- map_chr(str_split(y, "_"), ~ paste(.[-c(1:4)], collapse = "_"))
        
        if(x == "default"){
          
          # check if default data exists. If not save all 0s data for use in model
          if(file.exists(paste0("data/", specLabel, "_ht_dflt.csv"))){
            file.copy(from = paste0("data/", specLabel, "_ht_dflt.csv"), 
                      to = paste0("data/", specLabel, "_ht.csv"), overwrite = TRUE)  
          }else{
            sendSweetAlert(
              session = session,
              title = "Warning !!!",
              text = paste0("<b> Missing flight height distribution data for species ", specLabel, ". Results for model Options 2 and 3 will be invalid</b>"),
              type = "warning",
              html = TRUE
            )
            
            fwrite(template_FHD, file=paste0("data/", specLabel, "_ht.csv"), 
                   row.names = FALSE)
          }
        }else{
          if(x == "other"){
            
            cUserFHD_LsIndice <- str_which(names(flgtHghDstInputs_ls()), specLabel)
            
            if(length(cUserFHD_LsIndice) > 0){
              fileToWrite <- flgtHghDstInputs_ls()[[cUserFHD_LsIndice]]
            }else{
              sendSweetAlert(
                session = session,
                title = "Warning !!!",
                text = paste0("<b> Missing flight height distribution data for species ", specLabel, ". Results for model Options 2 and 3 will be invalid</b>"),
                type = "warning",
                html = TRUE
              )
              fileToWrite <- template_FHD
            }
            fwrite(fileToWrite, file=paste0("data/", specLabel, "_ht.csv"), 
                   row.names = FALSE)
          }}
      })
      
      
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
    rotSpeed_E <- ifelse(input$radGrpInput_rotationAndPitchOption == 'probDist',   
                           input$numInput_turbinePars_rotnSpeed_E_, NA)
    rotSpeed_SD <- ifelse(input$radGrpInput_rotationAndPitchOption == 'probDist', 
          input$numInput_turbinePars_rotnSpeed_SD_, NA)
    
    pitch_E <- ifelse(input$radGrpInput_rotationAndPitchOption == 'probDist', 
           input$numInput_turbinePars_bladePitch_E_, NA)
    
    pitch_SD <- ifelse(input$radGrpInput_rotationAndPitchOption == 'probDist', 
           input$numInput_turbinePars_bladePitch_SD_, NA)
    
    
    # merging turbine data
    turbineData <- tibble(
      TurbineModel = input$numInput_turbinePars_turbinePower,
      Blades = input$numInput_turbinePars_numBlades,
      RotorRadius	= input$numInput_turbinePars_rotRadius,
      RotorRadiusSD	= 0, #input$numInput_turbinePars_rotRadius_SD_,
      HubHeightAdd = input$numInput_turbinePars_airGap,
      HubHeightAddSD	= 0, #input$numInput_turbinePars_hubHght_SD_,
      BladeWidth	= input$numInput_turbinePars_maxBladeWdth,
      BladeWidthSD = 0, #input$numInput_turbinePars_maxBladeWdth_SD_,
      RotorSpeedAndPitch_SimOption = input$radGrpInput_rotationAndPitchOption,
      RotationSpeed = rotSpeed_E,
      RotationSpeedSD = rotSpeed_SD,
      Pitch = pitch_E,
      PitchSD = pitch_SD,
      windSpeedMean = input$numInput_miscPars_windSpeed_E_, 
      windSpeedSD = input$numInput_miscPars_windSpeed_SD_
    ) %>%
      left_join(., turbineData_Operation, by = "TurbineModel")
  
    
    
    # -- birds density data
    if(length(rv$mthDens_userOptions>0)){
      
      #browser()
      
      monthDensOpt <- rv$mthDens_userOptions %>%
        map2_df(., names(.), function(x,y){
          tibble(userOptionTag = as.character(y), userOption = as.character(x))
        }) %>%
        mutate(
          specLabel = str_replace(userOptionTag, "slctInput_userOpts_monthDens_sampler_", replacement = ""),
          specName = str_replace_all(specLabel, "_", " ")
          )

      monthDensData <- monthDensOpt %>% 
        group_by(userOption) %>%
        mutate(option = userOption) %>%
        nest() %>% 
        mutate(proc = map(data, function(x){
          
          
          if(unique(x$option) == "truncNorm"){
            if(length(rv$birdDensParsInputs_ls)>0){
              countData <- rv$birdDensParsInputs_ls %>%
                map2_df(., names(.), function(x,y){
                  x %>%
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
          }
          
          if(unique(x$option) == "reSamp"){
            if(length(monthDens_userDt_sample_ls())>0){
              
              out <- rbindlist(monthDens_userDt_sample_ls(), idcol = TRUE) %>%
                mutate(specLabel = str_replace_all(.id, "upldInput_monthDens_userDt_samples_", "")) %>%
                select(specLabel, January:December)
            }
          }
          
          if(unique(x$option) == "pcntiles"){
            if(length(monthDens_userDt_summs_ls())>0){
              
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
          }
          return(out)
        }))
      
      
      monthDensData %>%
        mutate(export = walk2(userOption, proc, function(x, y){
          
          if(x == "truncNorm"){
            fwrite(y, file = "data/CountData.csv", row.names = FALSE)
            write.csv(y, file = "shinyOutputs/inputs/birdDensityData_truncnorm.csv", row.names = FALSE)
          }
          
          if(x == "reSamp"){
            fwrite(y, file = "data/birdDensityData_samples.csv", row.names = FALSE)
            write.csv(y, file = "shinyOutputs/inputs/birdDensityData_samples.csv", row.names = FALSE)
          }
          
          if(x == "pcntiles"){
            fwrite(y, file = "data/birdDensityData_refPoints.csv", row.names = FALSE)
            write.csv(y, file = "shinyOutputs/inputs/birdDensityData_refPoints.csv", row.names = FALSE)
          }
          
        }))

    }
    
    
    # --- rotor speed and pitch vs windspeed
    windPowerData <- left_join(hot_to_r(input$hotInput_turbinePars_rotationVsWind), # produces "Warning in asMethod(object) : NAs introduced by coercion"
                               hot_to_r(input$hotInput_turbinePars_pitchVsWind),    # produces "Warning in asMethod(object) : NAs introduced by coercion"
                               by = "windSpeed") %>%
      rename(Wind = windSpeed, Rotor = rotationSpeed, Pitch = bladePitch) %>%
      drop_na()

    

    # --- wind farm parameters
    windfarmData <- tibble(
      #targetPower_MW = input$numInput_windfarmPars_nTurbines * input$numInput_turbinePars_turbinePower,
      nTurbines = input$numInput_windfarmPars_nTurbines,
      latitude_deg = input$numInput_windfarmPars_Latitude,
      width_km = input$numInput_windfarmPars_width,
      TidalOffset_m = input$numInput_windfarmPars_tidalOffset,
      upwindFlights_prop = input$sldInput_windfarmPars_upWindDownWindProp
    )
    
    # --- simulation Options
    simOptions <- tibble(
      iterations = input$sldInput_simulPars_numIter, 
      largeArrayCorrection = ifelse(input$chkBoxInput_simulPars_largeArrarCorr==TRUE, "yes", "no")
    )

    
    #' model function expects data to be provided in csv files, so saving them out for now to avoid code inconsistencies 
    #' Should change model function to expect data.frames instead of files once it's final version is established
    write.csv(birdData, file = "data/BirdData.csv", row.names = FALSE)
    write.csv(turbineData, file = "data/TurbineData.csv", row.names = FALSE)
    write.csv(windPowerData, file = paste0("data/windpower_", input$numInput_turbinePars_turbinePower, ".csv"), row.names = FALSE)
    
    
    
    #' save inputs to the user's downloadable folder - repeating the previous step (bar changes in turbine data) makes 
    #' code a bit inefficient, but the previous step should be temporary
    write.csv(birdData, file = "shinyOutputs/inputs/BirdData.csv", row.names = FALSE)
    write.csv(turbineData %>% select(-c(RotorRadiusSD, HubHeightAddSD, BladeWidthSD)) %>% rename(airGap = HubHeightAdd), 
              file = "shinyOutputs/inputs/TurbineData.csv", row.names = FALSE)
    write.csv(windPowerData, file = paste0("shinyOutputs/inputs/windpower_", input$numInput_turbinePars_turbinePower, ".csv"), row.names = FALSE)
    write.csv(windfarmData, file = paste0("shinyOutputs/inputs/windfarmData.csv"), row.names = FALSE)
    write.csv(simOptions, file = paste0("shinyOutputs/inputs/simOptions.csv"), row.names = FALSE)
    
    
      
    # ----- step 3: Set progress bar ----- #
    
    # Create a Progress objects for species and iterations within each species
    progress_Spec <- shiny::Progress$new()
    progress_Iter <- shiny::Progress$new()
    
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
    

    # ----- step 4: run simulation function ----- # 
    
    if(1){
      rv$sCRM_output_ls <- stochasticBand(
        workingDirectory="sCRM/",
        results_folder = "results",
        BirdDataFile = "data/BirdData.csv",
        TurbineDataFile = "data/TurbineData.csv",
        CountDataFile = "data/CountData.csv",
        FlightDataFile = "data/FlightHeight.csv",
        iter = input$sldInput_simulPars_numIter, 
        CRSpecies = slctSpeciesTags()$specLabel,
        TPower = input$numInput_windfarmPars_nTurbines*input$numInput_turbinePars_turbinePower, 
        LargeArrayCorrection = ifelse(input$chkBoxInput_simulPars_largeArrarCorr==TRUE, "yes", "no"), # "yes",
        WFWidth = input$numInput_windfarmPars_width,
        Prop_Upwind = input$sldInput_windfarmPars_upWindDownWindProp/100, # convert % (user input) to proportion (expected by model function)
        Latitude = input$numInput_windfarmPars_Latitude,
        TideOff = input$numInput_windfarmPars_tidalOffset,
        windSpeedMean = input$numInput_miscPars_windSpeed_E_, 
        windSpeedSD = input$numInput_miscPars_windSpeed_SD_,
        #windPowerData = windPowerData,
        updateProgress_Spec,  # pass in the updateProgress function so that it can update the progress indicator.
        updateProgress_Iter,
        DensityOpt = monthDensOpt  # pass in the user options for bird density data
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
    
    pmap(list(x = as.character(listLevels$option), y = as.character(listLevels$specLabel), z=as.character(listLevels$turbineModel)), 
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
  
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~  boxplots and summary tables of collisons per month, for each option and species  ~~~ #
    
    df_monthlyColl <- df %>%
      gather(Month, Collisions, -c(option, specLabel, turbineModel, iter)) %>%
      mutate(Month = factor(Month, levels = unique(Month)),
             specLabel = as.character(specLabel)) %>%
      group_by(specLabel, option, turbineModel) %>%
      nest()
    
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
        ggsave(paste0(plotTag, ".png"), p2, path="shinyOutputs/outputs", width = 19, height = 12, units = "cm")
        
      }))
    
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
        
        write.csv(dt, file.path("shinyOutputs/outputs", paste0(sumTableTag, ".csv")), row.names = FALSE)
        
      }))
    
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
        ggsave(paste0(plotTag, ".png"), p2, path="shinyOutputs/outputs", width = 19, height = 12, units = "cm")
      }))
      

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
        
        write.csv(dt, file.path("shinyOutputs/outputs", paste0(sumTableTag, ".csv")), row.names = FALSE)
      }))
    
    
  })
  
  
  
  
  #' --------------------------------------------------------------------------------------------------------
  #  ----   Massage and relocate summaries of randomly generated parameter values for user Download      ----
  #' --------------------------------------------------------------------------------------------------------

  observeEvent(rv$sCRM_output_ls, {
  
    slctSpecLabels <- slctSpeciesTags()$specLabel
    
    # Turbine parameters
    turbSampFiles <- list.files(path = "results/tables/", pattern = "_sampledTurbineParameters")
    
    walk(turbSampFiles, function(x){
    
      #browser()
      
      cSpecLabel <- slctSpecLabels[str_which(string = x, slctSpecLabels)]
      
      cSpecTurbSampledData <- fread(paste0("results/tables/", x)) %>%
        select(-V1) %>%
        mutate_at(.vars = vars(Mean:IQR), funs(round), digits = 4) %>%
        mutate_at(.vars = vars(Mean:IQR), funs(sprintf(fmt = "%.4f", .)))
      
      fwrite(cSpecTurbSampledData, file = paste0("shinyOutputs/outputs/", cSpecLabel, "_sampledTurbineParameters.csv"), row.names = FALSE)
    })
    
    
    # Bird parameters
    birdSampFiles <- list.files(path = "results/tables/", pattern = "_sampledBirdParameters")
    
    walk(birdSampFiles, function(x){
      
      cSpecLabel <- slctSpecLabels[str_which(string = x, slctSpecLabels)]
      
      cSpecBirdSampledData <- fread(paste0("results/tables/", x)) %>%
        select(-V1) %>%
        mutate_at(.vars = vars(Mean:IQR), funs(round), digits = 4) %>%
        mutate_at(.vars = vars(Mean:IQR), funs(sprintf(fmt = "%.4f", .)))
      
      fwrite(cSpecBirdSampledData, file = paste0("shinyOutputs/outputs/", cSpecLabel, "_sampledBirdParameters.csv"), row.names = FALSE)
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
      setwd("shinyOutputs")
      fs <- list.files()
      #fs <- list.files("shinyOutputs", full.names = TRUE)
      zip(zipfile = file, files = fs)
      setwd(basedir)
    },
    contentType = "application/zip"
  )

  

  #' ----------------------------------------------------------------
  #  ----              Miscellaneous  Stuff                    ----
  #' ----------------------------------------------------------------
  
  # Version updates - describing latest developments/updates implemented in the app
  observeEvent(input$appvrsn, {
    showModal(
      modalDialog(size = "l",
                  title = h3("Release Notes"),
                  h4("Current v2.2.1 - March 20th, 2018"),
                  p("This is a major release based on feedback and discussions with the steering group after the first release. The following lists are not exhaustive, i.e. only the most relevant changes mentioned"),
                  tags$ul(
                    tags$li(tags$b("Additions & Updates"), 
                            tags$ul(
                              tags$li("Win farm's 'Target Power' replaced with 'Number of Turbines'"),
                              tags$li("Turbine parameters 'Rotor Radius', 'Air Gap' and 'Maximum Blade Witdh' made fixed (i.e. stochasticity present in previous release was removed)"),
                              tags$li("Turbine's 'Rotation Speed' and 'Blade Pitch' are now simulated from Truncated Normals (bounded at 0) by default. Also, bug found by MacArthur Grenn has been fixed, i.e. the model does not override this option in favour of a windpseed relationship"),
                              tags$li("The option of simulating 'Rotation Speed' and 'Blade Pitch' in relation to 'Wind Speed' (now simulated from a Truncated Normal bounded at 0) was kept in case data from developers is made available"),
                              tags$li("Biometric parameters 'Body Length', 'Wing Span' and 'Flight Speed' are now simulated as Truncated Normals bounded at 0"),
                              tags$li("Biometric parameters 'Nocturnal Activity', 'Basic Avoidance', 'Extended Avoidance' and 'Proportion at CRH' are now simulated as Beta distributions"),
                              tags$li("Implemented facility to upload user-defined bootstrap samples of flight heights distributions (FHD), either for listed species or new species"),
                              tags$li("Added set of options to specify bird monthly densities: (a) Truncated Normal bounded at 0; (b) Reference points (min, max and percentiles) & (c) Random samples"),
                              tags$li("Implemented on-the-fly validation feature for inputs sense-check, flagging up nonsensical values (e.g. negative SDs, decimal 'Number of Blades, etc)"),
                              tags$li("Sampled parameter values are now included in the model output .zip file")
                            )),
                    tags$li(tags$b("To-Do List"),
                            tags$ul(
                              tags$li("Add logos of Marine Scotland, HiDef & DMP"),
                              tags$li("Implement Bookmark feature to save current app status, e.g. allowing chosen model inputs to be recycled for future simulations"),
                              tags$li("Allow user to choose a tag for the scenario under simulation, to be used as a prefix in the output file names"),
                              tags$li("Implement an online user support facility (probably via GitHub) for users to e.g. submit issues found or access the User's manual"),
                              tags$li("Add option to upload data into input tables (e.g. turbine's 'Monthly Operation' table)")
                            ))
                  ),
                  easyClose = TRUE
      ))
  }, priority = 10)
  
  
  

}



