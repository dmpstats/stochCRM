# --- Number of Turbines
nTurbs <- input$numInput_windfarmPars_nTurbines

if(!is.na(nTurbs)){
 # js$backgroundCol("numInput_windfarmPars_nTurbines", "white")
  if(!is.integer(nTurbs)){
    createAlert(session, anchorId = "alert", alertId = "alertInput_windfarmPars_nTurbines", title = "Ahem...",
              content = "</b>Number of turbines<b> must be an integer", append = TRUE, style = "danger")
    }else{
      if(!nTurbs>0){
        createAlert(session, anchorId = "alert", alertId = "alertInput_windfarmPars_nTurbines", title = "Ahem...",
                    content = "<b>Number of turbines</b> must be a positive <br/> integer", append = TRUE, style = "danger")
      }else{
        closeAlert(session, alertId = "alertInput_windfarmPars_nTurbines")
      }
    }  
}#else{
#  js$backgroundCol("numInput_windfarmPars_nTurbines", NAsHighlightColor)
#}



# --- Latitude
lat  <- input$numInput_windfarmPars_Latitude

if(!is.na(lat)){
  if(!between(lat, -90, 90)){
    createAlert(session, anchorId = "alert", alertId = "alertInput_windfarmPars_Latitude", title = "Ahem...",
                content = "<b>Latitude</b> must be between -90 and <br/> 90 degrees", append = TRUE, style = "danger")
  }else{
    closeAlert(session, alertId = "alertInput_windfarmPars_Latitude")
  }  
}



# --- windfarm width
windFarmWitdth <- input$numInput_windfarmPars_width
if(!is.na(windFarmWitdth)){
  if(windFarmWitdth <= 0){
    createAlert(session, anchorId = "alert", alertId = "alertInput_windfarmPars_width", title = "Ahem...",
                content = "<b>Windfarm width</b> must be positive", append = TRUE, style = "danger")
  }else{
    closeAlert(session, alertId = "alertInput_windfarmPars_width")
  }
}



# --- Tidal offset
input$numInput_windfarmPars_tidalOffset





# --- Turbine model
turbPower <- input$numInput_turbinePars_turbinePower
if(!is.na(turbPower)){
  if(!turbPower > 0){
    createAlert(session, anchorId = "alert", alertId = "alertInput_turbinePars_turbinePower", title = "Oops...",
                content = "<b>Turbine power</b> must be positive", append = TRUE, style = "danger")
    }else{
      closeAlert(session, alertId = "alertInput_turbinePars_turbinePower")
    }
}


# --- Number of Blades
nBlades <- input$numInput_turbinePars_numBlades
if(!is.na(nBlades)){
  if(!nBlades > 0){
    createAlert(session, anchorId = "alert", alertId = "alertInput_turbinePars_numBlades", title = "Oops...",
                content = "<b>Number of blades</b> must be a positive <br/> integer", append = TRUE, style = "danger")
  }else{
    if(!is.integer(nBlades)){
      createAlert(session, anchorId = "alert", alertId = "alertInput_turbinePars_numBlades", title = "Oops...",
                  content = "<b>Number of blades</b> must be a positive <br/> integer", append = TRUE, style = "danger")
    }else{  
      closeAlert(session, alertId = "alertInput_turbinePars_numBlades")
    }
  } 
}


# --- Rotor Radius
rotRadius <- input$numInput_turbinePars_rotRadius
if(!is.na(rotRadius)){
   if(!rotRadius > 0){
    createAlert(session, anchorId = "alert", alertId = "alertInput_turbinePars_rotRadius", title = "Oops...",
                content = "<b>Rotor radius</b> must be positive", append = TRUE, style = "danger")
  }else{
      closeAlert(session, alertId = "alertInput_turbinePars_rotRadius")
  }
}


# --- Air Gap
airGap <- input$numInput_turbinePars_airGap
if(!is.na(airGap)){
  if(!airGap > 0){
    createAlert(session, anchorId = "alert", alertId = "alertInput_turbinePars_airGap", title = "Oops...",
                content = "<b>Air gap</b> must be positive", append = TRUE, style = "danger")
  }else{
    closeAlert(session, alertId = "alertInput_turbinePars_airGap")
  }
}



# --- Maximum blade width
maxBladeWdth <- input$numInput_turbinePars_maxBladeWdth
if(!is.na(maxBladeWdth)){
  if(!maxBladeWdth > 0){
    createAlert(session, anchorId = "alert", alertId = "alertInput_turbinePars_maxBladeWdth", title = "Oops...",
                content = "<b>Maximum blade width</b> must be positive", append = TRUE, style = "warning")
  }else{
    closeAlert(session, alertId = "alertInput_turbinePars_maxBladeWdth")
  }
}



# --- Mean & SD of Rotation Speed
tnormParamsAlert(expVal = input$numInput_turbinePars_rotnSpeed_E_, 
                 stdv = input$numInput_turbinePars_rotnSpeed_SD_, 
                 varName = "Rotation speed", varTag = "miscPars_windSpeed", 
                 session = session)

# --- Mean & SD of blade pitch
tnormParamsAlert(expVal = input$numInput_turbinePars_bladePitch_E_, 
                 stdv = input$numInput_turbinePars_bladePitch_SD_, 
                 varName = "Blade pitch", varTag = "miscPars_bladePitch", 
                 session = session)

# --- Mean & SD of Windspeed
tnormParamsAlert(expVal = input$numInput_miscPars_windSpeed_E_, 
                 stdv = input$numInput_miscPars_windSpeed_SD_, 
                 varName = "Wind speed", varTag = "miscPars_rotnSpeed", 
                 session = session)







# --- Mean & SD of biometric parameters
c_biomPars <- rv$biomParsInputs_ls
if(length(c_biomPars) > 0){

  #browser()

  c_biomParVals <- tibble(inputTag = names(c_biomPars), inputVal = unlist(c_biomPars)) %>%
    filter(!str_detect(inputTag, pattern= "flType_tp")) %>%
    mutate(inputVal = as.numeric(inputVal)) %>%
    mutate(inputTags_split = str_split(inputTag, "_"),
           specLabel = map_chr(inputTags_split, function(x) paste(x[-c(1:4)], collapse = "_")),
           specName = str_replace_all(specLabel, pattern = "_", " "),
           par = map_chr(inputTags_split, function(x) x[3]),
           hyper = map_chr(inputTags_split, function(x) x[4]),
           # par_hyper = paste(par, hyper, sep = "_"),
           parName = case_when(
             par == "bodyLt" ~ "Body length",
             par == "wngSpan" ~ "Wing Span",
             par == "flSpeed" ~ "Flight Speed",
             par == "noctAct" ~ "Nocturnal Activity",
             par == "basicAvoid" ~ "Basic avoidance",
             par == "extAvoid" ~ "Extended avoidance",
             par == "CRHeight" ~ "CRH"
           ))
  
  c_biomParVals %>%
    select(-inputTags_split, inputTag) %>%
    group_by(specLabel, par) %>%
    nest() %>%
    mutate(alerts = walk(data, function(x){
      input_E <- filter(x, hyper == "E")$inputVal
      input_SD <- filter(x, hyper == "SD")$inputVal
      cVarName <- unique(x$parName)
      cSpecName <- unique(x$specName)
      cVarTag <- paste0("biomPars_", str_replace_all(cSpecName, " ", "_"), "_", str_replace_all(cVarName, " ", ""))
      
      if(cVarName %in% c("Body length", "Wing Span", "Flight Speed")){
        tnormParamsAlert(expVal = input_E,
                         stdv = input_SD,
                         varName = paste0(cVarName, " for <br/>", cSpecName),
                         varTag = cVarTag,
                         session = session)  
      }
      
      if(cVarName %in% c("Nocturnal Activity", "Basic avoidance", "Extended avoidance", "CRH")){
        
        betaParamsAlert(p =input_E,
                        stdv = input_SD,
                        varName = paste0(cVarName, " for <br/>", cSpecName),
                        varTag = cVarTag,
                        session = session)
      }
      
    }))
}



#inputParsValidationStatus



# --- Monthly Wind availability
if(!is.null(input$hotInput_turbinePars_monthOps)){

  #browser()

  turbineOperation <- hot_to_r(input$hotInput_turbinePars_monthOps) %>% rownames_to_column(var="Variable") %>%
    gather(month, percentage, -Variable) %>%
    mutate(Variable = str_replace(Variable, " \\(%\\)", "")) %>%
    mutate(Variable = str_replace(Variable, " ", "_"))

  apply(turbineOperation, MARGIN = 1, FUN = function(x){

    pctage <- as.numeric(x[["percentage"]])
    
    if(!is.na(pctage)){
      if(!between(pctage, 0, 100)){
        createAlert(session, anchorId = "alert", alertId = paste("alertInput_turbinePars_monthOp_", x[["Variable"]], x[["month"]], sep = "_"), title = "Oops...",
                    content = paste0("<b>", str_replace(x[["Variable"]], "_", " "), " (", x[["month"]], ")</b> should be </br>between 0% and 100%"),
                    append = TRUE, style = "danger")
      }else{
        closeAlert(session, alertId = paste("alertInput_turbinePars_monthOp_", x[["Variable"]], x[["month"]], sep = "_"))
      }
    }
    
  })

}





## ------------------------------------------------------------------------ ##
##     Highlight background of numeric input fields with NAs                ##
## ------------------------------------------------------------------------ ##

names(isolate(input)) %>%
  str_subset(., pattern = "numInput_") %>%
  map(function(x){
    val <- isolate(input[[x]])
    if(!is.na(val)){
      js$backgroundCol(x,"white")
    }else{
      js$backgroundCol(x, NAsHighlightColor)
    }
  })


