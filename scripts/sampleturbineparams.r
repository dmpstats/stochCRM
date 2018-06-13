# Samples parameters for the turbine --------------------------------------
  # NB: Masden comment - if no measure of variance provided, only the mean value is used
  


#### BC ##### -- adding option of sampling from a prob distns alone  & adding simulated windspeed values to records =====================
# Rotor speed & Pitch -------------------------------------------------------------

if(TurbineData$RotorSpeedAndPitch_SimOption == "windSpeedReltn"){
  
  randomSample <- sample(1:length(rotorSpeed), iter, replace=T)
  
  # wind speed
  sampledTurbine$WindSpeed <- wind.speed.m.s[randomSample]
  
  # rotor speed
  sampledTurbine$RotorSpeed <- rotorSpeed[randomSample]
  
  # Pitch
  sampledTurbine$Pitch <- rotorPitch[randomSample]
  
  sampledTurbine$Pitch = sampledTurbine$Pitch*pi / 180 #### Transform Pitch from degrees to radians, needed for Collision Risk Sheet
  
}else{
  if(TurbineData$RotorSpeedAndPitch_SimOption == "probDist"){
    
    # Rotor speed
    sampledTurbine$RotorSpeed<- sampleRotnSpeed(iter, TurbineData$RotationSpeed[t], TurbineData$RotationSpeedSD[t])
    
    # Pitch
    sampledTurbine$Pitch<- samplePitch(iter, TurbineData$Pitch[t], TurbineData$PitchSD[t])
    
    sampledTurbine$Pitch = sampledTurbine$Pitch*pi / 180 #### Transform Pitch from degrees to radians, needed for Collision Risk Sheet
  }
}



# Radius  -----------------------------------------------------------------


  if(!is.na(TurbineData$RotorRadiusSD[t])){
    
    sampledTurbine$RotorRadius<- sampleRotorRadius(iter, TurbineData$RotorRadius[t], TurbineData$RotorRadiusSD[t])
    
  } else {
    
    sampledTurbine$RotorRadius<- rep(TurbineData$RotorRadius[t], iter)
    
  }
  
  
  

# Hub height --------------------------------------------------------------


  if(!is.na(TurbineData$HubHeightAddSD[t])){
    
    sampledTurbine$HubHeight <- sampleHubHeightAdd(iter, TurbineData$HubHeightAdd[t], TurbineData$HubHeightAddSD[t])
    
  } else {
    
    sampledTurbine$HubHeight <- rep(TurbineData$HubHeightAdd[t], iter)
    
  }
  
  sampledTurbine$HubHeight <- sampledTurbine$RotorRadius + sampledTurbine$HubHeight
  

# Blade width -------------------------------------------------------------


    if(!is.na(TurbineData$BladeWidthSD[t])){
      
      sampledTurbine$BladeWidth <- sampleBladeWidth(iter, TurbineData$BladeWidth[t], TurbineData$BladeWidthSD[t])
      
    } else {
      
        sampledTurbine$BladeWidth <- rep(TurbineData$BladeWidth[t], iter)

      }
  

# Monthly estimates below here --------------------------------------------
  
  for(currentMonth in monthLabels){
    
    # separate out the current month mean and SD. Species.count is already filtered for current species
    workingMean <- TurbineData %>% select(contains(currentMonth)) %>% select(contains('Mean'))
    
    workingSD <- TurbineData %>% select(contains(currentMonth)) %>% select(contains('SD'))
    
    workingOp <- TurbineData %>% select(contains(currentMonth)) %>% select(-contains('SD'), -contains('Mean'))
    
    # if we have an SD, then we sample, other wise just the mean
    if(!is.na(workingSD[1,1])){
      
      workingVect <- sampleOp(iter, workingMean[1,1], workingSD[1,1]) 
      
      ### === BC BUG FIXING === ### workingOp needs to be converted from a data.frame otherwise it doesn't return the vector of differences (only the 1st difference)
      sampledTurbine[,grep(currentMonth, names(sampledTurbine))] <- as.numeric(workingOp) - workingVect  
      
      # will explicitly rep mean, although not needed as filling into the DF
      ### === BC BUG FIXING === ### workingOp needs to be converted from a data.frame (see above)
    } else {sampledTurbine[,grep(currentMonth, names(sampledTurbine))] <- as.numeric(workingOp) - rep(workingMean[1,1], iter)}
    
  }
  

