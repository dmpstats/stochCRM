
# fill bird parameters with sampled values --------------------------------
  #= [NB Masden note: if no measure of variance provided, only the mean value is used
  #= here replace all ifelse with if. Only a constant needs evaluating for the if - so ifelse inefficient

# Avoidance ---------------------------------------------------------------


  if(!is.na(species.dat$AvoidanceBasicSD)){
    
    sampledBirdParams$AvoidanceBasic <- sampleAvoidance(iter, species.dat$AvoidanceBasic, species.dat$AvoidanceBasicSD)
    
  } else {
    
      sampledBirdParams$AvoidanceBasic <- rep(species.dat$AvoidanceBasic, iter)
    
    }
      
    
  # extended variant
  
  if(!is.na(species.dat$AvoidanceExtendedSD)){
    
    sampledBirdParams$AvoidanceExtended <- sampleAvoidance(iter, species.dat$AvoidanceExtended, species.dat$AvoidanceExtendedSD)
    
  } else {
    
      sampledBirdParams$AvoidanceExtended <- rep(species.dat$AvoidanceExtended, iter)
    
    }
   
  
# Wing span ---------------------------------------------------------------
  if(!is.na(species.dat$WingspanSD)){
    
    sampledBirdParams$WingSpan <- sampleWingSpan(iter, species.dat$Wingspan, species.dat$WingspanSD)
    
  } else {
    
      sampledBirdParams$WingSpan <- rep(species.dat$Wingspan, iter)
  
    }
    
   
# body length -------------------------------------------------------------


  if(!is.na(species.dat$Body_LengthSD)){
    
    sampledBirdParams$BodyLength <-   sampleBirdLength(iter, species.dat$Body_Length, species.dat$Body_LengthSD)
    
  } else {
    
      sampledBirdParams$BodyLength <- rep(species.dat$Body_Length, iter)
    
    }
  
# proportion collision ----------------------------------------------------


  if(!is.na(species.dat$Prop_CRH_ObsSD)){
    
    sampledBirdParams$PCH <- sampleCRH(iter, species.dat$Prop_CRH_Obs, species.dat$Prop_CRH_ObsSD)
    
  } else {
    
      sampledBirdParams$PCH <- rep(species.dat$Prop_CRH_Obs, iter)
    
    }
  
  

# flight speed ------------------------------------------------------------


  if(!is.na(species.dat$Flight_SpeedSD)){
    
    sampledBirdParams$FlightSpeed <- sampleBirdFlight(iter, species.dat$Flight_Speed, species.dat$Flight_SpeedSD)
    
  } else {
    
      sampledBirdParams$FlightSpeed <- rep(species.dat$Flight_Speed, iter)
    
    }
    
  

# Nocturnal activity ------------------------------------------------------


  if(!is.na(species.dat$Nocturnal_ActivitySD)){
    
    sampledBirdParams$NocturnalActivity <- sampleBirdFlight(iter, species.dat$Nocturnal_Activity, species.dat$Nocturnal_ActivitySD)
    
  } else {
    
      sampledBirdParams$NocturnalActivity <- rep(species.dat$Nocturnal_Activity, iter)

    }
  


# Monthly estimates below here --------------------------------------------
  
  for(currentMonth in monthLabels){
      
      # separate out the current month mean and SD. Species.count is already filtered for current species
      workingMean <- species.count %>% select(contains(currentMonth)) %>% select(-contains('SD'))
      
      workingSD <- species.count %>% select(contains(currentMonth)) %>% select(contains('SD'))
      
      # if we have an SD, then we sample, other wise just the mean
      if(!is.na(workingSD[1,1])){
        
        workingVect <- sampleCount(iter, workingMean[1,1], workingSD[1,1]) 
        
        sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- workingVect
        
        # will explicitly rep mean, although not needed as filling into the DF
      } else {sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- rep(workingMean[1,1], iter)}
    
    }
    
  