
# fill bird parameters with sampled values --------------------------------
  #= [NB Masden note: if no measure of variance provided, only the mean value is used
  #= here replace all ifelse with if. Only a constant needs evaluating for the if - so ifelse inefficient



  
# Wing span ---------------------------------------------------------------
  if(!is.na(species.dat$WingspanSD)){
    
    sampledBirdParams$WingSpan <- sampleWingSpan(iter, species.dat$Wingspan, species.dat$WingspanSD)
    
  } else {
    
      sampledBirdParams$WingSpan <- rep(species.dat$Wingspan, iter)
  
    }
    
   
# body length -------------------------------------------------------------


  if(!is.na(species.dat$Body_LengthSD)){
    
    sampledBirdParams$BodyLength <- sampleBirdLength(iter, species.dat$Body_Length, species.dat$Body_LengthSD)
    
  } else {
    
      sampledBirdParams$BodyLength <- rep(species.dat$Body_Length, iter)
    
    }
  

# flight speed ------------------------------------------------------------

if(!is.na(species.dat$Flight_SpeedSD)){
  
  sampledBirdParams$FlightSpeed <- sampleFlightSpeed(iter, species.dat$Flight_Speed, species.dat$Flight_SpeedSD)   ##### BC ####
  
} else {
  
  sampledBirdParams$FlightSpeed <- rep(species.dat$Flight_Speed, iter)
  
}



# proportion collision ----------------------------------------------------


  if(!is.na(species.dat$Prop_CRH_ObsSD)){
    
    sampledBirdParams$PCH <- sampleCRH(iter, species.dat$Prop_CRH_Obs, species.dat$Prop_CRH_ObsSD)
    
  } else {
    
      sampledBirdParams$PCH <- rep(species.dat$Prop_CRH_Obs, iter)
    
    }
  
  

# Nocturnal activity ------------------------------------------------------

  if(!is.na(species.dat$Nocturnal_ActivitySD)){
    
    sampledBirdParams$NocturnalActivity <- sampleNocturnal(iter, species.dat$Nocturnal_Activity, species.dat$Nocturnal_ActivitySD)   ###  BC ###
    
  } else {
    
      sampledBirdParams$NocturnalActivity <- rep(species.dat$Nocturnal_Activity, iter)

    }




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


# Monthly density estimates below here --------------------------------------------

if(c_densOpt == "truncNorm"){
  
  for(currentMonth in monthLabels){
      
      # separate out the current month mean and SD. Species.count is already filtered for current species
      workingMean <- species.count %>% select(contains(currentMonth)) %>% select(-contains('SD'))
      
      workingSD <- species.count %>% select(contains(currentMonth)) %>% select(contains('SD'))
      
      # if we have an SD, then we sample, other wise just the mean
      if(!is.na(workingSD[1,1])){
        
        workingVect <- sampleCount_tnorm(iter, workingMean[1,1], workingSD[1,1])                  # <<<<< BC <<<<<
        
        sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- workingVect
        
        # will explicitly rep mean, although not needed as filling into the DF
      } else {
        sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- rep(workingMean[1,1], iter)}
    }
}



if(c_densOpt == "reSamp"){
  for(currentMonth in monthLabels){
    
    #browser()
  
    workingVect <- sampleCount_resample(n = iter, countsSample = species.count %>% select(contains(currentMonth)))
    
    sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- workingVect
    
  }
}
  


if(c_densOpt == "pcntiles"){
  for(currentMonth in monthLabels){
    
    cPcntls <- species.count %>% select(referenceProbs, contains(currentMonth))
    
    workingVect <- sampleCount_pctiles(iter, probs = cPcntls[, 1], countsPctls = cPcntls[, 2])
    
    sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- workingVect
    
  }
}


