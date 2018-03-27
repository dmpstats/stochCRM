
# Rotor properties - minor tidying of Masden ------------------------------


# get pitch and rotor values for turbine sampling -------------------------

  # truncated normal sampling of wind speed (non-negativity constraint) -- S is bigger than iter
  windSpeed<-rtnorm(S, windSpeedMean,windSpeedSD,0)

  #= Unsure of the umber of samples being taken here - gerater than iter seems to be the goal
  wind.speed.m.s<-sample(windSpeed, iter+(iter/5), replace=T)

#= read in the wind-speed/rotor-speed & pitch relationships
  
  windName <- paste("windpower",TurbineData$TurbineModel[t], sep="_")
  windD <- paste(windName,"csv", sep=".")
  windData <- read.csv(paste("data",windD, sep="/"), header=T)

  #remove anything less than wind threshold of power curve
  windThreshold <- windData$Wind[min(which(windData$Rotor != 0))]
  wind.speed.m.s <- wind.speed.m.s[wind.speed.m.s>windThreshold]


#= assign rotor speeds to wind speeds

  rotorSpeed <- numeric()
  rotorPitch <- numeric()
  
#= unsure about the construction below here - "else" looks hacky
  for (y in 1:length(wind.speed.m.s)){
    
    for (z in 1:length(windData$Wind)){
      
      if(z<length(windData$Wind)){
        
        
        if(wind.speed.m.s[y]>=windData$Wind[z] & wind.speed.m.s[y]<windData$Wind[z+1]){rotorSpeed[y]<-windData$Rotor[z]}
        if(wind.speed.m.s[y]>=windData$Wind[z] & wind.speed.m.s[y]<windData$Wind[z+1]){rotorPitch[y]<-windData$Pitch[z]}}else
          
          if(wind.speed.m.s[y]>=windData$Wind[z]){rotorSpeed[y]<-windData$Rotor[z]}
      if(wind.speed.m.s[y]>=windData$Wind[z]){rotorPitch[y]<-windData$Pitch[z]}
    }
  }
  