
# Option 2 calculation ----------------------------------------------------

# Masden comment: Do model using option 2 - modelled flight height distribution		

##### create dataframe giving the width of the chord relative to its maximum width at given points along the radius of the rotor

  rad = round(seq(0,1,0.05),2) 
  
  circ = c(0.69,0.73,0.79,0.88,0.96,1,0.98,0.92,0.85,0.8,0.75,0.7,0.64,0.58,0.52,0.47,0.41,0.37,0.3,0.24,0)
  
  coverC = data.frame(cbind(rad, circ))

##### bring in call functions needed to calculate collision risk along blade

  source("scripts\\PCollFunctions.r", local=T)


##### Get flight height data for species of interest

#FH.dat = FlightHeight[,c(grep(CRSpecies[s], colnames(FlightHeight)))] ##using generic/'averaged' distribution curve
  FH.dat = FlightHeightSpec[,flight.boot.sample[i]] ## using bootstraps


##### start to calculate collision risk along the rotor blade


  HD.y <- round(seq(-1,1,0.05),2)
  
  
  height <- sampledTurbine$HubHeight[i] + (HD.y * sampledTurbine$RotorRadius[i]) + TideOff
  
  
  HD.d.y <- (FH.dat[floor(height)+1] + ((FH.dat[floor(height)+1] - FH.dat[ceiling(height)+1]) * (floor(height)-height))) * sampledTurbine$RotorRadius[i]

  risk.up <- vector()
  risk.down <- vector()


  for (q in 1:length(HD.y)) {
  
    	risk.up[q] <- xrisksum2(HD.y[q], 0.05, "up")
    	risk.down[q] <- xrisksum2(HD.y[q], 0.05, "down")
    
  	}

  d.risk.up <- HD.d.y * risk.up
  d.risk.down <- HD.d.y * risk.down
  
  Q2r <- 0.05 * (HD.d.y[1]/2 + HD.d.y[41]/2 + sum(HD.d.y[c(2:40)]))


  Option2_collisions_No_Avoid <- Option1_collisions_No_Avoid * Q2r / sampledBirdParams$PCH[i]

  Option2_CollisionRate <- data.frame(matrix(data = 0, nrow = 12, ncol = 1))
  names(Option2_CollisionRate) <- "Month"
  
  Option2_CollisionRate$Month = monthLabels


  Option2_CollisionRate[,2]<- if(LargeArrayCorrection == "yes"){

				Option2_collisions_No_Avoid * (1-sampledBirdParams$AvoidanceBasic[i]) * L_ArrayCF
          
    } else {
    
        Option2_collisions_No_Avoid * (1-sampledBirdParams$AvoidanceBasic[i])
    }

  names(Option2_CollisionRate)[2] <- "Collisions"


