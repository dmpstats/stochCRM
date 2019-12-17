
# Option 3 calculations ---------------------------------------------------
  #= masden: Do model using option 3 - modelled flight height distribution
  #= taking account of variation in risk along the rotor blades		

#### Work out Integral Flux

  FluxMin <- HD.d.y[1] * (2 * ((1 - HD.y[1] * HD.y[1])^0.5)) / 2 ### risk at lowest point on rotor blade
  FluxInt <- FluxMin + HD.d.y[41] * (2 * ((1 - HD.y[41] * HD.y[41])^0.5)) / 2 ### risk at highest point on rotor blade

  for (r in 2:40) {

	    FluxInt <- FluxInt + HD.d.y[r] * (2 * ((1 - HD.y[r] * HD.y[r])^0.5))  #### Fill in intermediate heights
 
		}


  FluxInt <- FluxInt * 0.05 * (2/pi)


##### Work out Collision Flux

## Up wind

CollMinUP <- HD.d.y[1] * xrisksum2(HD.y[1], 0.05, 1) / 2 ### risk at lowest point on rotor blade
CollIntUP <- CollMinUP + HD.d.y[41] * xrisksum2(HD.y[41], 0.05, 1) / 2 ### risk at highest point on rotor blade

  for (v in 2:40) {

	  CollIntUP <- CollIntUP + HD.d.y[v] * xrisksum2(HD.y[v], 0.05, 1)  #### Fill in intermediate heights
 
		}


  CollIntUP = CollIntUP * 0.05 * (2/pi)

## Down wind

CollMinDown <- HD.d.y[1] * xrisksum2(HD.y[1], 0.05, -1) / 2 ### risk at lowest point on rotor blade
CollIntDown <- CollMinDown + HD.d.y[41] * xrisksum2(HD.y[41], 0.05, -1) / 2 ### risk at highest point on rotor blade

for (w in 2:40) {

	CollIntDown = CollIntDown + HD.d.y[w] * xrisksum2(HD.y[w], 0.05, -1)  #### Fill in intermediate heights
 
		}


CollIntDown <- CollIntDown * 0.05 * (2/pi)

## Average Collision Integral

CollInt <- (Prop_Upwind * CollIntUP) + ((1-Prop_Upwind) * CollIntDown)
CollRiskDist <- CollInt/FluxInt ##Average collision risk for single rotor transit


## Calculate Collisions

# Operational = c(sampledJanOp[i],sampledFebOp[i],sampledMarOp[i],sampledAprOp[i],sampledMayOp[i],sampledJunOp[i],sampledJulOp[i],sampledAugOp[i],sampledSepOp[i],sampledOctOp[i],sampledNovOp[i],sampledDecOp[i])

  Operational <- sampledTurbine[i,] %>% select(contains('Op', ignore.case = F))

  Operational <- unlist(Operational)/100
  
  Option3_collisions_No_Avoid <- hours$Flux * CollInt * Operational
  
  Option3_CollisionRate <- data.frame(matrix(data = 0, nrow = 12, ncol = 1))
  names(Option3_CollisionRate) <- "Month"
  Option3_CollisionRate$Month <- monthLabels
  
  
  
  if(LargeArrayCorrection == "yes"){
    
              Option3_CollisionRate[,2] <- Option3_collisions_No_Avoid * (1-sampledBirdParams$AvoidanceExtended[i]) * L_ArrayCF
    
            } else {
              
              Option3_CollisionRate[,2] <- Option3_collisions_No_Avoid * (1-sampledBirdParams$AvoidanceExtended[i])
          
            }
  
  names(Option3_CollisionRate)[2] = "Collisions"



