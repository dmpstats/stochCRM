
# Option 1 calculations ---------------------------------------------------
#= Masden comment: Do model using option 1 - Site specific flight height information	


# Calculate potential transits through rotor area each month --------------


Operational <-  unlist(MonthlyOperational[i,])

Operational <- Operational/100


Option1_Transits <- hours$Flux * sampledBirdParams$PCH[i] * Operational

Option1_collisions_No_Avoid <- Option1_Transits * (P_Collision/100)

Option1_CollisionRate <- data.frame(Month = monthLabels, Collisions = NA)


if(LargeArrayCorrection == "yes"){

    Option1_CollisionRate$Collisions <- Option1_collisions_No_Avoid * (1-sampledBirdParams$AvoidanceBasic[i]) * L_ArrayCF
  
    } else {
    
      Option1_CollisionRate$Collisions<- Option1_collisions_No_Avoid * (1-sampledBirdParams$AvoidanceBasic[i])

        }

