
# Minorly modified version from Masden ------------------------------------


CollisionRiskTab = data.frame(matrix(data = 0, nrow = 21, ncol = 7))

names(CollisionRiskTab) = c("radius", "chord", "alpha", "Up_length", "Up_P", "Down_length", "Down_P")

CollisionRiskTab$radius = seq(0, 1, 0.05)

CollisionRiskTab$chord = c(NA, 0.73, 0.79, 0.88, 0.96, 1.00, 0.98, 0.92, 0.85, 0.80, 0.75, 0.70, 0.64, 0.58, 0.52, 0.47,0.41, 0.37, 0.30,0.24,0.00) 

#### can be revised to match actual turbine blades

CollisionRiskTab$alpha[1] = NA
CollisionRiskTab$Up_length[1] = NA
CollisionRiskTab$Up_P[1] = 1
CollisionRiskTab$Down_length[1] = NA
CollisionRiskTab$Down_P[1] = 1


### populate collision risk table

for (u in 1:20) {
		
#### First calculate alphas

			CollisionRiskTab$alpha[u + 1] = sampledBirdParams$FlightSpeed[i] * (60/sampledTurbine$RotorSpeed[i]) /  
				(CollisionRiskTab$radius[u + 1] * sampledTurbine$RotorRadius[i] * 2 * pi)   

# collision risk calculations ---------------------------------------------



#### Now calculate upwind length


			CollisionRiskTab$Up_length[u+1] <- ifelse (CollisionRiskTab$alpha[u + 1] < (sampledBirdParams$BodyLength[i] /sampledBirdParams$WingSpan[i]),
			
	sampledBirdParams$BodyLength[i] + 	
		abs(sampledTurbine$BladeWidth[i]*CollisionRiskTab$chord[u + 1]*sin(sampledTurbine$Pitch[i])+
			(CollisionRiskTab$alpha[u + 1] * sampledTurbine$BladeWidth[i]*CollisionRiskTab$chord[u + 1]*cos(sampledTurbine$Pitch[i]))),

	
	(sampledBirdParams$WingSpan[i] * Flap_Glide * CollisionRiskTab$alpha[u + 1]) + 	
		abs(sampledTurbine$BladeWidth[i]*CollisionRiskTab$chord[u + 1]*sin(sampledTurbine$Pitch[i])+
			(CollisionRiskTab$alpha[u + 1] * sampledTurbine$BladeWidth[i]*CollisionRiskTab$chord[u + 1]*cos(sampledTurbine$Pitch[i]))))

#### Now calculate upwind probability of collision

			
	CollisionRiskTab$Up_P[u+1] = min (1, (TurbineData$Blades[t]/(60/sampledTurbine$RotorSpeed[i])) *
	                                    CollisionRiskTab$Up_length[u+1]/sampledBirdParams$FlightSpeed[i])

#### Now calculate downwind length

			ifelse (CollisionRiskTab$alpha[u + 1] < (sampledBirdParams$BodyLength[i] /sampledBirdParams$WingSpan[i]),
			
			        sampledBirdParams$BodyLength[i] + 	
		abs(-sampledTurbine$BladeWidth[i]*CollisionRiskTab$chord[u + 1]*sin(sampledTurbine$Pitch[i])+
			(CollisionRiskTab$alpha[u + 1] * sampledTurbine$BladeWidth[i]*CollisionRiskTab$chord[u + 1]*cos(sampledTurbine$Pitch[i])))
						-> CollisionRiskTab$Down_length[u+1],

	
	(sampledBirdParams$WingSpan[i] * Flap_Glide * CollisionRiskTab$alpha[u + 1]) + 	
		abs(-sampledTurbine$BladeWidth[i]*CollisionRiskTab$chord[u + 1]*sin(sampledTurbine$Pitch[i])+
			(CollisionRiskTab$alpha[u + 1] * sampledTurbine$BladeWidth[i]*CollisionRiskTab$chord[u + 1]*cos(sampledTurbine$Pitch[i])))
						-> CollisionRiskTab$Down_length[u+1])


#### Now calculate Down wind probability of collision

			
	CollisionRiskTab$Down_P[u+1] = min (1, (TurbineData$Blades[t]/(60/sampledTurbine$RotorSpeed[i])) *
	                                      CollisionRiskTab$Down_length[u+1]/sampledBirdParams$FlightSpeed[i])


}
	

Total_Up_Wind_P = 2 * (sum(CollisionRiskTab$radius[2:20] * CollisionRiskTab$Up_P[2:20]) + CollisionRiskTab$Up_P[21]/2) * 0.05

Total_Down_Wind_P = 2 * (sum(CollisionRiskTab$radius[2:20] * CollisionRiskTab$Down_P[2:20]) + CollisionRiskTab$Down_P[21]/2) * 0.05


P_Collision = (Prop_Upwind * Total_Up_Wind_P) + ((1-Prop_Upwind) * Total_Down_Wind_P)
P_Collision = 100 * P_Collision

