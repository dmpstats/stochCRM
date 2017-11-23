
# pcoll function definitions ----------------------------------------------
 #= Masdeon commentary [Calculate collision probability at any given point along the rotor blade]

#' original pcoll function very poorly constructed - 3 args indicated
#' r, phi, updown - calls several global workspoce objects however
#' Args:
#' r, phi, updown, coverC, blade width, rotor: radius, speed, pitch, flight type, wingspan, blade number, 


pcoll = function (r, phi, updown) {
 
 cell_val <- ifelse (r > 1, 21, which(coverC$rad == (round(ceiling(r*100)/5) * 5)/100))
 upper = coverC[cell_val,1]
 lower = coverC[cell_val - 1, 1]

 p = (r - lower) / (upper-lower)
 c = coverC[cell_val-1, 2] + p * (coverC[cell_val,2] - coverC[cell_val-1,2])


 radius <- sampledTurbine$RotorRadius[i] * r
 chord <- sampledTurbine$BladeWidth[i] * c
 omega <- sampledTurbine$RotorSpeed[i] * 2 * pi /60
 pitch <- sampledTurbine$Pitch[i] * pi /180
 phi <- phi * pi /180
 
 direction <- ifelse(updown == "up", 1, -1)
 
 Wingspan2 <- ifelse(species.dat$Flight == "Flapping", sampledBirdParams$WingSpan, sampledBirdParams$WingSpan * abs(cos(phi)))
 
 multiplier <- TurbineData$Blades[t] * omega /(2 * pi * sampledBirdParams$FlightSpeed[i])
 
 alpha <- sampledBirdParams$FlightSpeed[i]/(radius * omega)
 
 CollideLength_1 <- abs(direction * chord * sin(pitch) + alpha * chord * cos(pitch))
 
 CollideLength2 <- ifelse(sampledBirdParams$BodyLength[i] > Wingspan2 * alpha,
 
                          sampledBirdParams$BodyLength[i], Wingspan2 * alpha)
 
 
 ifelse(radius == 0, 1,  (multiplier * (CollideLength_1 + CollideLength2)))
 
 }





###### Function 3


pcollxy = function(x,y,updown) {

			r = (x * x + y * y) ^ 0.5

				phi <- ifelse ( y == 0,

				ifelse( x >= 0, pi /2, -pi/2),
			
				atan (x/y))

			phi2 <- ifelse ( y < 0, phi + pi , phi)

			phi2 = phi2 * 180 / pi

			pcoll(r, phi2, updown)


}

#### Function 3


xrisksum2 = function (y, xinc,updown) {

		xmax = (1- y * y) ^ 0.5
		imax = as.integer (xmax/xinc)

		risk = (pcollxy (imax * xinc, y, updown) / 2 + pcollxy(xmax, y, updown)/2) * (xmax - imax * xinc)

		risk2 = risk + (pcollxy(0,y,updown) / 2 + pcollxy(imax * xinc, y, updown)/2) * xinc
	
		for (i in 1: (imax - 1)) {
		
			risk2 = risk2 + pcollxy(i * xinc, y, updown) * xinc

					}


	ifelse(imax > 0, 2 * risk2, 2*risk)


	}


