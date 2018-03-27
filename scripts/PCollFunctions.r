
# pcoll function definitions ----------------------------------------------
#= Masdeon commentary [Calculate collision probability at any given point along the rotor blade]

#' original pcoll function very poorly constructed - 3 args indicated
#' r, phi, direction - calls several global workspoce objects however
#' Args:
#' r, phi, direction, coverC, blade width, rotor: radius, speed, pitch, flight type, wingspan, blade number, 




# xrisksum2 function - calls pcoll ----------------------------------------


xrisksum2 = function (y, xinc, direction, 
                      inputRad = currentRad,
                      inputCirc = currentCirc,
                      inputRotorRadius = currentRotorRadius,
                      inputBladeWidth = currentBladeWidth,
                      inputRotorSpeed = currentRotorSpeed,
                      inputPitch = currentPitch,
                      inputFlight = currentFlightNumeric,
                      inputWingSpan = currentWingSpan,
                      inputFlightSpeed = currentFlightSpeed,
                      inputBirdLength = currentBirdLength,
                      inputBlades = currentBlades) {
  
  xmax = (1- y * y) ^ 0.5
  
  imax = as.integer (xmax/xinc)
  
  # avoid repeating calcs - once here for use later
  pcollxy_imax <- pcollxy (imax * xinc, y, direction)
  
  pcollxy_xmax <- pcollxy(xmax, y, direction)
  pcollxy_0 <- pcollxy(0, y, direction)
  
  # 
  risk = (pcollxy_imax/2 + pcollxy_xmax/2) * (xmax - imax * xinc)
  risk2 = risk + (pcollxy_0/2 + pcollxy_imax/2) * xinc
  
  # loop over k
  for (k in 1: (imax - 1)) {
    
    
    risk2 = risk2 + pcollxy(k*xinc, y, direction) * xinc
    
    
  } # end of k loop
  
  
  ifelse(imax > 0, 2 * risk2, 2*risk)
  
  
}



# pcollxy - called within xrisksum2 -----------------------------------------
  #' In Masden construction, relies on various objects from outside function
  #' These will be passed explicitly:
  #' sampledTurbine [ith row]
  #' sampledBirdParams [ith row]
  
pcollxy = function(x, y, direction, 
                   inputRad = currentRad,
                   inputCirc = currentCirc,
                   inputRotorRadius = currentRotorRadius,
                   inputBladeWidth = currentBladeWidth,
                   inputRotorSpeed = currentRotorSpeed,
                   inputPitch = currentPitch,
                   inputFlight = currentFlightNumeric,
                   inputWingSpan = currentWingSpan,
                   inputFlightSpeed = currentFlightSpeed,
                   inputBirdLength = currentBirdLength,
                   inputBlades = currentBlades
) {
  
  r = (x * x + y * y) ^ 0.5
  
  phi <- ifelse ( y == 0,
                  
                  ifelse( x >= 0, pi /2, -pi/2),
                  
                  atan (x/y))
  
  phi2 <- ifelse ( y < 0, phi + pi , phi)
  
  phi2 = phi2 * 180 / pi
  
  pcoll(r, phi2, direction)
  
  
}

# pcoll - called within xrisksum2 and pcollxy -----------------------------------------
#' In Masden construction, calls various objects from outside function
#' These will be passed explicitly:
#' sampledTurbine [ith row]
#' sampledBirdParams [ith row]


pcoll = function (r, phi, direction, 
                  inputRad = currentRad,
                  inputCirc = currentCirc,
                  inputRotorRadius = currentRotorRadius,
                  inputBladeWidth = currentBladeWidth,
                  inputRotorSpeed = currentRotorSpeed,
                  inputPitch = currentPitch,
                  inputFlight = currentFlightNumeric,
                  inputWingSpan = currentWingSpan,
                  inputFlightSpeed = currentFlightSpeed,
                  inputBirdLength = currentBirdLength,
                  inputBlades = currentBlades) {
  
  cell_val <- ifelse (r > 1, 21, which(inputRad == (round(ceiling(r*100)/5) * 5)/100))
  upper = inputRad[cell_val]
  lower = inputRad[cell_val - 1]
  
  
  p = (r - lower) / (upper-lower)
  
  c = inputCirc[cell_val-1] + p * (inputCirc[cell_val] - inputCirc[cell_val-1])
  
  
  radius <- inputRotorRadius * r
  chord <- inputBladeWidth * c
  omega <- inputRotorSpeed * 2 * pi /60
  pitch <- inputPitch * pi /180
  phi <- phi * pi /180
  
  Wingspan2 <- ifelse(inputFlight == 1, inputWingSpan, inputWingSpan * abs(cos(phi)))
  
  
  multiplier <- inputBlades * omega /(2 * pi * inputFlightSpeed)
  
  alpha <- inputFlightSpeed/(radius * omega)
  
  CollideLength_1 <- abs(direction * chord * sin(pitch) + alpha * chord * cos(pitch))
  
  CollideLength2 <- ifelse(inputBirdLength > Wingspan2 * alpha,

                           inputBirdLength, Wingspan2 * alpha)
  
  
  ifelse(radius == 0, 1,  (multiplier * (CollideLength_1 + CollideLength2)))
  
}



