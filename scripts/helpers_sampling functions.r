
# The reading of sampling functions ---------------------------------------
  # The following were all previously individual source calls in masden



# Sampling functions for birds --------------------------------------------

  
#= wingspan
  sampleWingSpan <- function(n, meanspan, sdspan){
    
    rnorm(n, meanspan, sdspan)
    
  }

#= bird length
  
  sampleBirdLength <- function(n, meanlength, sdlength){
    
    rnorm(n, meanlength, sdlength)
    
  }

#= CHR?
  
  sampleCRH <- function(n, meanCRH, sdCRH){
    
    rnorm(n, meanCRH, sdCRH)
    
  }

#= Bird heights
  
  sampleBirdFlight <- function(n, meanflight, sdflight){
    
    rnorm(n, meanflight, sdflight)
    
  }


#= Nocturnal (?)

  sampleNocturnal <- function(n, meannoc, sdnoc){
    
    rnorm(n, meannoc, sdnoc)
    
  }



#= Bird counts

  sampleCount <- function(n, meancount, sdcount){
    
    rtnorm(n, meancount, sdcount,0,2)
    
  }


#= Avoidance 

  sampleAvoidance <- function(n, meanavoid, sdavoid){
    
    rnorm(n, meanavoid, sdavoid)
    
  }



# Sampling functions for turbine pars -------------------------------------

#= Blade width
  
  sampleBladeWidth <- function(n, meanwidth, sdwidth){
    
    rnorm(n, meanwidth, sdwidth)
    
  }

#= rotor radius

  sampleRotorRadius <- function(n, meanrotor, sdrotor){
    
    rnorm(n, meanrotor, sdrotor)
    
  }

#= Hub height

  sampleHubHeightAdd <- function(n, meanadd, sdadd){
    
    rnorm(n, meanadd, sdadd)
    
  }

#= operation times?
  
  sampleOp <- function(n, meanop, sdop){
    
    rnorm(n, meanop, sdop)
    
  }


# Misc --------------------------------------------------------------------

#= basic CV in 100%
  
  CV <- function(mean, sd){
    
    (sd/mean)*100
    
  }




