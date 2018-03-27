# The reading of various data files ---------------------------------------
  # following all previous individual source calls in masden

# Reading bird species parameters -----------------------------------------

  BirdData <- read.csv(BirdDataFile, header = T)
  
  row.names(BirdData) <-  BirdData$Species
  
  #### flapping or Gliding for collision risk sheet
  Flap_Glide <- ifelse (BirdData$Flight == "Flapping", 1, 2/pi) 


# Read turbine characteristics --------------------------------------------

  TurbineData <- read.csv(TurbineDataFile, header = T)
  
  #### Transform Pitch, needed for Collision Risk Sheet
  # superceeded by the sampled versions I believe
  #Pitch <- TurbineData$Pitch*pi/180 


# Read in bird count data -------------------------------------------------

  CountData <- read.csv(CountDataFile, header = T)


# Read in flight height data for birds ------------------------------------

  FlightHeight <- read.csv(FlightDataFile, header = T)







