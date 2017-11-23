stochasticBand <- function(
  workingDirectory,
  results_folder = "results", 
  BirdDataFile = "data/BirdData.csv",
  TurbineDataFile = "data/TurbineData.csv",
  CountDataFile = "data/CountData.csv",
  FlightDataFile = "data/FlightHeight.csv",
  iter = 10,
  CRSpecies = c("Black_legged_Kittiwake"),
  TPower = 600, 
  LargeArrayCorrection = "yes",
  WFWidth = 10, 
  Prop_Upwind = 0.5,
  Latitude = 55.8,
  TideOff = 2.5, 
  windSpeedMean = 7.74,
  windSpeedSD = 3.2,
  windPowerData = NULL,
  updateProgress_Spec = NULL,
  updateProgress_Iter = NULL) {
  
  
  
  for (s in 1 : length(CRSpecies)){
    
    
    if (is.function(updateProgress_Spec)) {
      text <- gsub("_", " ", CRSpecies[s])
      updateProgress_Spec(value = s/(length(CRSpecies)), detail = text)
      #updateProgress_Spec(value = s/(length(CRSpecies)) * i/iter, detail = text)
      #updateProgress_Spec(detail = text, n = length(CRSpecies))
    }
    
    
    for (i in 1:iter){
  
      Sys.sleep(0.1)

      # if (is.function(updateProgress_Iter)) {
      #   text <- NULL # paste0("Working through iteration ", i)
      #   updateProgress_Iter(detail = text, n = iter)
      # }
      
      
      if (is.function(updateProgress_Iter)) {
        text <- NULL # paste0("Working through iteration ", i)
        updateProgress_Iter(value = i/iter, detail = text)
      }
      
      #FlightDataFile
    }
   
    if (is.function(updateProgress_Iter)) {
      text <- NULL # paste0("Working through iteration ", i)
      updateProgress_Iter(value = 0, detail = text)
    }
     
  }
  
 
  
   
}