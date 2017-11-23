
  TurbineData = read.table(TurbineDataFile, header = T, sep = ",")
  Pitch = TurbineData$Pitch*pi / 180 #### Transform Pitch, needed for Collision Risk Sheet
  
  



