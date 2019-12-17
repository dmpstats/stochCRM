# Format bird bootstrap files for consistency with template ---------------
  #' 
  #' Amending all the BTO FHD bootstrap files to conform to the bootstrap template
  #' Taking the files from Masdens zip file and:
  #'  - adding a column of heights (1 to 300 m)
  #'  - adding column names as per the template
  #'  - writing out with the suffix "_ht_dflt.csv" to match previous
  #'  
  #'  All files then copied to the repo
  #' 
  
  # downloaded template file from the shiny stochCRM app
  template <- read.csv("../../Downloads/FHD_bootstrapData_template (1).csv", header = T)
  
  # output them here
  dir.create("bootstrapFiles")

  inputDir <- "../Documents/GitHub/Masden_stochCRM/code_FinalVersion/code_FinalVersion/data/"
  
  stochCRMData <- list.files(inputDir)
  
  htFiles <- stochCRMData[grepl("_ht.", stochCRMData)]
  
  # for each, read, add row numbers and column headings
  for(i in 1:length(htFiles)){
    
    cat("reading ", htFiles[i], "\n")
    
    currentFile <- read.csv(paste0(inputDir, htFiles[i]), header = F)
    
    currentFile <- cbind(1:300, currentFile)
    
    names(currentFile) <- names(template)
    
    fileName <- gsub(".csv", "_dflt.csv", htFiles[i])
    
    outFile <- paste0("bootstrapFiles/", fileName)
    
    cat("outputting", outFile, "\n")
    
    write.csv(currentFile, file = outFile, row.names = F)
    
  }

