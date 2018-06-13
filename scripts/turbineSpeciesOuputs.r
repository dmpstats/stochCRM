# Output tables and box plots ---------------------------------------------
  #' Partial rewrite of Masden to dplyr the table generation
  #' Very verbose previously


    ###relabel sampledTurbine by turbine name###
    assign(paste(TurbineData$TurbineModel[t],"params", sep="_"), sampledTurbine)
    
    ###relabel results table
    assign(paste(CRSpecies[s],TurbineData$TurbineModel[t],"opt1", "results", sep="_"), tab1)
    assign(paste(CRSpecies[s],TurbineData$TurbineModel[t],"opt2", "results", sep="_"), tab2)
    assign(paste(CRSpecies[s],TurbineData$TurbineModel[t],"opt3", "results", sep="_"), tab3)
    
    #== Table 1 generation
      monthlySummaryOpt1 <- tab1 %>% gather(key = month, value = count) %>% mutate(month = factor(month, levels = monthLabels)) %>%
        group_by(month) %>% summarise(mean = mean(count), SD=sd(count), CV=sd(count)/mean(count), Med = median(count), IQR = IQR(count))

  #== Table 2 generation
      
    monthlySummaryOpt2 <- tab2 %>% gather(key = month, value = count) %>% mutate(month = factor(month, levels = monthLabels)) %>%
      group_by(month) %>% summarise(mean = mean(count), SD=sd(count), CV=sd(count)/mean(count), Med = median(count), IQR = IQR(count))

  #== Table 3 generation

    monthlySummaryOpt3 <- tab3 %>% gather(key = month, value = count) %>% mutate(month = factor(month, levels = monthLabels)) %>%
      group_by(month) %>% summarise(mean = mean(count), SD=sd(count), CV=sd(count)/mean(count), Med = median(count), IQR = IQR(count))

# save tables  -----------------------------------------------------------
  
    fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"monthlySummaryOpt1.csv", sep="_")
    write.csv (monthlySummaryOpt1, paste(results_folder,"tables",  fileName, sep="/"))
    fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"monthlySummaryOpt2.csv", sep="_")
    write.csv (monthlySummaryOpt2, paste(results_folder,"tables",  fileName, sep="/"))
    fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"monthlySummaryOpt3.csv", sep="_")
    write.csv (monthlySummaryOpt3, paste(results_folder,"tables",  fileName, sep="/"))



# Add data to density tables ----------------------------------------------

    ###DATA TO DENSITY SUMMARY TABLE###
    densitySummary[,(t-1)*3+1]=rowSums(tab1)
    densitySummary[,(t-1)*3+2]=rowSums(tab2)
    densitySummary[,(t-1)*3+3]=rowSums(tab3)


# Create and save boxplots ------------------------------------------------

  fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s], sep="_")
  fileName<-paste(fileName, ".png", sep="")
  png(paste(results_folder, "figures", fileName, sep="/"),width=500,height=900,res=100)
  
    par(mfrow = c( 3, 1))
    boxplot(tab1, outline=F, ylim=c(0,max(max(tab1), max(tab2), max(tab3))),ylab="Number of collisions", main="Option 1")
    boxplot(tab2, outline=F, ylim=c(0,max(max(tab1), max(tab2), max(tab3))),ylab="Number of collisions", main="Option 2")
    boxplot(tab3, outline=F, ylim=c(0,max(max(tab1), max(tab2), max(tab3))),ylab="Number of collisions", main="Option 3")
  
  dev.off()


# Make density plots -----------------------------------------------------


  fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s], "density",sep="_")
  fileName<-paste(fileName, ".png", sep="")
    png(paste(results_folder, "figures", fileName, sep="/"),width=800,height=600,res=100)
    
      plot(density(rowSums(tab1)), main="", xlab="Number of Collisions", ylab ="Probability Density", col="chocolate",
           xlim=(c(0, max(max(density(rowSums(tab1))$x),max(density(rowSums(tab2))$x),max(density(rowSums(tab3))$x)))), 
           ylim=(c(0, max(max(density(rowSums(tab1))$y),max(density(rowSums(tab2))$y),max(density(rowSums(tab3))$y)))))
      
      lines(density(rowSums(tab2)), col="darkgoldenrod", lty=2)
      lines(density(rowSums(tab3)), col="darkorange4", lty=3)
      
      
      legend("topright", c("Option 1", "Option 2", "Option 3"), cex=0.8, lty=1:3, col = c("chocolate", "darkgoldenrod", "darkorange4"))
    
  dev.off()


# Results summary table ---------------------------------------------------
  # Largely unmodifed from Masden

  resultsSummary[(s*nrow(TurbineData)-(nrow(TurbineData)-t))*3-2,]<-c(CRSpecies[s], TurbineData$TurbineModel[t], 1, 
                                                                      mean(rowSums(tab1)),sd(rowSums(tab1)), 
                                                                      CV(mean(rowSums(tab1)),sd(rowSums(tab1))), 
                                                                      median(rowSums(tab1)), IQR(rowSums(tab1))) #option 1
  resultsSummary[(s*nrow(TurbineData)-(nrow(TurbineData)-t))*3-1,]<-c(CRSpecies[s], TurbineData$TurbineModel[t], 2, 
                                                                      mean(rowSums(tab2)),sd(rowSums(tab2)), 
                                                                      CV(mean(rowSums(tab2)),sd(rowSums(tab2))), 
                                                                      median(rowSums(tab2)), IQR(rowSums(tab2))) #option 2
  resultsSummary[(s*nrow(TurbineData)-(nrow(TurbineData)-t))*3-0,]<-c(CRSpecies[s], TurbineData$TurbineModel[t], 3, 
                                                                      mean(rowSums(tab3)),sd(rowSums(tab3)), 
                                                                      CV(mean(rowSums(tab3)),sd(rowSums(tab3))), 
                                                                      median(rowSums(tab3)), IQR(rowSums(tab3))) #option 3


# Summaries of input parameters -------------------------------------------
  
  
  
  #== Birds
  
  sampledBirdParamsSummary <- sampledBirdParams %>% 
    gather(key = Parameter, value = value) %>% 
    mutate(Parameter = factor(Parameter, levels = names(sampledBirdParams))) %>%
    group_by(Parameter) %>% 
    summarise(Mean = mean(value), SD=sd(value), Median = median(value), IQR = IQR(value))

  #== write out bird parameter tables
  fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"sampledBirdParameters.csv", sep="_")
  write.csv (sampledBirdParamsSummary, paste(results_folder,"tables",  fileName, sep="/"))



# Turbine parameters ------------------------------------------------------

  
  sampledTurbineParamsSummary <- sampledTurbine %>% gather(key = Parameter, value = value) %>% mutate(Parameter = factor(Parameter, levels = names(sampledTurbine))) %>%
    group_by(Parameter) %>% summarise(Mean = mean(value), SD=sd(value), Median = median(value), IQR = IQR(value)) %>%
    mutate(Parameter = ifelse(Parameter == "Pitch", "Pitch_rad", as.character(Parameter)))
  
  
  #== write out turbine parameter tables
  fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"sampledTurbineParameters.csv", sep="_")
  write.csv (sampledTurbineParamsSummary, paste(results_folder, "tables",fileName, sep="/"))
  
  
  
  