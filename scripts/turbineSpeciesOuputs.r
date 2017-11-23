

###relabel sampledTurbine by turbine name###
assign(paste(TurbineData$TurbineModel[t],"params", sep="_"), sampledTurbine)

###relabel results table
assign(paste(CRSpecies[s],TurbineData$TurbineModel[t],"opt1", "results", sep="_"), tab1)
assign(paste(CRSpecies[s],TurbineData$TurbineModel[t],"opt2", "results", sep="_"), tab2)
assign(paste(CRSpecies[s],TurbineData$TurbineModel[t],"opt3", "results", sep="_"), tab3)

###output monthly summaries###
###OPTION 1###
monthlySummaryOpt1 = data.frame(matrix(data = 0, ncol = 6, nrow = 12))
names(monthlySummaryOpt1) = c("Month", "Mean", "SD", "CV", "Median", "IQR")
monthlySummaryOpt1[,1]=monthLabels
monthlySummaryOpt1[1,2:6]=c(mean(tab1$Jan),sd(tab1$Jan), CV(mean(tab1$Jan),sd(tab1$Jan)), median(tab1$Jan),IQR(tab1$Jan))
monthlySummaryOpt1[2,2:6]=c(mean(tab1$Feb),sd(tab1$Feb),CV(mean(tab1$Feb),sd(tab1$Feb)), median(tab1$Feb),IQR(tab1$Feb))
monthlySummaryOpt1[3,2:6]=c(mean(tab1$Mar),sd(tab1$Mar),CV(mean(tab1$Mar),sd(tab1$Mar)), median(tab1$Mar),IQR(tab1$Mar))
monthlySummaryOpt1[4,2:6]=c(mean(tab1$Apr),sd(tab1$Apr),CV(mean(tab1$Apr),sd(tab1$Apr)), median(tab1$Apr),IQR(tab1$Apr))
monthlySummaryOpt1[5,2:6]=c(mean(tab1$May),sd(tab1$May),CV(mean(tab1$May),sd(tab1$May)), median(tab1$May),IQR(tab1$May))
monthlySummaryOpt1[6,2:6]=c(mean(tab1$Jun),sd(tab1$Jun),CV(mean(tab1$Jun),sd(tab1$Jun)), median(tab1$Jun),IQR(tab1$Jun))
monthlySummaryOpt1[7,2:6]=c(mean(tab1$Jul),sd(tab1$Jul),CV(mean(tab1$Jul),sd(tab1$Jul)), median(tab1$Jul),IQR(tab1$Jul))
monthlySummaryOpt1[8,2:6]=c(mean(tab1$Aug),sd(tab1$Aug),CV(mean(tab1$Aug),sd(tab1$Aug)), median(tab1$Aug),IQR(tab1$Aug))
monthlySummaryOpt1[9,2:6]=c(mean(tab1$Sep),sd(tab1$Sep),CV(mean(tab1$Sep),sd(tab1$Sep)), median(tab1$Sep),IQR(tab1$Sep))
monthlySummaryOpt1[10,2:6]=c(mean(tab1$Oct),sd(tab1$Oct),CV(mean(tab1$Oct),sd(tab1$Oct)), median(tab1$Oct),IQR(tab1$Oct))
monthlySummaryOpt1[11,2:6]=c(mean(tab1$Nov),sd(tab1$Nov),CV(mean(tab1$Nov),sd(tab1$Nov)), median(tab1$Nov),IQR(tab1$Nov))
monthlySummaryOpt1[12,2:6]=c(mean(tab1$Dec),sd(tab1$Dec),CV(mean(tab1$Dec),sd(tab1$Dec)), median(tab1$Dec),IQR(tab1$Dec))

###OPTION 2###
monthlySummaryOpt2 = data.frame(matrix(data = 0, ncol = 6, nrow = 12))
names(monthlySummaryOpt2) = c("Month", "Mean", "SD","CV", "Median", "IQR")
monthlySummaryOpt2[,1]=monthLabels
monthlySummaryOpt2[1,2:6]=c(mean(tab2$Jan),sd(tab2$Jan),CV(mean(tab2$Jan),sd(tab2$Jan)), median(tab2$Jan),IQR(tab2$Jan))
monthlySummaryOpt2[2,2:6]=c(mean(tab2$Feb),sd(tab2$Feb),CV(mean(tab2$Feb),sd(tab2$Feb)), median(tab2$Feb),IQR(tab2$Feb))
monthlySummaryOpt2[3,2:6]=c(mean(tab2$Mar),sd(tab2$Mar), CV(mean(tab2$Mar),sd(tab2$Mar)), median(tab2$Mar),IQR(tab2$Mar))
monthlySummaryOpt2[4,2:6]=c(mean(tab2$Apr),sd(tab2$Apr),CV(mean(tab2$Apr),sd(tab2$Apr)), median(tab2$Apr),IQR(tab2$Apr))
monthlySummaryOpt2[5,2:6]=c(mean(tab2$May),sd(tab2$May),CV(mean(tab2$May),sd(tab2$May)), median(tab2$May),IQR(tab2$May))
monthlySummaryOpt2[6,2:6]=c(mean(tab2$Jun),sd(tab2$Jun),CV(mean(tab2$Jun),sd(tab2$Jun)), median(tab2$Jun),IQR(tab2$Jun))
monthlySummaryOpt2[7,2:6]=c(mean(tab2$Jul),sd(tab2$Jul),CV(mean(tab2$Jul),sd(tab2$Jul)), median(tab2$Jul),IQR(tab2$Jul))
monthlySummaryOpt2[8,2:6]=c(mean(tab2$Aug),sd(tab2$Aug),CV(mean(tab2$Aug),sd(tab2$Aug)), median(tab2$Aug),IQR(tab2$Aug))
monthlySummaryOpt2[9,2:6]=c(mean(tab2$Sep),sd(tab2$Sep),CV(mean(tab2$Sep),sd(tab2$Sep)), median(tab2$Sep),IQR(tab2$Sep))
monthlySummaryOpt2[10,2:6]=c(mean(tab2$Oct),sd(tab2$Oct),CV(mean(tab2$Oct),sd(tab2$Oct)), median(tab2$Oct),IQR(tab2$Oct))
monthlySummaryOpt2[11,2:6]=c(mean(tab2$Nov),sd(tab2$Nov),CV(mean(tab2$Nov),sd(tab2$Nov)), median(tab2$Nov),IQR(tab2$Nov))
monthlySummaryOpt2[12,2:6]=c(mean(tab2$Dec),sd(tab2$Dec),CV(mean(tab2$Dec),sd(tab2$Dec)), median(tab2$Dec),IQR(tab2$Dec))

###OPTION 3###
monthlySummaryOpt3 = data.frame(matrix(data = 0, ncol = 6, nrow = 12))
names(monthlySummaryOpt3) = c("Month", "Mean", "SD", "CV", "Median", "IQR")
monthlySummaryOpt3[,1]=monthLabels
monthlySummaryOpt3[1,2:6]=c(mean(tab3$Jan),sd(tab3$Jan),CV(mean(tab3$Jan),sd(tab3$Jan)), median(tab3$Jan),IQR(tab3$Jan))
monthlySummaryOpt3[2,2:6]=c(mean(tab3$Feb),sd(tab3$Feb),CV(mean(tab3$Feb),sd(tab3$Feb)), median(tab3$Feb),IQR(tab3$Feb))
monthlySummaryOpt3[3,2:6]=c(mean(tab3$Mar),sd(tab3$Mar),CV(mean(tab3$Mar),sd(tab3$Mar)), median(tab3$Mar),IQR(tab3$Mar))
monthlySummaryOpt3[4,2:6]=c(mean(tab3$Apr),sd(tab3$Apr),CV(mean(tab3$Apr),sd(tab3$Apr)), median(tab3$Apr),IQR(tab3$Apr))
monthlySummaryOpt3[5,2:6]=c(mean(tab3$May),sd(tab3$May),CV(mean(tab3$May),sd(tab3$May)), median(tab3$May),IQR(tab3$May))
monthlySummaryOpt3[6,2:6]=c(mean(tab3$Jun),sd(tab3$Jun),CV(mean(tab3$Jun),sd(tab3$Jun)), median(tab3$Jun),IQR(tab3$Jun))
monthlySummaryOpt3[7,2:6]=c(mean(tab3$Jul),sd(tab3$Jul),CV(mean(tab3$Jul),sd(tab3$Jul)), median(tab3$Jul),IQR(tab3$Jul))
monthlySummaryOpt3[8,2:6]=c(mean(tab3$Aug),sd(tab3$Aug),CV(mean(tab3$Aug),sd(tab3$Aug)), median(tab3$Aug),IQR(tab3$Aug))
monthlySummaryOpt3[9,2:6]=c(mean(tab3$Sep),sd(tab3$Sep),CV(mean(tab3$Sep),sd(tab3$Sep)), median(tab3$Sep),IQR(tab3$Sep))
monthlySummaryOpt3[10,2:6]=c(mean(tab3$Oct),sd(tab3$Oct),CV(mean(tab3$Oct),sd(tab3$Oct)), median(tab3$Oct),IQR(tab3$Oct))
monthlySummaryOpt3[11,2:6]=c(mean(tab3$Nov),sd(tab3$Nov),CV(mean(tab3$Nov),sd(tab3$Nov)), median(tab3$Nov),IQR(tab3$Nov))
monthlySummaryOpt3[12,2:6]=c(mean(tab3$Dec),sd(tab3$Dec),CV(mean(tab3$Dec),sd(tab3$Dec)), median(tab3$Dec),IQR(tab3$Dec))

###SAVE MONTHLY SUMMARIES###
fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"monthlySummaryOpt1.csv", sep="_")
write.csv (monthlySummaryOpt1, paste(results_folder,"tables",  fileName, sep="\\"))
fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"monthlySummaryOpt2.csv", sep="_")
write.csv (monthlySummaryOpt2, paste(results_folder,"tables",  fileName, sep="\\"))
fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"monthlySummaryOpt3.csv", sep="_")
write.csv (monthlySummaryOpt3, paste(results_folder,"tables",  fileName, sep="\\"))

###DATA TO DENSITY SUMMARY TABLE###
densitySummary[,(t-1)*3+1]=rowSums(tab1)
densitySummary[,(t-1)*3+2]=rowSums(tab2)
densitySummary[,(t-1)*3+3]=rowSums(tab3)

###make 3 panel figure of boxplots and save###

fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s], sep="_")
fileName<-paste(fileName, ".png", sep="")
png(paste(results_folder, "figures", fileName, sep="\\"),width=500,height=900,res=100)
par(mfrow = c( 3, 1))

boxplot(tab1, outline=F, ylim=c(0,max(max(tab1), max(tab2), max(tab3))),ylab="Number of collisions", main="Option 1")
boxplot(tab2, outline=F, ylim=c(0,max(max(tab1), max(tab2), max(tab3))),ylab="Number of collisions", main="Option 2")
boxplot(tab3, outline=F, ylim=c(0,max(max(tab1), max(tab2), max(tab3))),ylab="Number of collisions", main="Option 3")

dev.off()

###make density plots and save###
fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s], "density",sep="_")
fileName<-paste(fileName, ".png", sep="")
png(paste(results_folder, "figures", fileName, sep="\\"),width=800,height=600,res=100)

plot(density(rowSums(tab1)), main="", xlab="Number of Collisions", ylab ="Probability Density", col="chocolate",xlim=(c(0, max(max(density(rowSums(tab1))$x),max(density(rowSums(tab2))$x),max(density(rowSums(tab3))$x)))), ylim=(c(0, max(max(density(rowSums(tab1))$y),max(density(rowSums(tab2))$y),max(density(rowSums(tab3))$y)))))
lines(density(rowSums(tab2)), col="darkgoldenrod", lty=2)
lines(density(rowSums(tab3)), col="darkorange4", lty=3)


legend("topright", c("Option 1", "Option 2", "Option 3"), cex=0.8, lty=1:3, col = c("chocolate", "darkgoldenrod", "darkorange4"))

dev.off()

##add results summary to summary table###
resultsSummary[(s*nrow(TurbineData)-(nrow(TurbineData)-t))*3-2,]<-c(CRSpecies[s], TurbineData$TurbineModel[t], 1, mean(rowSums(tab1)),sd(rowSums(tab1)), CV(mean(rowSums(tab1)),sd(rowSums(tab1))), median(rowSums(tab1)), IQR(rowSums(tab1))) #option 1
resultsSummary[(s*nrow(TurbineData)-(nrow(TurbineData)-t))*3-1,]<-c(CRSpecies[s], TurbineData$TurbineModel[t], 2, mean(rowSums(tab2)),sd(rowSums(tab2)), CV(mean(rowSums(tab2)),sd(rowSums(tab2))), median(rowSums(tab2)), IQR(rowSums(tab2))) #option 2
resultsSummary[(s*nrow(TurbineData)-(nrow(TurbineData)-t))*3-0,]<-c(CRSpecies[s], TurbineData$TurbineModel[t], 3, mean(rowSums(tab3)),sd(rowSums(tab3)), CV(mean(rowSums(tab3)),sd(rowSums(tab3))), median(rowSums(tab3)), IQR(rowSums(tab3))) #option 3
#resultsSummary[t*s*3-0,]<-c(CRSpecies[s], TurbineData$TurbineModel[t], 3, mean(rowSums(tab3)),sd(rowSums(tab3)), median(rowSums(tab3)), IQR(rowSums(tab3))) #option 3

###create summary tables of input parameters###

###BIRD PARAMETERS###
sampledBirdParamsSummary = data.frame(matrix(data = 0, ncol = 5, nrow = 7))
names(sampledBirdParamsSummary)=c("Parameter","Mean", "SD", "Median", "IQR")
sampledBirdParamsSummary[,1] = c("AvoidanceBasic", "AvoidanceExtended",  "WingSpan", "BodyLength", "PCH", "FlightSpeed", "NocturnalActivity")

##write summaries###
sampledBirdParamsSummary[1,2:5]=c(mean(sampledBirdParams$AvoidanceBasic), sd(sampledBirdParams$AvoidanceBasic), median(sampledBirdParams$AvoidanceBasic), IQR(sampledBirdParams$AvoidanceBasic))
sampledBirdParamsSummary[2,2:5]=c(mean(sampledBirdParams$AvoidanceExtended), sd(sampledBirdParams$AvoidanceExtended), median(sampledBirdParams$AvoidanceExtended), IQR(sampledBirdParams$AvoidanceExtended))
sampledBirdParamsSummary[3,2:5]=c(mean(sampledBirdParams$WingSpan), sd(sampledBirdParams$WingSpan), median(sampledBirdParams$WingSpan), IQR(sampledBirdParams$WingSpan))
sampledBirdParamsSummary[4,2:5]=c(mean(sampledBirdParams$BodyLength), sd(sampledBirdParams$BodyLength), median(sampledBirdParams$BodyLength), IQR(sampledBirdParams$BodyLength))
sampledBirdParamsSummary[5,2:5]=c(mean(sampledBirdParams$PCH), sd(sampledBirdParams$PCH), median(sampledBirdParams$PCH), IQR(sampledBirdParams$PCH))
sampledBirdParamsSummary[6,2:5]=c(mean(sampledBirdParams$FlightSpeed), sd(sampledBirdParams$FlightSpeed), median(sampledBirdParams$FlightSpeed), IQR(sampledBirdParams$FlightSpeed))
sampledBirdParamsSummary[7,2:5]=c(mean(sampledBirdParams$NocturnalActivity), sd(sampledBirdParams$NocturnalActivity), median(sampledBirdParams$NocturnalActivity), IQR(sampledBirdParams$NocturnalActivity))

###output parameter table###
fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"sampledBirdParameters.csv", sep="_")
write.csv (sampledBirdParamsSummary, paste(results_folder,"tables",  fileName, sep="\\"))


###TURBINE PARAMETERS###
sampledTurbineParamsSummary = data.frame(matrix(data = 0, ncol = 5, nrow = 17))
names(sampledTurbineParamsSummary)=c("Parameter","Mean", "SD", "Median", "IQR")
sampledTurbineParamsSummary[,1] = c("RotorSpeed", "RotorRadius", "HubHeight", "BladeWidth", "Pitch", "JanOp", "FebOp", "MarOp", "AprOp", "MayOp",   "JunOp", "JulOp",   "AugOp", "SepOp", "OctOp", "NovOp", "DecOp")

##write summaries###
sampledTurbineParamsSummary[1,2:5]=c(mean(sampledTurbine$RotorSpeed), sd(sampledTurbine$RotorSpeed), median(sampledTurbine$RotorSpeed), IQR(sampledTurbine$RotorSpeed))
sampledTurbineParamsSummary[2,2:5]=c(mean(sampledTurbine$RotorRadius), sd(sampledTurbine$RotorRadius), median(sampledTurbine$RotorRadius), IQR(sampledTurbine$RotorRadius))
sampledTurbineParamsSummary[3,2:5]=c(mean(sampledTurbine$HubHeight), sd(sampledTurbine$HubHeight), median(sampledTurbine$HubHeight), IQR(sampledTurbine$HubHeight))
sampledTurbineParamsSummary[4,2:5]=c(mean(sampledTurbine$BladeWidth), sd(sampledTurbine$BladeWidth), median(sampledTurbine$BladeWidth), IQR(sampledTurbine$BladeWidth))
sampledTurbineParamsSummary[5,2:5]=c(mean(sampledTurbine$Pitch), sd(sampledTurbine$Pitch), median(sampledTurbine$Pitch), IQR(sampledTurbine$Pitch))
sampledTurbineParamsSummary[6,2:5]=c(mean(sampledTurbine$JanOp), sd(sampledTurbine$JanOp), median(sampledTurbine$JanOp), IQR(sampledTurbine$JanOp))
sampledTurbineParamsSummary[7,2:5]=c(mean(sampledTurbine$FebOp), sd(sampledTurbine$FebOp), median(sampledTurbine$FebOp), IQR(sampledTurbine$FebOp))
sampledTurbineParamsSummary[8,2:5]=c(mean(sampledTurbine$MarOp), sd(sampledTurbine$MarOp), median(sampledTurbine$MarOp), IQR(sampledTurbine$MarOp))
sampledTurbineParamsSummary[9,2:5]=c(mean(sampledTurbine$AprOp), sd(sampledTurbine$AprOp), median(sampledTurbine$AprOp), IQR(sampledTurbine$AprOp))
sampledTurbineParamsSummary[10,2:5]=c(mean(sampledTurbine$MayOp), sd(sampledTurbine$MayOp), median(sampledTurbine$MayOp), IQR(sampledTurbine$MayOp))
sampledTurbineParamsSummary[11,2:5]=c(mean(sampledTurbine$JunOp), sd(sampledTurbine$JunOp), median(sampledTurbine$JunOp), IQR(sampledTurbine$JunOp))
sampledTurbineParamsSummary[12,2:5]=c(mean(sampledTurbine$JulOp), sd(sampledTurbine$JulOp), median(sampledTurbine$JulOp), IQR(sampledTurbine$JulOp))
sampledTurbineParamsSummary[13,2:5]=c(mean(sampledTurbine$AugOp), sd(sampledTurbine$AugOp), median(sampledTurbine$AugOp), IQR(sampledTurbine$AugOp))
sampledTurbineParamsSummary[14,2:5]=c(mean(sampledTurbine$SepOp), sd(sampledTurbine$SepOp), median(sampledTurbine$SepOp), IQR(sampledTurbine$SepOp))
sampledTurbineParamsSummary[15,2:5]=c(mean(sampledTurbine$OctOp), sd(sampledTurbine$OctOp), median(sampledTurbine$OctOp), IQR(sampledTurbine$OctOp))
sampledTurbineParamsSummary[16,2:5]=c(mean(sampledTurbine$NovOp), sd(sampledTurbine$NovOp), median(sampledTurbine$NovOp), IQR(sampledTurbine$NovOp))
sampledTurbineParamsSummary[17,2:5]=c(mean(sampledTurbine$DecOp), sd(sampledTurbine$DecOp), median(sampledTurbine$DecOp), IQR(sampledTurbine$DecOp))

###output parameter table###
fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"sampledTurbineParameters.csv", sep="_")
write.csv (sampledTurbineParamsSummary, paste(results_folder, "tables",fileName, sep="\\"))