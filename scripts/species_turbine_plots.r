option1<-numeric()
option2<-numeric()
option3<-numeric()
max1<-0
max2<-0
max3<-0
max1y<-0
max2y<-0
max3y<-0

for (j in 1:nrow(TurbineData)){
  option1<-cbind(option1,densitySummary[,(j-1)*3+1])
  ifelse (max(density(densitySummary[,(j-1)*3+1])$x)>max1,max1<-max(density(densitySummary[,(j-1)*3+1])$x),max1)
  ifelse (max(density(densitySummary[,(j-1)*3+1])$y)>max1y,max1y<-max(density(densitySummary[,(j-1)*3+1])$y),max1y)
  option2<-cbind(option2,densitySummary[,(j-1)*3+2])
  ifelse (max(density(densitySummary[,(j-1)*3+2])$x)>max2,max2<-max(density(densitySummary[,(j-1)*3+2])$x),max2)
  ifelse (max(density(densitySummary[,(j-1)*3+2])$y)>max2y,max2y<-max(density(densitySummary[,(j-1)*3+2])$y),max2y)
  option3<-cbind(option3,densitySummary[,(j-1)*3+3])
  ifelse (max(density(densitySummary[,(j-1)*3+3])$x)>max3,max3<-max(density(densitySummary[,(j-1)*3+3])$x),max3)
  ifelse (max(density(densitySummary[,(j-1)*3+3])$y)>max3y,max3y<-max(density(densitySummary[,(j-1)*3+3])$y),max3y)
}

fileName<-CRSpecies[s]
fileName<-paste(fileName, ".png", sep="")
png(paste(results_folder, "figures", fileName, sep="/"),width=500,height=900,res=100)
par(mfrow = c(3, 1))

plot(density(option1[,1]), main="Option 1", xlab="Number of Collisions", ylab ="Probability Density", xlim=c(0,max1), ylim=(c(0, max1y)))
names<-paste("Turbine model",TurbineData$TurbineModel[1], sep=":")
for (i in 2:nrow(TurbineData)){
  lines(density(option1[,i]), lty=i)
  names<-c(names,paste("Turbine model",TurbineData$TurbineModel[i], sep=":"))
}
legend("topright", names, cex=0.8, lty=1:nrow(TurbineData))

plot(density(option2[,1]), main="Option 2", xlab="Number of Collisions", ylab ="Probability Density", xlim=c(0,max2), ylim=(c(0, max2y)))
names<-paste("Turbine model",TurbineData$TurbineModel[1], sep=":")
for (i in 2:nrow(TurbineData)){
  lines(density(option2[,i]), lty=i)
  names<-c(names,paste("Turbine model",TurbineData$TurbineModel[i], sep=":"))
}
legend("topright", names, cex=0.8, lty=1:nrow(TurbineData))

plot(density(option3[,1]), main="Option 3", xlab="Number of Collisions", ylab ="Probability Density",xlim=c(0,max3), ylim=(c(0, max3y)))
names<-paste("Turbine model",TurbineData$TurbineModel[1], sep=":")
for (i in 2:nrow(TurbineData)){
  lines(density(option3[,i]), lty=i)
  names<-c(names,paste("Turbine model",TurbineData$TurbineModel[i], sep=":"))
}
legend("topright", names, cex=0.8, lty=1:nrow(TurbineData))

dev.off()