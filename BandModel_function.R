# stochastic Band function ------------------------------------------------

#' distillation of the band master file to operate as a simple function
#' Author: CRD
#' Created: 12/11/2017
#' Args:
#'  - workingDirectory: parent directory - contains data and script directories
#'  
#'  - results_folder: output folder for tables/plots defaults to "results" in working directory
#'  
#'  - BirdDataFile: currently species-by-parameter CSV file, found in "data\\BirdData.csv"
#'  -- [contains: Species AvoidanceBasic	AvoidanceBasicSD	
#'               AvoidanceExtended	AvoidanceExtendedSD	
#'               Body_Length	Body_LengthSD	
#'               Wingspan	WingspanSD	
#'               Flight_Speed	Flight_SpeedSD	
#'               Nocturnal_Activity	Nocturnal_ActivitySD	
#'               Flight	
#'               Prop_CRH_Obs	Prop_CRH_ObsSD]

#'  - TurbineDataFile: currently contains turbine model-by-parameter CSV, found in "data\\TurbineData.csv"
#'  -- [contains: TurbineModel	Blades	
#'                RotationSpeed	RotationSpeedSD	RotorRadius	RotorRadiusSD	
#'                HubHeightAdd	HubHeightAddSD	BladeWidth	BladeWidthSD	
#'                Pitch	PitchSD	
#'                JanOp	JanOpMean	JanOpSD	
#'                FebOp	FebOpMean	FebOpSD ... etc to Dec]
#'                
#'  - CountDataFile: currently contains species-by-parameters CSV, found in "data\\CountData.csv"  
#'  -- [contains: Species	Jan	JanSD	Feb	FebSD ... etc to Dec]
#'  
#'  - FlightDataFile: currently contains height-by-species CSV , found in "data\\FlightHeight.csv" 
#'  -- [contains: Height (300 values) Shag	Little_Gull ... etc ]
#'  
#'  - iter: integer constant > 0. number of iterations - the number of stochastic draws to take
#'  
#'  - CRSpecies: character 'vector' of species to consider, looks to match/define other species name, so must match throughout
#'  -- [eg.g contains c("Black_legged_Kittiwake", "Northern_Gannet"... ]
#'  
#'  - TPower: constant. How much power will the wind farm generate (MW)
#'  
#'  - LargeArrayCorrection: character ["yes" or "no"]
#'  
#'  - WFWidth: constant. "width" of wind farm used in Large Array Correction (units?)
#'  
#'  - Prop_Upwind: constant, ought to be 0-1 bounded as roportion of flights upwind - default of 0.5.
#'  
#'  - Latitude: numeric. Decimal latitude.
#'  
#'  - TideOff: Numeric constant. Tidal offset in metres.
#'  
#'  - windSpeedMean: Numeric constant. Site specific mean wind speed (units?). 
#'  
#'  - windSpeedSD: Constant. Site specific standard deviation of wind speeds.


# Start of function -------------------------------------------------------

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
            windSpeedSD = 3.2) {
  
# preamble ----------------------------------------------------------------
    
    # library msm - believe this is only for the truncated normal distrubtion
    library(msm)    
    library(dplyr)
    library(tidyr)

    ###start timers
    start.time <- Sys.time()
    
    ###set random number seed
    set.seed(100)
    S<-iter*20 ## this is number of samples, increased to ensure enough valid values - is this used?
    


# Create folders and paths ------------------------------------------------


    ###create results folder
    if(results_folder == "") results_folder<- Sys.Date() ## if no name given for results folder, use today's date
    if(results_folder !="") dir.create(results_folder) ## if name given for results folder use that and create folder
    
    ##make input, figures and tables folders
    dir.create(paste(results_folder, "figures", sep="/"))
    dir.create(paste(results_folder, "tables", sep="/"))
    dir.create(paste(results_folder, "input", sep="/"))
    
    # Masden use this a lot - just make once
    monthLabels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",  "Aug", "Sep", "Oct", "Nov", "Dec")


# read data sources -------------------------------------------------------

    ### Read in Distance corrected count data for each species, bird biometric data, flight height distributions and turbine characteristics
    
    source("scripts/helpers_read data.r", local=T)
    
  
    #### produces a data frame with number of hours daylight and night per month
    
    source("scripts/DayLength.r", local=T) 

# load sampling functions for stochastic bits -----------------------------

    source("scripts/helpers_sampling functions.r", local=T)
    

# calculations ------------------------------------------------------------
    
    ##set progress bar
    pb   <- txtProgressBar(1, iter*length(CRSpecies)*nrow(TurbineData), style=3)
    
    ###create overall results summary table###
    resultsSummary = data.frame(matrix(data = 0, ncol = 8, nrow = length(CRSpecies)*nrow(TurbineData)))
    names(resultsSummary) = c("Species", "Turbine", "Option", "Mean", "SD","CV", "Median", "IQR")
    
# Start of the species loop -----------------------------------------------    
    
    for (s in 1 : length (CRSpecies)){
      
      species.dat = subset (BirdData, Species == CRSpecies[s])
      species.count = subset (CountData, Species == CRSpecies[s])
      
      Flap_Glide = ifelse (species.dat$Flight == "Flapping", 1, 2/pi)
      
      ##input flight curves for the species
   
      ht<-paste("data/", CRSpecies[s],"_ht.csv", sep='')
      
      FlightHeightSpec <- read.csv(ht, header = T) #and change in option2 code
      
      flight.boot <- 1:dim(FlightHeightSpec)[2]
      
      flight.boot.sample <- sample(flight.boot, iter, replace=T)
      
      
      
      
      ###CREATE BIRD PARAMETER DATA FRAME###
      sampledBirdParams = data.frame(matrix(data = 0, ncol = 7, nrow = iter))
      names(sampledBirdParams) = c("AvoidanceBasic", "AvoidanceExtended",  "WingSpan", "BodyLength", "PCH", "FlightSpeed", "NocturnalActivity")
      
      ###CREATE COUNT/DENSITY DATA FRAME###
      sampledSpeciesCount = data.frame(matrix(data = 0, ncol = 12, nrow = iter))
      names(sampledSpeciesCount) = monthLabels
      
      ###CRAETE DATA FRAME FOR DENSITY DATA###
      densitySummary=data.frame(matrix(data = 0, ncol = nrow(TurbineData)*3, nrow = iter))
      ##add names of columns later in turbine loop###
      

# Start of turbine loop ---------------------------------------------------


      for ( t in 1:nrow(TurbineData))  {
        
        
        ###CREATE TURBINE DATA FRAME###
        sampledTurbine = data.frame(matrix(data = 0, ncol = 17, nrow = iter))
        names(sampledTurbine) = c("RotorSpeed", "RotorRadius", "HubHeight", "BladeWidth", "Pitch", 
                                  "JanOp", "FebOp", "MarOp", "AprOp", "MayOp",   "JunOp", "JulOp",   
                                  "AugOp", "SepOp", "OctOp", "NovOp", "DecOp")
        
        
        ## create results tables - 3 identical
        
        tab1 <- data.frame(matrix(data = 0, ncol = 12, nrow = iter))
        names(tab1) <- monthLabels
        
        tab2 <- tab3 <- tab1
        
        
        
        ###set vectors to store PCol and CollInt###
        sampledPColl <- data.frame(matrix(data = 0, ncol = 1, nrow = iter))
        names(sampledPColl) <- "PColl"
        
        sampledCollInt <- data.frame(matrix(data = 0, ncol = 1, nrow = iter))
        names(sampledCollInt) <- "CollInt"
        
        #= sample bird parameters
        
          source("scripts\\samplebirdparams.r", local=T)
          
# Iterating i - over random samples  --------------------------------------        
  
      for (i in 1:iter){                    
          
          # sample turbine pars based on their sampling dists
          #= samples from wind pars, then uses pitch/speed curves
        
          source("scripts\\get_rotor_plus_pitch_auto.r", local=T)
        
          #= outputs large (size S) rotor speeds and pitch - sampled into the DF
        
          source("scripts\\sampleturbineparams.r", local=T)
        
          MonthlyOperational <- sampledTurbine %>% select(contains("Op", ignore.case = F))
        
          MeanOperational <- apply(MonthlyOperational, 1, mean)
          

          #######################################
          ######Start collision Risk model#######
          #######################################
          
          ############## STEP ONE - Calculate the collision risk in the absence of avoidance action
          
          source("scripts\\ProbabilityCollision.r", local=T)
          
          
          ############## STEP TWO - Calculate Flux Factor - the number of birds passing a turbine in each month
        
          
          
          ## First calculate turbine frontal area
          
          NTurbines = round (TPower / TurbineData$TurbineModel[t]) ### Number of turbines of given Output required to produce target output
          TotalFrontalArea = NTurbines * pi * sampledTurbine$RotorRadius[i] ^2
          
          #### Calculate the total number of birds passing through the wind farm in each month
          
          
          for (h in 1:nrow(hours)) { 
            
            hours$Flux[h] = sampledBirdParams$FlightSpeed[i] * sampledSpeciesCount[i, h]/ (2 * sampledTurbine$RotorRadius[i]) * TotalFrontalArea *
              (hours$Day[h] + sampledBirdParams$NocturnalActivity[i] * hours$Night[h]) * 3600/1000000
            
          }
          
          
          ############## STEP THREE - Calculate Large Array Correction Factor
          
          ## calculate number of turbine rows - manually enter if appropriate
          
          NTurbRows = NTurbines ^ 0.5
          

          
          # MeanOperational = mean(c(sampledJanOp[i],sampledFebOp[i],sampledMarOp[i],sampledAprOp[i],
          #                          sampledMayOp[i],sampledJunOp[i],sampledJulOp[i],sampledAugOp[i],
          #                          sampledSepOp[i],sampledOctOp[i],sampledNovOp[i],sampledDecOp[i]))
          # 
          
          CollRiskSinglePassage = NTurbines * (pi * sampledTurbine$RotorRadius[i]^2)/(2 * sampledTurbine$RotorRadius[i] * WFWidth * 1000) * 
            (P_Collision/100) * (MeanOperational[i]/100) * (1-sampledBirdParams$AvoidanceBasic[i])
          
          L_ArrayCF = 1 - (NTurbRows - 1) / (2*NTurbRows) * CollRiskSinglePassage + 
            (NTurbRows - 1) * (2*NTurbRows)/(6 * NTurbRows^2) * (CollRiskSinglePassage ^2)
          
          
          
          #######################		Do model using option 1 - Site specific flight height information	###############################
          
          source("scripts\\Option1.r", local=T)
          
          ## add results to overall species/turbine results table
          tab1[i,]=Option1_CollisionRate[,2]
          
          #store P_Coll
          sampledPColl[i,]<-P_Collision/100
          
          
          #######################		Do model using option 2 - modelled flight height distribution		###############################
          
          source("scripts\\Option2.r", local=T)
          ## add results to overall species/turbine results table
          tab2[i,]=Option2_CollisionRate[,2]
          
          
          #######################		Do model using option 3 - modelled flight height distribution		###############################
          #######################		taking account of variation in risk along the rotor blades		###############################
          
          source("scripts\\Option3.r", local=T)
          ## add results to overall species/turbine results table
          tab3[i,]=Option3_CollisionRate[,2]  
          
          #Store Collision Integral
          sampledCollInt[i,]<-CollInt
          
          ##progress bar for iterations##
          #setTxtProgressBar(pb, s*t+i)
          setTxtProgressBar(pb, (s*nrow(TurbineData)-(nrow(TurbineData)-t))*iter-(iter-i))
          
          
      } # end of i to iter
        
# End of the random samplling iterations i --------------------------------

      source("scripts/turbineSpeciesOuputs.r", local=T)
        
      } # end of t over number of turbine
      

# End of the turbine loop -------------------------------------------------


      
      ###output species plots of density by option with curves for turbine model###
      ###PLOT DENSITY BY OPTION (useful if several turbine models)###
      
      if (nrow(TurbineData)>1)  {
        
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
        png(paste(results_folder, "figures", fileName, sep="\\"),width=500,height=900,res=100)
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
      }
      
      ###relabel sampledBirdParams by species name###
      assign(paste(CRSpecies[s],"params", sep="_"), sampledBirdParams)
      
      ###relabel sampledSpeciesCount by species name###
      assign(paste(CRSpecies[s],"counts", sep="_"), sampledSpeciesCount)
      
      
    } # end of the species loop over s
    
    ##output input data##
    write.csv(BirdData, paste(results_folder,"input", "BirdData.csv", sep="\\"))
    write.csv(CountData, paste(results_folder,"input", "CountData.csv", sep="\\"))
    write.csv(TurbineData, paste(results_folder,"input", "TurbineData.csv", sep="\\"))
    
    ###output results table###
    write.csv (resultsSummary, paste(results_folder,"tables", "CollisionEstimates.csv", sep="\\"))
    
    
    end.time <- Sys.time()
    run.time <- end.time - start.time
    run.time
    
    sink(paste(results_folder,"run.time.txt", sep="\\"))
    print(run.time)
    print(paste("The model ran", iter,"iterations", sep=" "))
    print("The following species were modelled:")
    print(CRSpecies)
    print("The following turbines were modelled:")
    print(TurbineData$TurbineModel)
    sink()
    
    
        
  }

