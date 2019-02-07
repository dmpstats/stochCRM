# stochastic Band function ------------------------------------------------

#' distillation of the band master file to operate as a simple function
#' Author: CRD
#' Created: 12/11/2017
#' Args:
#'  - workingDirectory: parent directory - contains data and script directories
#'  
#'  - results_folder: output folder for tables/plots defaults to "results" in working directory
#'  
#'  - BirdDataFile: currently species-by-parameter CSV file, found in "data/BirdData.csv"
#'  -- [contains: Species AvoidanceBasic	AvoidanceBasicSD	
#'               AvoidanceExtended	AvoidanceExtendedSD	
#'               Body_Length	Body_LengthSD	
#'               Wingspan	WingspanSD	
#'               Flight_Speed	Flight_SpeedSD	
#'               Nocturnal_Activity	Nocturnal_ActivitySD	
#'               Flight	
#'               Prop_CRH_Obs	Prop_CRH_ObsSD]

#'  - TurbineDataFile: currently contains turbine model-by-parameter CSV, found in "data/TurbineData.csv"
#'  -- [contains: TurbineModel	Blades	
#'                RotationSpeed	RotationSpeedSD	RotorRadius	RotorRadiusSD	
#'                HubHeightAdd	HubHeightAddSD	BladeWidth	BladeWidthSD	
#'                Pitch	PitchSD	
#'                JanOp	JanOpMean	JanOpSD	
#'                FebOp	FebOpMean	FebOpSD ... etc to Dec]
#'                
#'  - CountDataFile: currently contains species-by-parameters CSV, found in "data/CountData.csv"  
#'  -- [contains: Species	Jan	JanSD	Feb	FebSD ... etc to Dec]
#'  
#'  - FlightDataFile: currently contains height-by-species CSV , found in "data/FlightHeight.csv" 
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
            CRSpecies = c("Northern_Gannet"),
            TPower = 1760, 
            LargeArrayCorrection = "yes",
            WFWidth = 54, 
            Prop_Upwind = 0.5,
            Latitude = 53.7,
            TideOff = 2.5, 
            windSpeedMean = 30,
            windSpeedSD = 0.000000001,
            updateProgress_Spec,  # pass in the updateProgress function so that it can update the progress indicator.
            updateProgress_Iter,
            DensityOpt
            ) {
    
# preamble ----------------------------------------------------------------
    
    # library msm - believe this is only for the truncated normal distrubtion
    library(msm)    
    library(dplyr)
    library(tidyr)
    library(pracma)
    library(data.table)

    ###start timers
    start.time <- Sys.time()
    
    ###set random number seed
    # set.seed(100)          ### BC CHANGE ### -- removing this as it was probably set for debugging purposes on the originbal Masden code - unclear to what would be the benefit of having identical runs on the online app
    S<-iter*20 ## this is number of samples, increased to ensure enough valid values - is this used?
    
    #### BC ##### -- initialise objects to store simulation replicates of monthly collisions, for each option, for current species and turbine  ===========
    monthCollsnReps_opt1 <- list()
    monthCollsnReps_opt2 <- list()
    monthCollsnReps_opt3 <- list()


# Create folders and paths ------------------------------------------------


    # ###create results folder
    # if(results_folder == "") results_folder<- Sys.Date() ## if no name given for results folder, use today's date
    # if(results_folder !="") dir.create(results_folder) ## if name given for results folder use that and create folder      #####    BC CHANGE  -- folder created beforehand by app  ######
    
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
    


# Prob collision functions and associated bits ----------------------------

    ##### create dataframe giving the width of the chord relative to its maximum width at given points along the radius of the rotor
    
    rad = round(seq(0,1,0.05),2) 
    
    circ = c(0.69,0.73,0.79,0.88,0.96,1,0.98,0.92,0.85,0.8,0.75,0.7,0.64,0.58,0.52,0.47,0.41,0.37,0.3,0.24,0)
    
    coverC = data.frame(cbind(rad, circ))
    
    ##### bring in call functions needed to calculate collision risk along blade
    
    source("scripts/PCollFunctions.r", local=T)
    
    
# calculations ------------------------------------------------------------
    
    ##set progress bar
    pb   <- txtProgressBar(1, iter*length(CRSpecies)*nrow(TurbineData), style=3)
    
    ###create overall results summary table###
    resultsSummary = data.frame(matrix(data = 0, ncol = 8, nrow = length(CRSpecies)*nrow(TurbineData)))
    names(resultsSummary) = c("Species", "Turbine", "Option", "Mean", "SD","CV", "Median", "IQR")
    
# Start of the species loop -----------------------------------------------    
    
    for (s in 1 : length (CRSpecies)){
      
      #### BC ##### -- progress bar update for iterations    ===========
      if (is.function(updateProgress_Spec)) {
        text <- gsub("_", " ", CRSpecies[s])
        updateProgress_Spec(value = s/(length(CRSpecies)), detail = text)
      }
      
      species.dat = subset (BirdData, Species == CRSpecies[s])
      
      species.dat$FlightNumeric <- ifelse(species.dat$Flight == 'Flapping', 1, 0)
      
  
      c_densOpt <- filter(DensityOpt, specLabel == CRSpecies[s])$userOption
      
      if(c_densOpt == "truncNorm"){
        species.count = subset(CountData, Species == CRSpecies[s])  
      }
      
      
      if(c_densOpt == "reSamp"){
        species.count <- fread("data/birdDensityData_samples.csv") %>%
          filter(specLabel == CRSpecies[s])
      }
      
      
      if(c_densOpt == "pcntiles"){
        species.count <- fread("data/birdDensityData_refPoints.csv") %>%
          filter(specLabel == CRSpecies[s])
      }
      
      
      
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
      
      #= sample bird parameters
      #' Not sure why this was within the t loop - moved
      
      source("scripts/samplebirdparams.r", local=T)

      
# Start of turbine loop ---------------------------------------------------


      for ( t in 1:nrow(TurbineData))  {
        #' Things indexed by t
        #' TurbineData
        
        ###CREATE TURBINE DATA FRAME###
        sampledTurbine = data.frame(matrix(data = 0, ncol = 18, nrow = iter))
        names(sampledTurbine) = c("RotorRadius", "HubHeight", "BladeWidth", "WindSpeed", "RotorSpeed", "Pitch", ### BC CHANGE ### -- windSpeed added
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
        
      
        # sample turbine pars based on their sampling dists
        #= samples from wind pars, then uses pitch/speed curves
        
        source("scripts/get_rotor_plus_pitch_auto.r", local=T)
        
        #= outputs large (size S) rotor speeds and pitch - sampled into the DF
        
        source("scripts/sampleturbineparams.r", local=T)
        
        MonthlyOperational <- sampledTurbine %>% select(contains("Op", ignore.case = F))
        
        MeanOperational <- apply(MonthlyOperational, 1, mean)
          
# Iterating i - over random samples  --------------------------------------        
  
      for (i in 1:iter){          
          #' things that are index by i in orginal
          #' MonthlyOperational
          #' sampledTurbine
          #' sampledBirdParams
          #' sampledSpeciesCount
          
        #### BC ##### -- progress bar update for iterations    ===========
        if (is.function(updateProgress_Iter)) {
          text <- NULL # paste0("Working through iteration ", i)
          updateProgress_Iter(value = i/iter, detail = text)
        }
        
        
        # following are required to speed up pcoll function - need single numeric inputs for speed        
        # coverC
        currentRad <- coverC$rad
        currentCirc <- coverC$circ
        
        # fixed turbine pars
        currentBlades <- TurbineData$Blades
        
        # sampled turbine pars
        currentRotorRadius <- sampledTurbine$RotorRadius[i]
        currentBladeWidth <- sampledTurbine$BladeWidth[i]
        currentRotorSpeed <- sampledTurbine$RotorSpeed[i]
        currentPitch <- sampledTurbine$Pitch[i]
        
        # fixed bird parameter
        currentFlightNumeric <- species.dat$FlightNumeric
        
        # sampled bird parameters
        currentWingSpan <- sampledBirdParams$WingSpan[i]
        currentFlightSpeed <- sampledBirdParams$FlightSpeed[i]
        currentBirdLength <- sampledBirdParams$BodyLength[i]
            

# Collision risk steps - options appear here ------------------------------


      ############## STEP ONE - Calculate the collision risk in the absence of avoidance action
          
          source("scripts/ProbabilityCollision.r", local=T)
          
          
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
          

          CollRiskSinglePassage = NTurbines * (pi * sampledTurbine$RotorRadius[i]^2)/(2 * sampledTurbine$RotorRadius[i] * WFWidth * 1000) * 
            (P_Collision/100) * (MeanOperational[i]/100) * (1-sampledBirdParams$AvoidanceBasic[i])
          
          L_ArrayCF = 1 - (NTurbRows - 1) / (2*NTurbRows) * CollRiskSinglePassage + 
            (NTurbRows - 1) * (2*NTurbRows)/(6 * NTurbRows^2) * (CollRiskSinglePassage ^2)
          
          

# Option 1 ----------------------------------------------------------------

          
          #######################		Do model using option 1 - Site specific flight height information	###############################
          
          source("scripts/Option1.r", local=T)
          
          ## add results to overall species/turbine results table
          tab1[i,]=Option1_CollisionRate[,2]
          
          #store P_Coll
          sampledPColl[i,]<-P_Collision/100
          

# Option 2 ----------------------------------------------------------------

         
          #######################		Do model using option 2 - modelled flight height distribution		###############################
          

          
          source("scripts/Option2.r", local=T)
          ## add results to overall species/turbine results table
          tab2[i,]=Option2_CollisionRate[,2]
          

# Option 3 ----------------------------------------------------------------

        
          #######################		Do model using option 3 - modelled flight height distribution		###############################
          #######################		taking account of variation in risk along the rotor blades		###############################
          
          source("scripts/Option3.r", local=T)
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
        
        #### BC ##### -- reset counter of progress bar for iterations =====================
        if (is.function(updateProgress_Iter)) {
          text <- NULL # paste0("Working through iteration ", i)
          updateProgress_Iter(value = 0, detail = text)
        }
        
        
        #### BC ##### -- Store simulation replicates under each option, for current species and turbine  ===========
        cSpec <- CRSpecies[s]
        cTurbModel <- paste0("turbModel", TurbineData$TurbineModel[t])
        
        monthCollsnReps_opt1[[cSpec]][[cTurbModel]] <- tab1
        monthCollsnReps_opt2[[cSpec]][[cTurbModel]] <- tab2
        monthCollsnReps_opt3[[cSpec]][[cTurbModel]] <- tab3
        
      } # end of t over number of turbine
      

# End of the turbine loop -------------------------------------------------


      
      ###output species plots of density by option with curves for turbine model###
      ###PLOT DENSITY BY OPTION (useful if several turbine models)###
      
      if (nrow(TurbineData)>1)  {
       
        source("scripts/species_turbine_plots.r", local = T) 
       
      }
      
      ###relabel sampledBirdParams by species name###
      assign(paste(CRSpecies[s],"params", sep="_"), sampledBirdParams)
      
      ###relabel sampledSpeciesCount by species name###
      assign(paste(CRSpecies[s],"counts", sep="_"), sampledSpeciesCount)
      
      
    } # end of the species loop over s
    
    ##output input data##
    fwrite(BirdData, paste(results_folder,"input", "BirdData.csv", sep="/"))
    fwrite(CountData, paste(results_folder,"input", "birdDensityData.csv", sep="/"))      # <<<<< BC <<<<<  change of file name, for clarity
    fwrite(TurbineData, paste(results_folder,"input", "TurbineData.csv", sep="/"))
    
    ###output results table###
    fwrite(resultsSummary, paste(results_folder,"tables", "CollisionEstimates.csv", sep="/"))
    
    
    end.time <- Sys.time()
    run.time <- end.time - start.time
    run.time
    
    sink(paste(results_folder,"run.time.txt", sep="/"))
    print(run.time)
    print(paste("The model ran", iter,"iterations", sep=" "))
    print("The following species were modelled:")
    print(CRSpecies)
    print("The following turbines were modelled:")
    print(TurbineData$TurbineModel)
    sink()
    
    #### BC ##### -- return collision replicates as output  ===========
    return(list(monthCollsnReps_opt1 = monthCollsnReps_opt1, monthCollsnReps_opt2 = monthCollsnReps_opt2, 
                monthCollsnReps_opt3 = monthCollsnReps_opt3))
        
  }

