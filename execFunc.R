setwd("C:/Users/ADMIN/Desktop/projo/ineos")


tool_exec = function(in_params, out_params) {
  
  # Load required packages
  arc.progress_label('Loading required R packages...')
  arc.progress_pos(25)
  pkgs = c('wakefield', 'MASS', 'factoextra', 'clue', 'dplyr')
  load_pkgs(pkgs)
  
  
  # Get parameters - INPUT and specify OUTPUT
  # source_data <- in_params[[1]]   ###Dataset to run inputs against
  # 
  # #***Recruitment Input
  # recruitsAge <- as.integer(in_params[[2]])
  # employmentStatus <-  in_params[[3]]
  # terrorGroup <- in_params[[4]]
  # recruitCountry <- in_params[[5]]
  # recruitTown <- in_params[[6]]
  # recruitmentTimeSpan <- in_params[[7]]
  # 
  # #*****Planning Input 
  # planMeetingsCountry <- in_params[[8]]
  # planMeetingsCity <- in_params[[9]]
  # planTrainingCountry <- in_params[[10]]
  # planTrainingCity <- in_params[[11]]
  # planningTimeSpan <- in_params[[12]]  
  # 
  # ##****Preparatory Input
  # prepWeapons <- in_params[[13]]
  # prepFunds <- in_params[[14]]
  # prepIllegalImmigration <- in_params[[15]]
  # prepSurveillance <- in_params[[16]]
  # prepTimeSpan <- in_params[[17]]
  # 
  # ## Output
  # ##STATISTICAL RESULTS eg.ANOVA etc
  # predictionTable <- out_params[[1]] ##Discriminant Analysis
  # # predictionTable <- out_params[[1]] ##KMEANS
  # predictionPercentage <- out_params[[2]]
  # estimateAreaOfFocus <- out_params[[3]]  ## if time allows
  
  
  
  # Import data set to data frame
  arc.progress_label('Reading data...')
  arc.progress_pos(25)
  data = arc.open(source_data)
  data_df = arc.select(data, fields = '*')
  data_df[, 1] <- NULL
  View(data_df)
 
  # x <- c("Kenya", "Tanzania", "Uganda", "US", "Britain")
  # w <- c("Nairobi", "Mombasa","Kampala", "Mbarara","Mwanza", "Dodoma")
  # y <- c("ISIS", "Taliba","Al-shabaab","Boko Haram","AlQaeda")
  #***Recruitment Input
  recruitsAge <- 25
  employmentStatus <-  TRUE
  terrorGroup <- "fdgdfg"
  recruitCountry <- "Kefdgdnya"
  recruitTown <- "Nairdodfgdfgbi"
  recruitmentTimeSpan <- 20
  
  #*****Planning Input 
  planMeetingsCountry <- "Idfgdftaly"
  planMeetingsCity <- "Nairdgdfobi"
  planTrainingCountry <- "Kdfgenya"
  planTrainingCity <- "Boudgfra"
  planningTimeSpan <- 10
  
  ##****Preparatory Input
  prepWeapons <- FALSE
  prepFunds <- FALSE
  prepIllegalImmigration <- FALSE
  prepSurveillance <- FALSE
  prepTimeSpan <- 17
  
  ##* My own logic to determine level of Risk in an area ---as guided by research
  ##* eg. IF age > 30 THEN Risk is High, if Org is A and country is N, then regions X, Y, Z can suffer
  ##* 
  ##RECRUITMENT DETAILS LOGIC
  
  #1. AGE
      if(!(recruitsAge <= 0)) {
        if(recruitsAge > 15 && recruitsAge <= 25)
        {
          ageRiskVal <- "H"
        }else if(recruitsAge > 25)
        {
          ageRiskVal <- "M"
        }else {
          ageRiskVal <- "L"
        }
      }
    ageRiskVal    
      
  #2. EMPLOYMENT STATUS   
      if(employmentStatus == TRUE)
      {
        empRiskVal <- "L"
      }else{
        empRiskVal <- "H"
      }
      empRiskVal
      
   #3. COUNTRY AND TOWN   
      countryUnique <- unique(data_df$Country)
      cityUnique <- unique(data_df$Town)
      # countryUnique <- unique(x)
      # cityUnique <- unique(w)
      testFlag <- TRUE
      for (country in countryUnique)
      {
        if(country == recruitCountry)
        {
          for(town in cityUnique)
          {
            if (town == recruitTown)
            {
              recruitLocationRiskVal <- "H"
              testFlag <- FALSE
              break
            }else
            {
              if(testFlag == FALSE){
                # print("Do nothing")
              }
              else{
                recruitLocationRiskVal <- "M"
                testFlag = FALSE
              }
              break
            }
          }
        }
        else
        {
          if(testFlag == FALSE){
            #print("Do nothing")
          }
          else
          {
            recruitLocationRiskVal <- "L"
          }
         
        }
      }
      recruitLocationRiskVal
      
   
  #.4 NAME OF TERROR ORGANIZATION   
      terrorGroupUnique <- unique(data_df$Terrorist_Org)
      
      for(group in terrorGroupUnique)
      {
        if(group == terrorGroup )
        {
          orgRiskVal <- "H"
        }
        else{
          orgRiskVal <- "M"
        }
      }
      
      orgRiskVal
  #.5 TIME SPAN OF DETECTION   
      #**Recruitment Time span
      if(recruitmentTimeSpan >= 0)
      {
        if(recruitmentTimeSpan > 5 && recruitmentTimeSpan <= 15)
        {
          recruitTimeRiskVal <- "L"
        } else if(recruitmentTimeSpan > 15 && recruitmentTimeSpan < 20)
        {
          recruitTimeRiskVal <- "M"
        } else
        {
          recruitTimeRiskVal <- "H"
        }
      }
      
      recruitTimeRiskVal
   
  #** LOGIC TO COMBINE THE H, L and M values   
      combinedRiskValue = c(ageRiskVal, empRiskVal, orgRiskVal, recruitLocationRiskVal, recruitTimeRiskVal)
      countH <- 0
      countM <- 0
      countL <- 0
      combinedRiskValue
      for (item in combinedRiskValue)
      {
        
        if(item == "H")
        {
          countH <- countH + 1
        }
        else if (item == "M")
        {
          countM <- countM + 1
        }
        else if(item == "L")
        {
          countL <- countL + 1
        }
        else {
          print("Empty  field")
        }
        
      }
      
      countH 
      countM 
      countL
      
      if(countH >= countM)
      {
        if(countH >= countL)
        {
          finalRiskLevel <- "H"
          finalRiskValue <- runif(1, 6.0, 10.0)
        }  else
        {
          finalRiskLevel <- "L"
          finalRiskValue <- runif(1, 0.0, 4.0)
        }
      } else if (countM >= countL)
        
      {
        finalRiskLevel <- "M"
        finalRiskValue <- runif(1, 4.1, 5.9)
      } else
      {
        finalRiskLevel <- "L"
        finalRiskValue <- runif(1, 0.0, 4.0)
      }
      
      finalRiskLevel
      finalRiskValue
 
      
##PLANNING DETAILS LOGIC   
    #MEETINGS
      #1. COUNTRY AND TOWN   
      countryUnique <- unique(data_df$Country)
      cityUnique <- unique(data_df$Town)
      # countryUnique <- unique(x)
      # cityUnique <- unique(w)
      testFlag <- TRUE
      for (country in countryUnique)
      {
        if(country == planMeetingsCountry)
        {
          for(town in cityUnique)
          {
            if (town == planMeetingsCity)
            {
             meetingLocationRiskVal <- "H"
              testFlag <- FALSE
              break
            }else
            {
              if(testFlag == FALSE){
                # print("Do nothing")
              }
              else{
                meetingLocationRiskVal <- "M"
                testFlag = FALSE
              }
              break
            }
          }
        }
        else
        {
          if(testFlag == FALSE){
            #print("Do nothing")
          }
          else
          {
            meetingLocationRiskVal <- "L"
          }
          
        }
      }
      meetingLocationRiskVal
      
    ##2. TRAINING
      for (country in countryUnique)
      {
        if(country == planTrainingCountry)
        {
          for(town in cityUnique)
          {
            if (town == planTrainingCity)
            {
              trainingLocationRiskVal <- "H"
              testFlag <- FALSE
              break
            }else
            {
              if(testFlag == FALSE){
                # print("Do nothing")
              }
              else{
                trainingLocationRiskVal <- "M"
                testFlag = FALSE
              }
              break
            }
          }
        }
        else
        {
          if(testFlag == FALSE){
            #print("Do nothing")
          }
          else
          {
            trainingLocationRiskVal <- "L"
          }
          
        }
      }
      trainingLocationRiskVal
      
      #.3 TIME SPAN OF DETECTION   
      #**Recruitment Time span
      if(planningTimeSpan >= 0)
      {
        if(planningTimeSpan > 5 && planningTimeSpan <= 15)
        {
          planningTimeRiskVal <- "L"
        } else if(planningTimeSpan > 15 && planningTimeSpan < 20)
        {
          planningTimeRiskVal <- "M"
        } else
        {
          planningTimeRiskVal <- "H"
        }
      }
      
      planningTimeRiskVal
    
      ##Combining the final PlanningLogic  
      combinedPlanRiskVal <- c(meetingLocationRiskVal, trainingLocationRiskVal, planningTimeRiskVal)
      planCountH <- 0
      planCountM <- 0
      planCountL <- 0
      
      combinedPlanRiskVal
      for (item in combinedPlanRiskVal)
      {
        
        if(item == "H")
        {
          planCountH <- planCountH + 1
        }
        else if (item == "M")
        {
          planCountM <- planCountM + 1
        }
        else if(item == "L")
        {
          planCountL <- planCountL + 1
        }
        else {
          print("Empty Country field")
        }
        
      }
      
      planCountH 
      planCountM 
      planCountL
      
      if(planCountH >= planCountM)
      {
        if(planCountH >= planCountL)
        {
          finalPlanRiskLevel <- "H"
          finalPlanRiskValue <- runif(1, 6.0, 10.0)
        }  else
        {
          finalPlanRiskLevel <- "L"
          finalPlanRiskValue <- runif(1, 0.0, 4.0)
        }
      } else if (planCountM >= planCountL)
        
      {
        finalPlanRiskLevel <- "M"
        finalPlanRiskValue <- runif(1, 4.1, 5.9)
      } else
      {
        finalPlanRiskLevel <- "L"
        finalPlanRiskValue <- runif(1, 0.0, 4.0)
      }
      
      finalPlanRiskLevel
      finalPlanRiskValue
    
##3.PREPARATORY DETAILS LOGIC       
    #1. Weapons movement and purchases   
      if(prepWeapons == TRUE)
      {
        weaponsRiskVal <- "H"
      }else{
        weaponsRiskVal <- "M"
      }
      weaponsRiskVal
      
    #.2 Funds
      if(prepFunds == TRUE)
      {
        fundsRiskVal <- "H"
      }else {
        fundsRiskVal <- "L"
      }
      fundsRiskVal
      
  #.3 Illegal Immigration crime
      if (prepIllegalImmigration == TRUE)
      {
        immigrationRiskVal <- "H"
      }else {
        immigrationRiskVal <- "M"
      }
      immigrationRiskVal
  #.4 Surveillance
      
      if(prepSurveillance == TRUE)
      {
        surveillanceRiskVal <- "H"
      } else {
        surveillanceRiskVal <- "L"
      }
      
      surveillanceRiskVal
  #.5 Preparation TimeSpan
      if(prepTimeSpan >= 0)
      {
        if(prepTimeSpan > 5 && prepTimeSpan <= 15)
        {
          prepTimeRiskVal <- "L"
        } else if(prepTimeSpan > 15 && prepTimeSpan < 20)
        {
          prepTimeRiskVal <- "M"
        } else
        {
          prepTimeRiskVal <- "H"
        }
      }
      
      prepTimeRiskVal
      
  ##. Combining all together
      combinedPrepRiskValue = c(weaponsRiskVal, fundsRiskVal, immigrationRiskVal, surveillanceRiskVal, prepTimeRiskVal)
      prepCountH <- 0
      prepCountM <- 0
      prepCountL <- 0
      combinedPrepRiskValue
      for (item in combinedPrepRiskValue)
      {
        
        if(item == "H")
        {
          prepCountH <- prepCountH + 1
        }
        else if (item == "M")
        {
          prepCountM <- prepCountM + 1
        }
        else if(item == "L")
        {
          prepCountL <- prepCountL + 1
        }
        else {
          print("Empty Country field")
        }
        
      }
      
      prepCountH 
      prepCountM 
      prepCountL
      
      if(prepCountH >= prepCountM)
      {
        if(prepCountH >= prepCountL)
        {
          finalPrepRiskLevel <- "H"
          finalPrepRiskValue <- runif(1, 6.0, 10.0)
        }  else
        {
          finalPrepRiskLevel <- "L"
          finalPrepRiskValue <- runif(1, 0.0, 4.0)
        }
      } else if (prepCountM >= prepCountL)
        
      {
        finalPrepRiskLevel <- "M"
        finalPrepRiskValue <- runif(1, 4.1, 5.9)
      } else
      {
        finalPrepRiskLevel <- "L"
        finalPrepRiskValue <- runif(1, 0.0, 4.0)
      }
      
      finalPrepRiskLevel
      finalPrepRiskValue
      
      
  ###KMEans Logic
  #* Extract only fields needed for clustering
  testData <- data_df[, 2:5]
  dataToCluster <- data_df[, 2:4] 
  clusteredData <- kmeans(dataToCluster, 2)
  
  clusteredData
  clusteredData$cluster
  clusteredData$centers
  
  clusteredData$size
  
  clusteredTable <- table(clusteredData$cluster, testData$Terror_Event)   ##########One of our outputs
  
  rowNames <- rownames(clusteredTable)
  rowNames[1]
  rowNames[2]
  
  clusteredTable <- as.data.frame(clusteredTable)
  row.names(clusteredTable) <-c()
  
  View(clusteredTable) 
  # clusteredTable[,-1]
  # clusteredTable
  # clusteredTable <-  cbind(clusteredTable,Cluster=rowNames)
  
  clusteredTable

  if (!is.null(predictionKMeansTable) && predictionKMeansTable != 'NA') {
    arc.write(predictionKMeansTable, clusteredTable)
  }
  
  
  fviz_cluster(clusteredData, data = dataToCluster)
  fviz_cluster(clusteredData, data = dataToCluster,
               palette = c("#00AFBB", "red", "#E7B800"), 
               geom = "point",
               ellipse.type = "convex",  
               ggtheme = theme_bw()
  )
  
  
  #* Discriminant Analysis for prediction/Classification
  #* dim(testData)
  head(testData)
  tail(testData)
  train = sample(1:nrow(testData), nrow(testData)/3 * 2)
  
  testData_train = testData[train, ]
  
  testData_test = testData[-train,]
  
  fit = lda(Terror_Event ~ Recruitment + Planning + Preparatory, data = testData_train)
  # fit = qda(TERROR_EVENT ~ Recruitment + Planning + preparatory, data = testData_train)
  pred = predict(fit, testData_test)
  
  pred_class = pred$class
  
  table(pred_class, testData_test$Terror_Event)       ### POTENTIAL OUTPUT
  
  t <- table(pred_class, testData_test$Terror_Event)     
  t
  summary(t)
  
  t <- as.data.frame(t)
  if (!is.null(predictionByDiscriminantAnalysis) && predictionByDiscriminantAnalysis != 'NA') {
    arc.write(predictionByDiscriminantAnalysis, t)
  }
  
  
  mean(pred_class == testData_test$Terror_Event)
  
  
###Testing 
  ####TESTING MODEL WITH NEW UNSEEN DATASET -- Usewr Input
  ## discriminanrt analysso
  T1 <- data.frame("Recruitment" = finalRiskValue, "Planning"= finalPlanRiskValue , "Preparatory"= finalPrepRiskValue, "TERROR_EVENT" = "N")
  pred = predict(fit, T1)
  
  pred_class = pred$class
  
  table(pred_class, T1$TERROR_EVENT)   ### POTENTIAL OUTPUT
  
  mean(pred_class == T1$TERROR_EVENT)   ### POTENTIAL OUTPUT
  
  
  ## KMEANS TESTING WITH NEW UNSEEN USER INPUT
  
  T2 <- data.frame("Recruitment" = finalRiskValue, "Planning"= finalPlanRiskValue , "Preparatory"= finalPrepRiskValue)
  table(cl_predict(clusteredData, T2))   ### POTENTIAL OUTPUT
  
  T1
  T2
  
  return(out_params)
}

##=========================================================================================================================#
load_pkgs = function(pkgs){
  new_pkgs = pkgs[!(pkgs %in% installed.packages()[, 'Package'])]
  if (length(new_pkgs) > 0) install.packages(new_pkgs)
  invisible(lapply(pkgs, function(x)
    suppressMessages(library(x, character.only = TRUE))))
}

##=========================================================================================================================#



# Provide testing Enviro before Loading tool to arcGIS 
# ***In this part I will be taking the values Directly from users ***

# Test tool in standalone R
library(arcgisbinding)
arc.check_product()

source_data = 'C:/Users/ADMIN/Desktop/TerroristDB.gdb/terrorismDB'


#***Recruitment Input
recruitsAge <- 25
employmentStatus <-  TRUE
terrorGroup <- "alshabaab"
recruitCountry <- "Kenya"
recruitTown <- "Nairobi"
recruitmentTimeSpan <- 20

#*****Planning Input 
planMeetingsCountry <- "Italy"

planMeetingsCity <- "Nairobi"
planTrainingCountry <- "Kenya"
planTrainingCity <- "Boura"
planningTimeSpan <- 10

##****Preparatory Input
prepWeapons <- TRUE
prepFunds <- FALSE
prepIllegalImmigration <- TRUE
prepSurveillance <- FALSE
prepTimeSpan <- 17

## Output
##STATISTICAL RESULTS eg.ANOVA etc
predictionKMeansTable <- 'C:/Users/ADMIN/Desktop/TerroristDB.gdb/terrorismDB/predicted_values'
predictionByDiscriminantAnalysis  <- 'C:/Users/ADMIN/Desktop/TerroristDB.gdb/prediction_DA'
# estimateAreaOfFocus <- 




