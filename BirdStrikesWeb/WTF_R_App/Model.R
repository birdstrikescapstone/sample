# Creating a vector of packages used within
packages <- c(
  'car',
  'caret',
  'data.table',
  'lubridate',
  'pROC',
  'ranger',
  'Rtsne',
  'tidyverse',
  'xgboost',
  'caretEnsemble'
)

# Checking for package installations on the system and installing if not found
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# Including the packages for use
for(package in packages){
  library(package, character.only = TRUE)
}


config.data <-
  data.frame(
    "AIRFIELD" = c('KDEN', 'KDFW', 'KORD', 'KSMF'),
    "MODELFILENAME" = c(
      "model_xgb_KDEN.RDS",
      "model_rf_KDFW.RDS",
      "model_xgb_KORD.RDS",
      "model_xgb_KSMF.RDS"
    ),
    "AVGFILENAME" = c(
      "Avgs_KDEN.RDS",
      "Avgs_KDFW.RDS",
      "Avgs_KORD.RDS",
      "Avgs_KSMF.RDS"
    )
  )

########################################################################
# Purpose: Function generates the input to the model based on the
# Averages of the strikes dataset based in Day Of Year
#
# Input: Airfield - KDEN/KDFW/KORD/KSMF
#        StrikeDate - Date to display the Strike Risk level
#
# Output: Dataframe containing data for model Input
#######################################################################
getAverageByInput <- function(airfield, strikedate) {
  # Get Averages file name from the custom dataframe
  avgFileName <-
    config.data %>% select(AVGFILENAME) %>% filter(AIRFIELD == airfield)
  
  # Read the Averages from file system
  data <- readRDS(avgFileName)
  
  # Convert the input to date format and convert to Day of Week
  data <-
    data %>% filter(DAYOFYEAR == lubridate::yday(as.Date(strikedate)))
  
  # Create Season columns as required by the input to the model
  data <-
    data %>% add_column(
      SEASON.winter = 0,
      SEASON.summer = 0,
      SEASON.fall = 0,
      SEASON.spring = 0
    )
  
  # Fill the season columns based on the value in the Averages dataframe
  if (data$SEASON == 1) {
    data$SEASON.winter <- 1
  } else if (data$SEASON == 2) {
    data$SEASON.spring <- 1
  } else if (data$SEASON == 3) {
    data$SEASON.summer <- 1
  } else {
    data$SEASON.fall <- 1
  }
  
  # Remove Season column and make all columns numeric
  data <-
    data %>% select(-SEASON) %>% mutate_all(as.numeric)
  
  # Return dataframe
  data
}


########################################################################
# Purpose: Function executes the predict on the model and returns the  
# strike risk level and probability
# 
# Input: Airfield - KDEN/KDFW/KORD/KSMF
#        modelInputData - Model input dataframe obtained from the 
#                         getAverageByInput functions
#
# Output: Dataframe containing data for model Input
#######################################################################
runPredict <- function(airfield, modelInputData) {
  
  # Get Model File name from config data 
  modelFileName <-
    config.data %>% select(MODELFILENAME) %>% filter(AIRFIELD == airfield)
  
  # Load model from RDS
  model <- readRDS(modelFileName)
  
  # Call predict with type = raw to get the risk levels
  predicted.raw <- predict(model, modelInputData, type = "raw")
  
  # Call Predict with type = prob to get probabilities
  predicted.prob <- predict(model, modelInputData, type = "prob")
  
  # Create results dataframe for use in UI
  strike.results <-
    data.frame("STRIKERISKLEVEL" = predicted.raw,
               "STRIKEPROBABILITY" = predicted.prob)
  
  # Return dataframe
  strike.results
}
