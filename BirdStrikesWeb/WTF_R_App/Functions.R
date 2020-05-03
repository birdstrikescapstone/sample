
#---------------------------- Libraries ----------------------------- 
  #Installing pacman  
  if (!require("pacman")) install.packages("pacman"); library(pacman)
  #enable pacman to call the required libraries  
  pacman::p_load(base,jsonlite,leaflet,lubridate,magrittr,padr,plotly,purrr,raster,rgdal,
                 rgeos,rvest,selectr,shiny,shinyjs,sp,stringr,urltools,utils,xml2,xts,zoo,
                 tidyverse,shinydashboard,shinythemes,shinyWidgets,car,caret,data.table,pROC,ranger,Rtsne,
                 xgboost,caretEnsemble,rebird,leaflet.extras,leaflet,shinyBS,png)
  
#---------------------------- Predefined Values ----------------------------- 
  ###### Predefined Values 
  #Getting the Airfield Dataset
  data <- readRDS("data/AIRFIELDS_MASTERv2.RDS")

  #List of Airfields to select in the User Interface
  airfields<-c("KDEN - Denver International" = "KDEN",
               "KDFW - Dallas Ft Worth International" = "KDFW",
               "KORD - Chicago OHare International" =  "KORD",
               "KSMF - Sacremento International" = "KSMF")
  
  #Airfields for Long & Lat
  air <- data.frame(
    airfields = c("KDEN",
                  "KDFW",
                  "KORD",
                  "KSMF"),
    latitude = c(39.86169815,
                 32.896801,
                 41.9786,
                 38.69540024),
    longitude = c(-104.6729965,
                  -97.038002,
                  -87.9048,
                  -121.5910034),
    strikes = c(2814,
                2170, 1557, 1891),
    modelfilename = c(
     # "data/model_xgb_KDEN.RDS",
      "data/model_gbm_KDEN.RDS",
      "data/model_rf_KDFW.RDS",
      "data/model_xgb_KORD.RDS",
      "data/model_xgb_KSMF.RDS"
    ),
    avgfilename = c(
      "data/Avgs_KDEN.RDS",
      "data/Avgs_KDFW.RDS",
      "data/Avgs_KORD.RDS",
      "data/Avgs_KSMF.RDS"
    )
  )
  
  #Ebird API Key
  EBIRD_KEY <- 'ldto4uofjnf7'
  

  
#---------------------------- Functions ----------------------------- 
  # Purpose: Function generates the input to the model based on the
  # Averages of the strikes dataset based in Day Of Year. After generating 
  # the input, executes the predict on the model and returns the  
  # strike risk level and probability
  #
  # Input: Airfield - KDEN/KDFW/KORD/KSMF
  #        StrikeDate - Date to display the Strike Risk level
  #
  # Output: Dataframe containing data for model Input
  getDataAndRunPredict <- function(airfield, strikedate) {
    
    # Get Averages file name from the custom dataframe
    fileNames <-
      air %>% select(avgfilename, modelfilename) %>% filter(airfields == airfield)

    
    # Read the Averages from file system
    data <- readRDS(paste0(fileNames$avgfilename))

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

    
    # Load model from RDS
    model <- readRDS((paste0(fileNames$modelfilename)))
    
    # Call predict with type = raw to get the risk levels
    predicted.raw <- predict(model, data, type = "raw")
    
    # Call Predict with type = prob to get probabilities
    predicted.prob <- predict(model, data, type = "prob")
    
    predicted.prob
    # Create results dataframe for use in UI
    strike.results <-
      data.frame("STRIKERISKLEVEL" = predicted.raw,
                 "STRIKEPROBABILITY" = predicted.prob)
    
    # Return dataframe
    strike.results
  }