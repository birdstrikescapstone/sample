###### Libraries  

  # Creating a vector of packages used within
  required.packages <- c("base","jsonlite","leaflet","lubridate","magrittr","padr","plotly","purrr","raster",
                         "rgdal","rgeos","rvest","selectr","shiny","shinyjs","sp","stringr","urltools","utils",
                         "xml2","xts","zoo","tidyverse","shinydashboard","shinythemes","shinyWidgets",  'car',
                         'caret','data.table','pROC','ranger','Rtsne','xgboost','caretEnsemble')
  
  # Function to Install and Load R Packages
  install.load.packages <- function(required.packages){
    required.packages <-
      required.packages[!(required.packages %in% installed.packages()[, "Package"])]
    if (length(required.packages)){
      install.packages(required.packages, repos = 'http://cran.us.r-project.org')
    }
    for (package.name in required.packages)
    {
      library(package.name,
              character.only = TRUE,
              quietly = TRUE)
    }
  }
  
  # Call the Function
  install.load.packages(required.packages)
  
  # Including the packages for use
  for (package in required.packages) {
    library(package, character.only = TRUE)
  }
  
#---------------------------- Predefined Values ----------------------------- 
  ###### Predefined Values 
  #Getting the Airfield Dataset
  data <- readRDS("AIRFIELDS_MASTERv2.RDS")

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
                2170,1557,1891))
  
  #Model Information 
  source("Model.R")
  
  #getAverageByInput("KDEN",Sys.Date())
  
  
  
  
  
  
  
  
  
  
  
  
  
  
