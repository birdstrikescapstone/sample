library(tidyverse)
library(shiny)
library(leaflet)
library(ggplot2)
library(showtext) 
library(Cairo)
library(RCurl) # needed for showtext

# font.add.google("Source Sans Pro", "Source Sans Pro")
# showtext.auto()

# source("flickr_api.R")
# 
data <- readRDS("AIRFIELDS_MASTERv2.RDS")

# Mistakes were made...
options(shiny.table.class = "usa-table-borderless")

# query_db <- function(query) {
#   conn <- dbConnect(SQLite(), dbname = "CollegeScorecard.sqlite")
#   on.exit(dbDisconnect(conn), add = TRUE)
#   
#   dbGetQuery(conn, query)
# }

# format_badge <- function(icon_name, label) {
#   tagList(icon(icon_name, class = "fa-2x fa-fw"),
#           br(),
#           span(label))
# }

# Some names in the dataset are in ALL CAPS... gross. Convert to title case.
# uncap <- function(str) {
#   allcap <- str == toupper(str)
#   str[allcap] <- stringr::str_to_title(str[allcap])
#   str
# }

function(input, output, session) {
  
  # map <- leaflet() %>% addTiles() %>% setView(-93.65, 
  #                                             42.0285, 
  #                                             zoom = 17)
  
  d <- reactive({
    dist <- switch(
      input$search,
      default="KDEN",
      kden = "KDEN",
      kdfw = "KDFW",
      kord = "KORD",
      ksmf = "KSMF")
    })
  
  output$summary <- renderTable({
    data %>%  
      filter(`AIRPORT ID`== input$airfield) %>% 
      group_by(`AIRPORT ID`) %>% 
      summarise(STRIKES = sum(STRIKECOUNT)) %>% 
      arrange(-STRIKES)
    
  })
  
  # output$test <- renderText({
  #   print(d())
  #   
  # })

  output$map <- renderLeaflet({
    # coord <- air %>% 
    #   filter(airfields == input$airfield)
    # lat<- coord[1,2]
    # long<-coord[1,3]
    
    
    mymap <- leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(noWrap = TRUE)) %>% 
     # addMarkers(lng = air$longitude, lat = air$latitude, popup = names(air)) %>% 
      addCircleMarkers(air$longitude,air$latitude,
                       sqrt(air$strikes) * 0.79899) %>%
      addPopups(air$longitude,air$latitude,
                htmltools::htmlEscape(paste(
                  paste(air$airfields, sep = " "),
                  paste("Strikes:", as.character(air$strikes), sep = " "),
                  sep = ", ")))

    # ,
    #                    label = ~htmlEscape(paste(
    #                        paste("Airfield = ", as.character(air$airfields), sep = " "),
    #                        paste("Count = ", as.character(air$sstrikes), sep = " "),
    #                        sep = ", ")))
    #     #weight = 1,
        #radius = ~ sqrt(Count) * 0.7,
        # label = ~htmlEscape(paste(
        #   paste("Zip = ", as.character(100), sep = " "),
        #   paste("Count = ", as.character(100), sep = " "),
        #   sep = ", ")))
      #   ))) %>% 
      # addHeatmap(
      #   lng =  ~ air$longitude,
      #   lat =  ~ air$latitude,
      #   blur = 20,
      #   max = 0.05,
      #   radius = 15
      # )
    mymap
    }) #the opening should show the 4 airfields and a heat map of max to min agg strikes
 }
