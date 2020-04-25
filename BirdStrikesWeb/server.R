library(tidyverse)
library(shiny)
library(RSQLite)
library(leaflet)
library(ggplot2)
library(showtext)
library(Cairo)
library(RCurl) # needed for showtext

# font.add.google("Source Sans Pro", "Source Sans Pro")
# showtext.auto()

# source("flickr_api.R")
# 
db <- readRDS("AIRFIELDS_MASTERv2.RDS")

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
                  -121.5910034)
  )
  

  d <- reactive({
    dist <- switch(
      input$search,
      default="KDEN",
      kden = "KDEN",
      kdfw = "KDFW",
      kord = "KORD",
      ksmf = "KSMF"
    )
  })
  
  # lat <- reactiveValues({
  #   air %>% select(latitude) %>% filter(air$airfields == d())
  # })
  # 
  # lng <- reactiveValues({
  #   air %>% select(longitude) %>% filter(air$airfields == d())
  # })
  # 
  # output$summary <- renderPrint({
  #   print(lng())
  #   print(lat())
  # })
  


  output$mymap <- renderLeaflet({

      #
      # req(lat, lon)
      
      # Please use a different tile URL for your own apps--we pay for these!
      # leaflet() %>% addTiles("//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png") %>%
      #   addMarkers(lng = lon, lat = lat)
    # leaflet::leaflet() %>% 
    #   addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%
      
      
    leaflet()  %>%
      addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(lng = -104.6729965,
                 lat = 39.86169815)
      # addMarkers(lng = 39.86169815, lat = -104.6729965)
      # %>%
      #   addCircleMarkers(
      #     lon,
      #     lat,
      #     weight = 1,
      #     radius = ~ sqrt(Count) * 0.7
      # )
      # leaflet(options = leafletOptions(
      #   minZoom = 1,
      #   maxZoom = 5,
      #   zoomDelta = 0.5
      # )) %>%
      #   addProviderTiles(providers$OpenStreetMap) %>% setView(lng = lng(),
      #                                                         lat = lat(), zoom = 2)
      # %>% addMarkers(m, lng = -104.6729965, lat = 39.86169815)
    })

  
  # output$summary <- renderTable({
  #   results <- db %>% filter(db$`AIRPORT ID` == d())
  #   head(results)
  # })
  
  # output$plot <- renderPlot({
  #   dist <- input$search
  #   n <- input$n
  # 
  #     hist(d(),
  #          main = paste("r", dist, "(", n, ")", sep = ""),
  #          col = "steelblue", border = "black")
  # })



  # 
  # output$table <- renderTable({
  #   head(data.frame(x = d()))
  # })

}
