library(leaflet)
library(shinydashboard)
library(shinythemes)
library(shiny)

#library(shinySignals)
# function(req) {
#   htmlTemplate("www/i1ndex.html")
# }


#Predefined Variables

#List of Airfields to select
airfields<-list("KDEN","KDFW","KORD","KSMF")

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

#  <option value="kden">KDEN - Denver International</option>
#  <option value="kdfw">KDFW - Dallas Ft Worth International</option>
#  <option value="kord">KORD - Chicago OHare International</option>
#  <option value="ksmf">KSMF - Sacremento International</option>

#R User Interface 
ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Birdstrikes"),
                    #Sidebar
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Pilot Dashboard", tabName = "Inputs", icon = icon("dashboard")),
                        selectInput("airfield","Airfields:",
                                    c(airfields)),
                        
                        dateInput("date","Date:",
                                  value = as.character(Sys.Date()),
                                  min = as.character(Sys.Date()),
                                  max = Sys.Date() + 7),
                        
                        menuItem("About WTF!", tabName = "Inputs", icon = icon("users"))
                        
                      )
                    ),
                    #Body 
                    dashboardBody(
                      fluidPage(
                        fluidRow(
                          column(5,
                                 tableOutput('summary')
                          )
                        )
                      ),
                      
                      # fluidPage(
                      #   fluidRow(
                      #     column(12,
                      #            textOutput('test')
                      #     )
                      #   )
                      # ),
                      
                      
                      
                      fluidRow(
                        column(7,
                               leaflet::leafletOutput("map")
                        )
                      )
                      
                      # tabItems(
                      #   tabItem("dashboard",
                      #           fluidRow(
                      #             valueBoxOutput("rate"),
                      #             valueBoxOutput("count"),
                      #             valueBoxOutput("users")
                      #           ),
                      #           fluidRow(
                      #             box(
                      #               width = 8, status = "info", solidHeader = TRUE,
                      #               title = "Popularity by package (last 5 min)",
                      #               bubblesOutput("packagePlot", width = "100%", height = 600)
                      #             ),
                      #             box(
                      #               width = 4, status = "info",
                      #               title = "Top packages (last 5 min)",
                      #               tableOutput("packageTable")
                      #             )
                      #           )
                      #   ),
                      #   tabItem("rawdata",
                      #           numericInput("maxrows", "Rows to show", 25),
                      #           verbatimTextOutput("rawtable"),
                      #           downloadButton("downloadCsv", "Download as CSV")
                      #   )
                    )
)


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





