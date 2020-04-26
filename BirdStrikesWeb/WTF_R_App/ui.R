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
