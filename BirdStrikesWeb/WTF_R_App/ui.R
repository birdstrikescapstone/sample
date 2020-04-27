 #Requiring all of the predefined libraries and functions
 source("Functions.R")

#R User Interface
dashboardPage(skin = "black",
              dashboardHeader(title = "Birdstrikes"),

              #Sidebar for inputs
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Pilot Dashboard", tabName = "Inputs", icon = icon("dashboard")),
                  selectInput("airfield","Airfields:",
                              airfields),

                  dateInput("date","Date:",
                            value = as.character(Sys.Date()),
                            min = as.character(Sys.Date()),
                            max = Sys.Date() + 7),

                  menuItem("About WTF!", tabName = "Inputs", icon = icon("users"))

                )
              ),
              #Body
              dashboardBody(
                fluidRow(
                  infoBoxOutput("vboxstrikes"),
                  valueBoxOutput("vboxrisk"),
                  valueBoxOutput("vboxengf")
                  ),
                fluidRow(id = "second",
                         column(6,
                                style = "border: 1px solid black;",
                                leaflet::leafletOutput("map"))
                         ),
                
                fluidRow(
                  tableOutput("summary"))
                
              )
              
)

#-------------OTHER APP ---------------
# shinyUI(navbarPage(title = "BirdStrikes",
#                    theme = "style/style.css",
#                    footer = includeHTML("footer.html"),
#                    fluid = TRUE, 
#                    collapsible = TRUE,
#                    
#                    # ----------------------------------
#                    # tab panel 1 - Home
#                    tabPanel("Home",
#                             includeHTML("home.html"),
#                             tags$script(src = "plugins/scripts.js"),
#                             tags$head(
#                               tags$link(rel = "stylesheet", 
#                                         type = "text/css", 
#                                         href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
#                               tags$link(rel = "icon", 
#                                         type = "image/png", 
#                                         href = "images/logo_icon.png")
#                             )
#                    ),
#                    
#                    # ----------------------------------
#                    # tab panel 2 - Neighborhood Browser
#                    tabPanel("BirdStrike App",
#                             "neighborhoodDescription()",
#                             includeHTML("scrollToTop.html")
#                    ),
#                    
#                    # ----------------------------------
#                    # tab panel 3 - Location Comparison
#                    tabPanel("Airfield Analysis",
#                             "propertyComparison()"
#                    ),
#                    
#                    # ----------------------------------
#                    # tab panel 4 - About
#                    tabPanel("About WTF",
#                             includeHTML("about.html"),
#                             shinyjs::useShinyjs(),
#                             tags$head(
#                               tags$link(rel = "stylesheet", 
#                                         type = "text/css", 
#                                         href = "plugins/carousel.css"),
#                               tags$script(src = "plugins/holder.js")
#                             ),
#                             tags$style(type="text/css",
#                                        ".shiny-output-error { visibility: hidden; }",
#                                        ".shiny-output-error:before { visibility: hidden; }"
#                             )
#                    )
#                    
# ))




