#  #Requiring all of the predefined libraries and functions
 source("Functions.R")
#---------------------------- Pilot App ----------------------------- 
#R User Interface
dashboardPage(skin = "blue",
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
                            max = Sys.Date()+500),

                  menuItem("About WTF!", tabName = "Inputs", icon = icon("users"))

                )
              ),
              #Body
              dashboardBody(
                fluidRow(),

                fluidRow(
                  infoBoxOutput("vboxrisk"),
                  valueBoxOutput("vboxstrikes"),
                  valueBoxOutput('vboxengf')),
                    # actionButton(inputId='vboxengf', label="Learn More",
                    #                           icon = icon("bird"),
                    #                           onclick ="window.open('https://wildlife.faa.gov/add')"))


                fluidRow(
                  # box(tags$style(type = "text/css", "html, body {width:100%;height:200%}"),
                  #            leaflet::leafletOutput("map"),
                  #            absolutePanel(id="controls",
                  #                          style="z-index:500;",
                  #                          class = "panel panel-default",
                  #                          draggable = TRUE),
                  #            width = 12)

                  column(12, align = "center",
                    box(
                      width = 10,
                      title = "Map of Current Bird & Weather Conditions at KDEN:",
                      status = "primary",
                      solidHeader = TRUE,
                      collapsible = FALSE,
                      height = "100%",
                      leafletOutput(outputId = "map", width="100%", height = 940)))
                  #box(uiOutput("tag"),
                  #    width = 4,
                  #    height = 3),
                  #valueBoxOutput("box_01"),
                  #bsModal("mod","Report a Strike","btn")

                  ),


                # fluidRow(
                #   box(tableOutput("summary"),
                #   width = 4,
                #   background = "light-blue",
                #   p("This is content. The background color is set to light-blue")))
                # ,fluidRow(
                #   align="LEFT", tags$img(src='images/1 Logo.png', width = "300px")
                # ),

              )

)
 
##hyperlink the faa strike data base as an option 

# #---------------------------- OTHER UI -----------------------------
# shinyUI(navbarPage(title = "BirdStrikes",
#                    theme = "style/style.css",
#                    footer = includeHTML("footer.html"),
#                    fluid = TRUE,
#                    collapsible = TRUE,
# 
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
#                    # tab panel 2 - Neighborhood Browser
#                    tabPanel("Pilot App",
#                             "neighborhoodDescription()",
#                             includeHTML("scrollToTop.html")
#                    ),
# 
#                    # tab panel 3 - Location Comparison
#                    tabPanel("Engine Failure App",
#                             "propertyComparison()"
#                    ),
# 
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




