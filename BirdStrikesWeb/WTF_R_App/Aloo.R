# library("shiny")
# library("shinydashboard")
# library("shinyBS")
# 
# header <- dashboardHeader(title = "reporductible example")
# 
# body <- dashboardBody(valueBoxOutput("box_01"),
#                       bsModal("modal", "foo", trigger = "", "bar"))
# sidebar <- dashboardSidebar()
# 
# ui <- dashboardPage(header,sidebar,body,skin="green")
# 
# server = function(input, output, session) {
#   # ----- First info box synthesis menu
#   output$box_01 <- renderValueBox({
#     entry_01 <- "BlaBla"
#     valueBox(value=entry_01, 
#              icon = icon("users",lib="font-awesome"),
#              width=NULL,
#              color = "blue",
#              subtitle = HTML("<b>my substitle</b> <button id=\"button\" type=\"button\" class=\"btn btn-default action-button\">Show modal</button>")
#     )
#   })
#   
#   observeEvent(input$button, {
#     toggleModal(session, "modal", "open")
#   })
# }
# 
# runApp(list(ui = ui, server = server))




# runApp(
#   list(ui = fluidPage(
#     uiOutput("tab")
#   ),
#   server = function(input, output, session){
#     url <- a("Google Homepage", href="https://www.google.com/")
#     output$tab <- renderUI({
#       tagList("URL link:", url)
#     })
#   })
# )






library(googleVis)
library(shiny)
library(ROpenWeatherMap)

g=list('Temperature'='temp','Minimum Temperature'='temp_min','Maximum temperature'='temp_max','Pressure'='pressure','Sea Level'='sea_level','Ground level'='grnd_level','Humidity'='humidity')

api_key<-"99f521810d0fef37f59930f36dbb2256"
ui<-shinyUI(fluidPage(
  
  # Application title
  titlePanel("Weather forecast using ROpenWeatherMap"),
  
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("time",
                  h3("Forecast"),
                  min = 1,
                  max = 37,
                  value = 1,animate =  animationOptions(interval = 4000,
                                                        playButton = icon('play', "fa-3x"),
                                                        pauseButton = icon('pause', "fa-3x"))),
      selectInput("feature", label = h3("Feature"), 
                  choices = list(g=g), selected = 1)          
    ),
    
    
    mainPanel(
      htmlOutput("gvis")
    )
  )
))

library(ROpenWeatherMap)
library(shiny)

states=c("Denver"
         ,"Arunachal Pradesh"
         ,"Assam"
         ,"Bihar"
         ,"Chhattisgarh"
         ,"Goa"
         ,"Gujarat"
         ,"Haryana"
         ,"Himachal Pradesh"
         ,"Jammu and Kashmir"
         ,"Jharkhand"
         ,"Karnataka"
         ,"Kerala"
         ,"Madhya Pradesh"
         ,"Maharashtra"
         ,"Manipur"
         ,"Meghalaya"
         ,"Mizoram"
         ,"Nagaland"
         ,"Odisha"
         ,"Punjab"
         ,"Rajasthan"
         ,"Sikkim"
         ,"Tamil Nadu"
         ,"Tripura"
         ,"Uttar Pradesh"
         ,"Uttarakhand"
         ,"West Bengal"
         ,"Andaman and Nicobar Islands"
         ,"Chandigarh"
         ,"Nepal"
         ,"Bhutan"
         ,"Bangladesh"
         ,"Dadra and Nagar Haveli"
         ,"Daman and Diu"
         ,"National Capital Territory of Delhi" 
         ,"Lakshadweep" ,"Pondicherry") 
len=length(states)

data=NULL
for(i in 1:len)
{
  print(i)
  temp=get_weather_forecast(api_key,city=states[i])
  temp=temp$list$main
  temp$state=states[i]
  temp$index=1:nrow(temp)
  data=as.data.frame(rbind(data,temp))
}
d=gsub("Odisha","Orissa",data$state)
d=gsub("National Capital Territory of Delhi","Delhi",d)
d=gsub("Uttarakhand","Uttaranchal",d)
data$state=d
data$state=as.character(data$state)

server<- shinyServer(function(input, output) {
  
  output$gvis = renderGvis({
    d1=subset(data,index==input$time)
    #print("processing")
    
    #   gvisGeoChart(d1,
    #               locationvar="state", colorvar="temp",
    #               options=list(region="IN", displayMode="regions", 
    #                           resolution="provinces",
    #                          width=500, height=400,
    #                         colorAxis="{colors:['#FFFFFF', '#0000FF']}"
    #           ))
    
    G3= gvisGeoChart(d1, 
                     locationvar = "state", 
                     colorvar = input$feature,
                     options=list(region="IN", 
                                  displayMode="regions", 
                                  resolution="provinces",
                                  width=500,height=400))
    Tbl <- gvisTable(d1[,c('state',input$feature)], options=list(height=300), 
                     formats=list(temp="#,###.0"))
    gvisMerge(G3, Tbl, horizontal=T)
  })
  
  
})


runApp(ui,server)