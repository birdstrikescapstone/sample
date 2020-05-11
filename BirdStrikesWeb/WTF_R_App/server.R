source("Functions.R")

function(input, output, session) {
  # 
  # d <- reactive({
  #   dist <- switch(
  #     input$search,
  #     default="KDEN",
  #     kden = "KDEN",
  #     kdfw = "KDFW",
  #     kord = "KORD",
  #     ksmf = "KSMF")
  #   })
  
  output$summary <- renderTable({
    getDataAndRunPredict(input$airfield,input$date)
    

  })
  output$map <- renderLeaflet({
    #get the coordinates of the input of the user 
    coord <- air %>%
      filter(airfields == input$airfield)
    lat<- coord[1,2]
    long<-coord[1,3]
    
    #get the weather data on the map
    owm_data <- find_city(lat = coord[1,2], lon = coord[1,3] , units = "imperial") %>%
      owmr_as_tibble()
    
    #get the bird data 
    birds <- rebird::ebirdgeo(species = NULL,
                              lat = coord[1,2],
                              lng = coord[1,3],
                              back = 1,
                              dist = as.numeric(
                                units::set_units(30, "mi")),
                              key = EBIRD_KEY)
    #group the bird data 
    birds<- birds %>% group_by(lat,lng) %>% 
      summarise(howMany = sum(howMany))
    
    #map the data 
    leaflet(birds) %>% addProviderTiles(providers$OpenStreetMap) %>% 
      setView(zoom = 9.5, lat = coord[1,2], lng = coord[1,3]) %>%
      addProviderTiles(providers$OpenWeatherMap.Precipitation,
                       options=providerTileOptions(apiKey="99f521810d0fef37f59930f36dbb2256")) %>% 
      owmr::add_weather(
        owm_data,
        template = "<b>{{name}}</b>, {{temp}}<c2><b0>F",
        icon = owm_data$weather_icon) %>% 
      addMarkers(lng = air$longitude, lat = air$latitude, popup = names(air)) %>%
      leaflet.extras::addHeatmap(
        lat = ~ birds$lat,
        lng = ~ birds$lng,
        blur = 20,
        max = 0.05,
        radius = 15) 
    

  })
  output$vboxengf<-renderValueBox({
    valueBox(
      "Report a Birdstrike (FAA):",
      #subtitle = tags$p("", style = "font-size: 200%;"),
      href="https://wildlife.faa.gov/add",
      subtitle=HTML("<button id=\"button\" type=\"button\" class=\"btn btn-default action-button\">Report a Strike</button>"),
      icon = icon("plane"),
      color = "yellow")
    })
  
  
  
  output$vboxstrikes <-renderValueBox({
    t <- data %>% 
      filter(`AIRPORT ID`== input$airfield) %>% 
      group_by(`AIRPORT ID`) %>% 
      summarise(STRIKES = sum(STRIKECOUNT)) %>% 
      arrange(-STRIKES)  
    
    valueBox(
        paste(input$airfield," Historical Strikes:"),
        subtitle = tags$p(t[1,2], style = "font-size: 200%;"),
        icon = icon("feather"),
        color = "aqua")
    
  })
  output$vboxrisk <-renderValueBox({
    risk <- getDataAndRunPredict(input$airfield,input$date)
    if(risk[1,1] == "L"){
      valueBox(
        "Birdstrike Risk Level:",
        subtitle = tags$p(paste(round(risk[1,3],1),"LOW RISK"), style = "font-size: 200%;"),
        icon = icon("earlybirds"),
        color = "green")
    }
    else if(risk[1,1] == "M"){
      valueBox(
        "Birdstrike Risk Level:",
        subtitle = tags$p("MEDIUM RISK", style = "font-size: 200%;"),
        icon = icon("earlybirds"),
        color = "yellow")
    }
    else valueBox(
      "Birdstrike Risk Level:",
      subtitle = tags$p(paste(" ALERT! HIGH RISK! on",input$date), style = "font-size: 200%;"),
      icon = icon("earlybirds"),
      color = "red")

  })
  
  
  # output$box_01 <- renderValueBox({
  #   box1<-valueBox(value=20,
  #                  icon = icon("users",lib="font-awesome"),
  #                  color = "blue",
  #                  href="https://wildlife.faa.gov/add",
  #                  subtitle=HTML("<button id=\"button\" type=\"button\" class=\"btn btn-default action-button\">Report a Strike</button>")
  #   )
  #   box1$children[[1]]$attribs$class<-"action-button"
  #   box1$children[[1]]$attribs$id<-"button_box_01"
  #   
  # 
  #   return(box1)
  # })
  # observeEvent(input$button_box_01, {
  #   toggleModal(session,"mod","open")
  #   })
  # output$tag <- renderUI({
  #  # urlfaa<- a("Click to Report a Strike", href="https://wildlife.faa.gov/add")
  #   tags$a(imageOutput("www/images/FAA.png"),
  #          href="https://wildlife.faa.gov/add")
  #   #tagList(urlfaa)
  # })
}



