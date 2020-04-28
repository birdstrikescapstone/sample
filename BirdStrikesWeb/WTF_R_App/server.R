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
    coord <- air %>%
      filter(airfields == input$airfield)
    lat<- coord[1,2]
    long<-coord[1,3]
    
    mymap <- leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(noWrap = TRUE)) %>% 
      addMarkers(lng = air$longitude, lat = air$latitude, popup = names(air)) %>% 
      addCircleMarkers(air$longitude,air$latitude,
                       sqrt(air$strikes) * 0.79899999) %>%
      addPopups(air$longitude,air$latitude,
                htmltools::htmlEscape(paste(
                  paste(air$airfields, sep = " "),
                  paste("Strikes:", as.character(air$strikes), sep = " "),
                  sep = ", ")))
    mymap
  })
  output$vboxengf<-renderValueBox({
    valueBox(
      "Engine Failure:",
      subtitle = tags$p("TEXT", style = "font-size: 200%;"),
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
      subtitle = tags$p(" ALERT! HIGH RISK!", style = "font-size: 200%;"),
      icon = icon("earlybirds"),
      color = "red")

  })
  
  
  
}



