source("./Functions.R", local = TRUE)
# source("Model.R")

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
    # t <-data %>%
    #   filter(`AIRPORT ID`== input$airfield) %>%
    #   group_by(`AIRPORT ID`) %>%
    #   summarise(STRIKES = sum(STRIKECOUNT)) %>%
    #   arrange(-STRIKES)
    
    getDataAndRunPredict(input$airfield,input$date)
  })
  
  output$map <- renderLeaflet({
    # coord <- air %>%
    #   filter(airfields == input$airfield)
    # lat<- coord[1,2]
    # long<-coord[1,3]
    #the opening should show the 4 airfields and a heat map of max to min agg strikes
    
    mymap <- leaflet() %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      # addMarkers(lng = air$longitude, lat = air$latitude, popup = names(air)) %>%
      addCircleMarkers(air$longitude, air$latitude,
                       sqrt(air$strikes) * 0.79899) %>%
      addPopups(air$longitude, air$latitude,
                htmltools::htmlEscape(paste(
                  paste(air$airfields, sep = " "),
                  paste("Strikes:", as.character(air$strikes), sep = " "),
                  sep = ", "
                )))
    mymap
  })
  
  #Header Boxes
  output$vboxengf <- renderValueBox({
    valueBox(
      "Engine Failure:",
      subtitle = tags$p("TEXT", style = "font-size: 200%;"),
      icon = icon("earlybirds"),
      color = "yellow"
    )
  })
  
  
  
  output$vboxstrikes <- renderValueBox({
    t <- data %>%
      filter(`AIRPORT ID` == input$airfield) %>%
      group_by(`AIRPORT ID`) %>%
      summarise(STRIKES = sum(STRIKECOUNT)) %>%
      arrange(-STRIKES)
    
    valueBox(
      "Historical Strikes:",
      subtitle = tags$p(t[1, 2], style = "font-size: 200%;"),
      icon = icon("feather"),
      color = "aqua"
    )
    
  })
  
  
  output$vboxrisk <- renderValueBox({
    risk <- getDataAndRunPredict(input$airfield, input$date)
    
    if (risk$STRIKERISKLEVEL == "L") {
      valueBox(
        "Risk Level:",
        subtitle = tags$p("LOW", style = "font-size: 200%;"),
        icon = icon("plane"),
        color = "green"
      )
    }
    else if (risk$STRIKERISKLEVEL == "H") {
      valueBox(
        "Risk Level:",
        subtitle = tags$p("HIGH", style = "font-size: 200%;"),
        icon = icon("plane"),
        color = "red"
      )
    }
    else
      valueBox(
        "Risk Level:",
        subtitle = tags$p("MEDIUM", style = "font-size: 200%;"),
        icon = icon("plane"),
        color = "teal"
      )
  })
}



