

#read in data from folder
Combo_dataset <- read_sf("Combo_dataset.shp")
Community_dataset <- read_sf("Community_dataset.shp")
Neighborhood_dataset <- read_sf("Neighborhood_dataset.shp")


# Initial Shiny Map - Simple ----------------------------------------------
#---------------------------Color palettes all here---------------------
pal_blue6 = c("#154f4a", "#376b66", "#588782", "#7ea8a2", 
              "#a9ccc6", "#d8f2ed")

pal_blue11 = c("#d8f2ed",
               "#bfded8", "#a9ccc6", "#93bab4", "#7ea8a2",
               "#6b9993", "#588782", "#477872", "#376b66", 
               "#265c56", "#154f4a")

# Create color palette for communities on map
paletteContinuousC <- colorNumeric(palette = pal_blue11, domain = Community_dataset$Rate)
paletteContinuousN <- colorNumeric(palette = pal_blue11, domain = Neighborhood_dataset$Rate)



server <- function(input, output, session){
  
  set.seed(122)
  histdata <- rnorm(500)
  
  #observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = histdata, { 
  
  # event will be called when histdata changes, which only happens once, when it is initially calculated
  # showModal(modalDialog(
  #   title = "Welcome", 
  #  h1('Predicting Neighborhood Lead Risk in Minneapolis'),
  #  p("This tool can be used to understand the likelihood of toxic lead paint presence in Minneapolis neighborhoods. 
  #   The work is a proof of concept method for predicting residential lead presence, and rates should not be 
  #   considered actual measures of lead. To protect the privacy of individual residents, all predictions are 
  #   aggregated to the neighborhood level.")
  # ))
  # })
  
  url <- a("Minneapolis Lead Hazard Grant Program", href="http://www.minneapolismn.gov/health/homes/lead/lead-window")
  output$tab <- renderUI({
    tagList("For more information on testing and remediation lead:", url)
  })
  
  url2 <- a("Modeling Lead Risk in Homes", href="https://pennmusa.github.io/MUSA_801.io/project_4/index.html")
  output$tab2 <- renderUI({
    tagList("For detailed documentation of the process for predicting neighborhood lead rates:", url2)
  })
  
  subset_Community <- Combo_dataset %>% 
    filter(Combo_dataset$Geography == "CommName")                   #filter
  
  subset_Neighborhood <- Combo_dataset %>% 
    filter(Combo_dataset$Geography == "Neighborhood")
  
  
  #what popup map labels say
  community_popup <- paste0("<strong>Community: </strong>", 
                            subset_Community$Name,
                            "<br/><strong>Rate of homes predicted to have lead: </strong>", 
                            round(subset_Community$Rate, digits=2))
  
  
  #bar chart coding
  # output$summaryPlot <- renderPlot({
  #    ggplot(subset_Community, aes(x = subset_Community$Name, y = subset_Community$Rate)) +
  #     geom_bar(stat="identity", fill = "#477371")+
  #     xlab("Community")+
  #     ylab("Rate of Predicted Lead Homes")
  #  })
 
  #GIF
  output$test <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./www',
                                        paste('gif', 1, '.gif', sep='')))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)

  
  #map coding
  output$myratemap <- renderLeaflet({
    leaflet(subset_Community) %>% addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-93.2650, 44.9778, zoom = 11) %>% 
      addPolygons( data = subset_Community,
                   fillColor = ~paletteContinuousC(Rate),
                   weight = 0.8,
                   opacity = 0.6,
                   smoothFactor = 0.1,
                   #color = ~paletteBins(~Rate),
                   color = "#477371",
                   fillOpacity = 0.5,
                   #label = ~paste0("Rate: "),
                   highlight = highlightOptions(
                     fillColor = "yellow",
                     fillOpacity = 0.8,
                     weight = 2,
                     bringToFront = TRUE), 
                   popup = community_popup) #%>%
    # addLegend(pal = CommunityBucketPal, 
    # values = ~Rate, 
    # position = "bottomright", 
    # title = "Rate by Community",
    # opacity = 1)
  })
  
  
  observe({ 
    
    ### VARIABLE TAB
    SelectedVariable <- input$SelectedVariable
    #GIF
  #  output$test <- renderImage({
      # When input$n is 1, filename is ./images/image1.jpeg
     # filename <- normalizePath(file.path('./www',
                                         # paste('gif', 1, '.gif', sep='')))
      
      # Return a list containing the filename
     # list(src = filename)
   # }, deleteFile = FALSE)

    
    output$test <- renderImage({
      
      if (SelectedVariable =="Population") { 
        return(list(
          src = "www/gif1.gif",
          width = 600,
          height = 600,
          contentType = "image/gif",
          alt = "Population map"
        ))
      } else if (SelectedVariable =="Vacancy") { 
        return(list(
          src = "www/gif4.gif",
          width = 600,
          height = 600,
          contentType = "image/gif",
          alt = "Vacancy map"
        ))
      } else if (SelectedVariable =="Rentership") { 
        return(list(
          src = "www/gif3.gif",
          width = 600,
          height = 600,
          contentType = "image/gif",
          alt = "Rentership map"
        ))
      } else if (SelectedVariable =="Ownership") {
        return(list(
          src = "www/gif2.gif",
          width = 600,
          height = 600,
          filetype = "image/gif",
          alt = "Ownership map"
        ))
      }
      
    }, deleteFile = FALSE)
  #})
    
    ### RATE TAB
    SelectedCommunity <- input$SelectedCommunity
    #  Neighborhood <- input$Neighborhood
    
    SelectedNeighborhood <- input$SelectedNeighborhood
    
    subset_C <- subset_Community %>% 
      filter(subset_Community$Name == SelectedCommunity)
    
    subset_N <- subset_Neighborhood %>% 
      filter(subset_Neighborhood$CommName == SelectedCommunity)
    
    subset_N_one <- subset_N %>% 
      filter(subset_N$Name == SelectedNeighborhood)
    
    neighborhood_popup <- paste0("<strong>Neighborhood: </strong>", 
                                 subset_N$Name,
                                 "<br/><strong>Rate of homes predicted to have lead: </strong>", 
                                 round(subset_N$Rate, digits = 2))
    
    #second dropdown
    output$SelectedNeighborhood <- renderUI({
      #selectInput("", choices =  c("All", filter(subset_Neighborhood$CommName == SelectedCommunity)))
      selectInput(inputId = "SelectedNeighborhood",         #drop down menu -
                  label = "Neighborhood",
                  selected = FALSE,           #how many can select
                  selectize = FALSE,
                  multiple = FALSE,
                  choices = c("All",sort(unique(subset_N$Name)))
      )
    })
    
    #paletteContinuousC <- colorNumeric(palette = "Blues", domain = Combo_dataset$Rate)
    #paletteContinuousN <- colorNumeric(palette = "Blues", domain = subset_N$Rate)                            # not working!!!
    
    #&                  #filter
    #hour_ >= range[1] & hour_ <= range[2])
    
    # subset_marker_popup <- paste0("<strong>Dispatch Date & Time: </strong>", 
    # subset_N$Name, 
    #  "<br><strong>Location: </strong>", 
    # subset_N$Rate
    
    #Output a new summary plot
    output$neighbourhoodPlot <- renderPlot({
      if(SelectedCommunity =="All"){
        return(ggplot(subset_Community, aes(x = Name, y = Rate)) +
                 geom_bar(stat="identity", fill = "#477371")+                   #aes(fill = Rate))    #Xiao - palette in here
                 xlab("ALl communities")+
                 ylab("Rate of Predicted Lead Homes")+
                 theme(axis.text.x = element_text(angle=60, hjust=1),
                       panel.background = element_rect("white"))
        )}
      ggplot(subset_N, aes(x = Name, y = Rate)) +
        geom_bar(stat="identity",
                 fill=ifelse(subset_N$Name == SelectedNeighborhood,
                             "yellow",
                             "#477371"))+                                  #Xiao - palette in here
        xlab(SelectedCommunity)+
        ylab("Rate of Predicted Lead Homes")+
        theme(axis.text.x = element_text(angle=60, hjust=1),
              panel.background = element_rect("white"))
    })
    
    #output a new DataTable
    # output$table <- renderDataTable(forshiny_N)  
    
    #re-draw map
    #leafletProxy("myratemap") %>% 
    output$myratemap <- renderLeaflet({
      if(SelectedCommunity =="All"){
        return(leaflet(subset_Community) %>% addProviderTiles(providers$CartoDB.Positron) %>%
                 setView(-93.2650, 44.9778, zoom = 11) %>% 
                 addPolygons( data = subset_Community,
                              fillColor = ~paletteContinuousC(Rate),
                              weight = 0.8,
                              opacity = 0.6,
                              smoothFactor = 0.1,
                              #color = ~paletteBins(~Rate),
                              color = "white",
                              fillOpacity = 0.8,
                              #label = ~paste0("Rate: "),
                              highlight = highlightOptions(
                                fillColor = "yellow",
                                weight = 2,
                                bringToFront = TRUE), 
                              popup = community_popup)
        )}
      if(SelectedNeighborhood == "All"){
        return(leaflet(subset_N) %>% addProviderTiles(providers$CartoDB.Positron) %>%
                 clearShapes() %>% 
                 setView(lng=subset_C$lon, lat=subset_C$lat, zoom = 13) %>%               #-93.2650, 44.9778, zoom = 11  -subset_N$lat, subset_N$lon, zoom = 8
                 addPolygons( data = subset_N,
                              fillColor = ~paletteContinuousN(Rate),
                              weight = 0.8,
                              opacity = 0.6,
                              smoothFactor = 0.1,
                              #color = ~paletteBins(~Rate),
                              color = "white",
                              fillOpacity = 0.8,
                              #label = ~paste0("Rate: "),
                              highlight = highlightOptions(
                                fillColor = "yellow",
                                weight = 2,
                                bringToFront = TRUE), 
                              popup = neighborhood_popup)
        )}
      else {
        leaflet(subset_N) %>% addProviderTiles(providers$CartoDB.Positron) %>%
          clearShapes() %>% 
          setView(lng=subset_C$lon, lat=subset_C$lat, zoom = 13) %>%               #-93.2650, 44.9778, zoom = 11  -subset_N$lat, subset_N$lon, zoom = 8
          addPolygons( data = subset_N,
                       #color = "yellow",
                       fillColor = ~paletteContinuousN(Rate),
                       weight = 0.8,
                       opacity = 0.6,
                       smoothFactor = 0.1,
                       #color = ~paletteBins(~Rate),
                       color = "white",
                       fillOpacity = 0.8,
                       #label = ~paste0("Rate: "),
                       highlight = highlightOptions(
                         fillColor = "yellow",
                         weight = 1.2,
                         bringToFront = TRUE), 
                       popup = neighborhood_popup)%>%
          addPolygons(data = subset_N_one,
                      fillColor = "yellow",
                      color = "white",
                      fillOpacity = 1,
                      opacity = 1,
                      weight = 1.2,
                      highlight = highlightOptions(color = "white", weight = 1.2,
                                                   bringToFront = TRUE))
      } 
    })
  }) 
}


#shinyApp(ui, server)
