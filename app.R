library(rsconnect)

source("take2.R")

ui <- fluidPage(
  titlePanel("Fast Food Spots and Grocery Stores in New Jersey"),
  sidebarLayout(
    sidebarPanel(
      textInput("street", "Street Address", "290 George St"),
      textInput("city", "City", "New Brunswick"),
      textInput("state", "State", "NJ"),
      textInput("zip", "ZIP Code", "08901"),
      actionButton("go", "Zoom in!"),
      actionButton("reset_view", "Reset View"),
      selectInput("heatmapType", "Toggle Population Heatmap:", 
                  choices = list("Off" = "off", "Population" = "population"), selected = "off"),
      HTML("<h3>What does this do?</h3>
           <p>This map shows fast food and grcoery store locations with a popualtion heatmap that can be toggled on or off. Enter you address to see all stores within a five mile radius! Click on store points for names and designation.</p>
           <h3>Where does this data come from?</h3>
          <p>The fast food data is from Open Street Maps and the Grocery Data is the NJ Department of Agriculture. The NJDA defines a supermarket or a grocery store as 'a retail outlet with at least 16,000 square feet, of which at least 80 percent is occupied by food and related products, which products shall be based on industry standards, as determined by the authority, except that the food and related products shall not include alcoholic beverages and products related to the consumption of such beverages', so that's why you might see some unexpected spots!.</p>
")
    ),
    mainPanel(
      leafletOutput("njMap", height = "700px")
    )
  )
)

server <- function(input, output) {
  output$njMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -74.5, lat = 40.0, zoom = 8) %>%
      addPolygons(
        data = nj_boundaries, 
        color = "#444444",
        weight = 2,
        fillOpacity = 0.2,  
        fillColor = "transparent"
      ) %>%
      addCircleMarkers(
        data = combined_df_map,
        color = ~ifelse(Category == "Fast Food", "blue", "green"),
        popup = ~paste(Name, "<br>", Category),
        opacity = .7, fillOpacity = 0.6, radius = .5
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("blue", "green"),
        labels = c("Fast Food", "Grocery"),
        title = "Category"
      )
  })
  
  observeEvent(input$go, {
    full_address <- paste(input$street, input$city, input$state, input$zip, sep = ", ")
    geocode_result <- ggmap::geocode(full_address, output = "latlona", source = "google")
    
    if (!is.na(geocode_result$lat) && !is.na(geocode_result$lon)) {
      leafletProxy("njMap") %>%
        addMarkers(lng = geocode_result$lon, lat = geocode_result$lat) %>%
        setView(lng = geocode_result$lon, lat = geocode_result$lat, zoom = 12)
    } else {
      showNotification("Could not geocode the address. Please try again.", type = "error")
    }
  })
  
  observeEvent(input$reset_view, {
    leafletProxy("njMap") %>% setView(lng = -74.5, lat = 40.5, zoom = 8)
  })
  
  observe({
    leafletProxy("njMap") %>% clearGroup("Population Heatmap")
    
    if(input$heatmapType == "population") {
      req(nrow(merged_data_sf) > 0)
      
      coords <- st_coordinates(merged_data_sf$geometry)
      population <- merged_data_sf$Population  
      
      leafletProxy("njMap") %>%
        addHeatmap(lng = coords[,1], lat = coords[,2], intensity = population, radius = 20, blur = 15, group = "Population Heatmap")
    }
  })
  
}

shinyApp(ui = ui, server = server)




