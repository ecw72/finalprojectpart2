library(devtools)
devtools::install_github("ropensci/rnaturalearthhires")
library(dplyr)
library(osmdata)
library(rnaturalearth)
library(shiny)
library(leaflet)
library(readxl)
library(ggmap)
library(leaflet)
library(leaflet.extras)
library(rsconnect)
library(openxlsx)
library(sf)
library(caret)
library(data.table)


register_google(key = "AIzaSyD4XfNNw8gn5l56R0HnriIzWSONtdRfUt0")
# 
# nj_bbox <- c(left = -75.562, bottom = 38.928, right = -73.885, top = 41.357)
# 
# fast_food_data <- opq(bbox = nj_bbox) %>%
#   add_osm_feature(key = 'amenity', value = 'fast_food') %>%
#   osmdata_sf()
# 
# filtered_data <- fast_food_data$osm_points %>%
#   filter(!is.na(name))
# head(filtered_data)
# 
# modified_data <- filtered_data %>%
#   rename(
#     Block_number = `addr:block_number`,
#     City = `addr:city`,
#     Country = `addr:country`,
#     County = `addr:county`,
#     Housenumber = `addr:housenumber`,
#     State = `addr:state`,
#     Postcode = `addr:postcode`,
#     Street = `addr:street`
#   ) %>%
#   select(osm_id, name, Block_number, City, Country, County, Housenumber, State, Postcode, Street, geometry)
# 
# 
# sf_data <- st_as_sf(modified_data, wkt = "geometry")
# saveRDS(sf_data, "sf_data.rds")
sf_data <- readRDS("sf_data.rds")

# setwd("/Users/ellawalmsley/finalproject")
# 
# grocery_data <- read_xlsx("AuthGroceryVendorsNJ.xlsx")
# head(grocery_data)
# 
# api_key_path <- "/Users/ellawalmsley/Downloads/Untitled document.txt"
# api_key <- readLines(api_key_path, warn = FALSE)
# api_key <- trimws(api_key[1])
# 
# if (api_key == "") {
#   stop("API key is missing. Check the contents of the file.")
# }
# 
# register_google(key = api_key)
# 
# 
# grocery_data <- grocery_data %>%
#   mutate(FullAddress = paste(Address, City, State, Zip, sep = ", "))
# 
# 
# geocode_address <- function(address) {
#   result <- geocode(address, output = "latlona", source = "google")
#   return(result)
# }
# 
# geocode_address <- function(address) {
#   result <- ggmap::geocode(address)
#   return(result)
# }
# 
# grocery_data <- grocery_data %>%
#   rowwise() %>%
#   mutate(GeoCode = list(geocode_address(FullAddress)),
#          Latitude = GeoCode$lat,
#          Longitude = GeoCode$lon) %>%
#   select(-GeoCode, -FullAddress)
# 
# print(grocery_data)
# 
# write.xlsx(grocery_data, file = "grocery_data_cords.xlsx", rowNames = FALSE)
grocery_data_cords <- read_excel("grocery_data_cords.xlsx")
head(grocery_data_cords)


grocery_data_clean <- grocery_data_cords %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

grocery_data_sf <- st_as_sf(grocery_data_clean, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

sf_data <- st_as_sf(sf_data, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) 

grocery_df_map <- grocery_data_sf %>%
  select(Name, Latitude, Longitude)

fastfood_df_map <- sf_data %>%
  rename(Name = name) %>%
  select(Name, geometry)

grocery_df_map$Category <- "Grocery"
fastfood_df_map$Category <- "Fast Food" 

grocery_df_map <- grocery_df_map %>%
  select(Name, Category, geometry)

fastfood_aligned <- fastfood_df_map %>% select(Name, Category, geometry)
grocery_aligned <- grocery_df_map %>% select(Name, Category, geometry)

combined_df_map <- rbind(fastfood_aligned, grocery_aligned)

str(combined_df_map)

head(combined_df_map)

states <- ne_states(country = "united states of america", returnclass = "sf")
nj_boundaries <- states[states$name == "New Jersey",]

head(combined_df_map)

zipcode_data <- read.csv("PLACES__ZCTA_Data__GIS_Friendly_Format___2023_release copy.csv")
head(zipcode_data)
income_data <- read_excel("incomedata.xlsx")

zipcode_data <- zipcode_data %>% rename(Zip = ZCTA5)


zipcode_data <- zipcode_data %>% select(Zip, Geolocation)


merged_data <- left_join(income_data, zipcode_data %>% select(Zip, Geolocation), by = "Zip")

merged_data <- merged_data %>% filter(!is.na(Geolocation))

merged_data <- merged_data %>%
  mutate(geometry = st_as_sfc(Geolocation, crs = 4326))

merged_data_sf <- st_as_sf(merged_data, crs = 4326, agr = "constant")

merged_data_sf <- merged_data_sf %>% select(-Geolocation)

merged_data_sf <- st_as_sf(merged_data, crs = 4326, agr = "constant")


coords <- st_coordinates(merged_data_sf)
populations <- merged_data_sf$Population

distances_to_nj <- st_distance(combined_df_map, nj_boundaries)
distances_to_nj_numeric <- as.numeric(distances_to_nj)
max_distance <- 1 * 1609.34
combined_df_map <- combined_df_map[distances_to_nj_numeric <= max_distance, ]


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
