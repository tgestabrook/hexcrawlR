#

library(shiny)
library(leaflet)
library(terra)
library(sf)
library(tidyverse)
library(tidyterra)

### Global variables
#https://www.ncei.noaa.gov/products/etopo-global-relief-model
world_elev <- rast("../ETOPO_2022_v1_60s_N90W180_bed.tif")
#https://data.apps.fao.org/map/catalog/srv/eng/catalog.search#/metadata/ba4526fd-cdbf-4028-a1bd-5a559c4bff38
world_lc <- rast("../GLC_SHARE_DominantLC.tif")

lc_legend <- as.data.frame(cbind(
  'key' = c(1:11),
  'label' = c('Urban', 'Cropland', 'Grassland', 'Forest', 'Scrub', 'Wetland', 'Mangrove', 'Barren', 'Desert', 'Snow', 'Water')
))
# #http://ihp-wins.unesco.org/layers/geonode:world_rivers
# world_river <- st_read("H:\\My Drive\\RPGs\\Worldbuilding\\Hexcrawler\\unesco_rivers.geojson")


stupid_join_function <- function(vec){
  outvec <- c()
  for (elt in vec){
    if (!(as.integer(elt) %in% lc_legend$key)){outvec <- c(outvec, NA)}
    else {outvec <- c(outvec, lc_legend$label[lc_legend$key == as.integer(elt)])}
  }
  return(outvec)
}

marker_coord <- NULL
terrainCols <- c('skyblue2', 'darkolivegreen3', 'tan3', 'ivory4')
terrainPal <- colorFactor(palette = terrainCols, domain=c('Water', 'Flatlands', 'Hills', 'Mountains'), na.color = "#FFFFFF00", ordered=TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("HexCrawlR"),

    # Sidebar with a slider input for sea level 
    sidebarLayout(
      sidebarPanel(
        "Select the desired extent and sea level of your hexcrawl grid, then click the map to designate the center of your desired area. \n\n
        Hexcrawler will automatically sample real-world elevation, terrain roughness, and landcover.",
        sliderInput('hex_size', 'Hex Size (mi)', min = 3, max = 30, value = 6),
        sliderInput('x_ext', "East-West extent (mi)", min = 6, max = 300, value = 30),
        sliderInput('y_ext', "North-South extent (mi)", min = 6, max = 300, value = 30),
        sliderInput("sealevel",
                    "Sea Level:",
                    min = -1000,
                    max = 5000,
                    value = 0,
                    step = 10),
        actionButton('generate', 'Generate hexes'),
        downloadButton('download_shp', 'Download Hexes')
      ),

        # Show a plot of the generated distribution
      mainPanel(
        leafletOutput('WorldMap', width='100%', height='650')
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$WorldMap <- renderLeaflet({
    leaflet() %>%
      addMapPane('base_layers', 410) %>%  # This ensures the base layers will render below the clickable polygon layer
      addMapPane('poly_layer', 450) %>%
      addTiles(group = "OSM (default)") %>%
      #addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      #addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addProviderTiles(providers$OpenTopoMap, group = "Open Topo Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") %>%
      addLegend(position = 'bottomright', pal = terrainPal, values = c('Water', 'Flatlands', 'Hills', 'Mountains'))
  })
  
  observeEvent(input$WorldMap_click, {
    leafletProxy('WorldMap') %>%
      clearMarkers() %>%
      addMarkers(lng = input$WorldMap_click$lng, lat = input$WorldMap_click$lat, layerId = 'map_center')
    marker_coord <<- c('lng' = input$WorldMap_click$lng, 'lat' = input$WorldMap_click$lat)
  })
  
  observeEvent(input$generate, {
    # check if markers exist
    if(is.null(marker_coord)) {return()}
    if(input$x_ext * input$y_ext > 5000){showNotification('Large map extent - this may take some time.', '', duration = 5, type = 'warning')}

    marker_coord <- st_point(cbind(input$WorldMap_click$lng, input$WorldMap_click$lat))

    lldf <- as.data.frame(cbind(input$WorldMap_click$lng, input$WorldMap_click$lat))
    colnames(lldf) <- c('lng', 'lat')
    
    marker_point <- st_as_sf(lldf, coords = c('lng', 'lat'), crs = 4326) %>%
      st_transform(4087)
    
    print(marker_point$geometry)

    n_pt <- marker_point$geometry + c(0, input$y_ext*1609/2)
    s_pt <- marker_point$geometry + c(0, input$y_ext*-1609/2)
    e_pt <- marker_point$geometry + c(input$x_ext*1609/2, 0)
    w_pt <- marker_point$geometry + c(input$x_ext*-1609/2, 0)
    
    bb_test <- st_bbox(c(n_pt, s_pt, e_pt, w_pt)) 
    
    bb_poly <- st_as_sfc(bb_test) %>%
      st_as_sf(crs = 4087)
    
    hexes <- st_make_grid(
      bb_poly,
      cellsize = c(input$hex_size*1609,input$hex_size*1609),
      what = "polygons",
      square = FALSE,
      flat_topped = TRUE
    ) %>%
      st_transform(4326)
    
    hexes <- vect(hexes)
    
    hex_mean <- terra::extract(world_elev, hexes, fun = mean, bind=TRUE, na.rm = TRUE) %>%
      rename(mean_elev = ETOPO_2022_v1_60s_N90W180_bed)
    
    hex_lc <- terra::extract(world_lc, hex_mean, fun = raster::modal, bind = TRUE, na.rm = TRUE) %>%
      mutate(biome = stupid_join_function(GLC_SHARE_DominantLC))
    
    hex_std <- terra::extract(world_elev, hex_lc, fun=sd, bind=TRUE) %>%
      rename(sd_elev = ETOPO_2022_v1_60s_N90W180_bed)
    
    hex_std <- hex_std %>%
      mutate(Terrain = as.factor(ifelse(mean_elev < input$sealevel, 'Water', 
                            ifelse(sd_elev < 150, 'Flatlands',
                                    ifelse(sd_elev < 300, 'Hills',
                                          'Mountains')))))
    
    #river_hexes <- relate(hex_std, world_river, relation = 'intersects')
    #print(river_hexes)
    
    hex_std <- sf::st_as_sf(hex_std)
    hex_out <<- hex_std
    
    #hex_out$River <- hex_out[world_river]
    map_bounds <- as.character(st_bbox(hex_out))
    
    leafletProxy('WorldMap') %>%
      clearShapes() %>%
      addPolygons(data=hex_std,
                  color = 'black',
                  weight = 0.5,
                  opacity = 0.9,
                  fillOpacity = 0.75,
                  fillColor = ~terrainPal(hex_std[['Terrain']])) %>%
      fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4])
  })
  
  output$download_shp <- downloadHandler(
    
    filename <- function() {"Data_shpExport.zip"},
    content = function(file) {
      withProgress(message = "Exporting Data", {
        
        incProgress(0.5)
        tmp.path <- dirname(file)
        
        name.base <- file.path(tmp.path, "HexCrawlerOutput")
        name.glob <- paste0(name.base, ".*")
        name.shp  <- paste0(name.base, ".shp")
        name.zip  <- paste0(name.base, ".zip")
        
        print(tmp.path)
        print(name.glob)
        
        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        hex_out %>%
          sf::st_write(dsn = name.shp, ## layer = "shpExport",
                       driver = "ESRI Shapefile", quiet = TRUE)
        
        zip::zipr(zipfile = name.zip, files = Sys.glob(name.glob))
        req(file.copy(name.zip, file))
        
        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        
        incProgress(0.5)
      })
    }  
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
