# Data to incorporate
# Elevation (set sea level)
# Rivers
# Precipitation -> climate/biome

# User toggles:
# Set sea level
# Randomize biomes?
# Randomize settlements?
library(raster)
library(sf)
library(terra)
library(tidyverse)
library(tidyterra)

#https://www.ncei.noaa.gov/products/etopo-global-relief-model
world_elev <- rast("H:\\My Drive\\RPGs\\Worldbuilding\\Hexcrawler\\ETOPO_2022_v1_60s_N90W180_bed.tif")
test_area <- st_read("H:\\My Drive\\RPGs\\Worldbuilding\\Hexcrawler\\test_AOI.gpkg") %>%
  st_transform('EPSG:4087')

# function to create a hex grid over an area, sample elevation
hexify <- function(object){
  hexes <- st_make_grid(
    object,
    cellsize = c(9656,9656),
    what = "polygons",
    square = FALSE,
    flat_topped = TRUE
  )
  hexes <- vect(hexes) %>% project(crs(world_elev))
  plot(hexes)
  hex_mean <- terra::extract(world_elev, hexes, fun = mean, bind=TRUE, na.rm = TRUE) %>%
    rename(mean_elev = ETOPO_2022_v1_60s_N90W180_bed)
  hex_std <- terra::extract(world_elev, hex_mean, fun=sd, bind=TRUE) %>%
    rename(sd_elev = ETOPO_2022_v1_60s_N90W180_bed)
  return(hex_std)
}

test_hex <- hexify(test_area) 

plot(test_hex)

plot(project(vect(test_area), crs(world_elev)))
plot(test_hex, add = TRUE)

# function to sample major rivers(?)

# function to 