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

#https://www.ncei.noaa.gov/products/etopo-global-relief-model
world_elev <- rast("H:\\My Drive\\RPGs\\Worldbuilding\\Hexcrawler\\ETOPO_2022_v1_60s_N90W180_bed.tif")
test_area <- vect("H:\\My Drive\\RPGs\\Worldbuilding\\Hexcrawler\\test_AOI.gpkg")

# function to create a hex grid over an area, sample elevation
hexify <- function(object){
  hexes <- st_make_grid(
    object,
    cellsize = c(9656,9656),
    what = "polygons",
    square = FALSE,
    flat_topped = TRUE
  )
  hexes <- vect(hexes)
  hex_mean <- extract(world_elev, hexes, fun = mean, na.rm = TRUE)
  return(hex_mean)
}

hexify(test_area)

# function to sample major rivers(?)

# function to 