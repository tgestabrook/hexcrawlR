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

test_hex <- test_hex %>%
  mutate(
    Terrain = ifelse(mean_elev < 0, 'Water', 
                     ifelse(sd_elev < 150, 'Flatland',
                            ifelse(sd_elev < 300, 'Hills',
                                   'Mountains')))
  ) 

plot(test_hex, 'Terrain')
plot(test_hex, 'sd_elev')

plot(project(vect(test_area), crs(world_elev)))
plot(test_hex, add = TRUE)

test_hex2 <- st_as_sf(test_hex) %>%
  st_transform(crs(world_river))
test_hex2[world_river]

plot(test_hex2[world_river], col='red', add = TRUE)

longlat <- c(c(33.04688), c(44.08))

lldf <- as.data.frame(cbind(33.04, 44.08))
colnames(lldf) <- c('lon', 'lat')

marker_pt <- st_as_sf(lldf, coords = c('lon', 'lat'), crs = 4326) %>% st_transform(4087)

n_pt <- marker_pt$geometry + c(0, 30*1609/2)
s_pt <- marker_pt$geometry + c(0, 30*-1609/2)
e_pt <- marker_pt$geometry + c(30*1609/2, 0)
w_pt <- marker_pt$geometry + c(30*-1609/2, 0)

bb_test <- st_bbox(c(n_pt, s_pt, e_pt, w_pt)) 

bb_poly <- st_as_sfc(bb_test) %>%
  st_as_sf(crs=4087) %>%
  st_transform(4326)
# function to sample major rivers(?)

# function to 