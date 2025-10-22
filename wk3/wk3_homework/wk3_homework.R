#import libraries 
library(dplyr)
library(fs)
library(here)
library(raster)
library(sf)
library(stringr)
library(terra)
library(tidyverse)

#VECTOR DATA
#check geopackage layers 
st_layers(here("wk3_hw_data", "gadm41_BRA.gpkg"))

#read geopackage layer for brazil outline
bra_outline <- st_read(here("wk3_hw_data", "gadm41_BRA.gpkg"),
                         layer = 'ADM_ADM_0')

#reproject data for local projected CRS
bra_outline_prj <- bra_outline %>% 
  st_transform(.,5641)

print(bra_outline_prj)

#simplify brazil outline
bra_outline_simple <- bra_outline_prj$geom %>% 
  st_simplify(.,dTolerance = 1000)

plot(bra_outline_simple)
print(bra_outline_simple)

#CSV DATA
#read csv with coordinates from Brazilian key cities 
key_cities_world <- read.csv(here::here("wk3_hw_data/", "World_Cities.csv"),
                             header = TRUE, 
                             sep = ",",
                             encoding = "latin1",
                             na = "n/a")

key_cities_bra <- key_cities_world %>% 
  filter(str_detect(key_cities_world$FIPS_CNTRY, 'BR'))

sites <- key_cities_bra[,c(1,2)]
rownames(sites) <- key_cities_bra$CITY_NAME

#convert to spatial vector
city_points <- vect(sites, geom = c("X", "Y"), crs = "EPSG:5641")
city_points

#RASTER DATA
#load all of the data into a SpatRaster (collection of raster layers with the same spatial extent and resolution)
tif_files <- dir_info("wk3_hw_data/") %>% 
  filter(str_detect(path, ".tif")) %>% 
  dplyr::select(path) %>% 
  pull()

max_temp_world <- tif_files %>% 
  terra::rast()

print(max_temp_world)

#reproject data for the local projected CRS
max_temp_world_prj <- terra::project(max_temp_world, "EPSG:5641")

max_temp_world_prj

#organize the SpatRaster layer names 
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

ssp <- c("SSP_126", "SSP_245", "SSP_370", "SSP_585")

layer_combinations <- expand.grid(Month = month, SSP = ssp) 

layer_names <- paste(layer_combinations$SSP, layer_combinations$Month, sep = "_")

names(max_temp_world_prj) <- layer_names

print(max_temp_world_prj)
plot(max_temp_world_prj)

#extract the data form the SparRaster for all the brazilian key cities 
max_temp_bra <- terra::extract(max_temp_world_prj, city_points)

#add the city names to the rows
max_temp_bra_2 <- max_temp_bra %>% 
  as_tibble() %>% 
  add_column(Site = key_cities_bra$CITY_NAME, .before = "SSP_126_Jan")

bra_temp <- bra_outline_simple %>% 
  terra::crop(max_temp_world_prj, .)

plot(bra_temp)

exact_bra <- terra::mask(bra_temp, bra_outline)
plot(exact_bra)
