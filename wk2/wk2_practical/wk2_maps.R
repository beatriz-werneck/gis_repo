library(dplyr)
library(janitor)
library(OpenStreetMap)
library(Rcpp)
library(readr)
library(sf)
library(stringr)
library(tmap)
library(tmaptools)

#read the geojson we downloaded 
EW <- st_read("/Users/beatrizwerneck/Documents/URBAN_SPATIAL_SCIENCES/GIS/wk2/practical_2/data_practical_2/LAD_Dec_2015_FCB_GB_2022.geojson")

# THIS FILE SOULD BE CONNECTED WITH THE FIRST FILE OF THE PRACTICAL 2, SO IT CAN USE THEIR OBJECTS? HOW TO DO THIS? 
#____________________________________________________________________________________________________________________
#read the data in straight from the web
LondonData <- read_csv("https://data.london.gov.uk/download/f33fb38c-cb37-48e3-8298-84c0d3cc5a6c/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a")

#plot London map
LondonMap <- EW %>%
  filter(str_detect(lad15cd, "^E09"))

qtm(LondonMap)

#clean up all the names with janitor
LondonData <- clean_names(LondonData)

BoroughtDataMap <- EW %>%
  clean_names() %>%
  filter(str_detect(lad15cd, "^E09")) %>%
  merge(.,
        LondonData,
        by.x = "lad15cd",
        by.y = "new_code",
        no.dups = TRUE) %>%
  distinct(., lad15cd,
           .keep_all = TRUE)

# READ OSM DOESN'T WORK !!!! FATAL ERROR -> ASK IN CLASS 
#------------------------------------------------------------
#tmap_mode("plot")

#qtm(BoroughtDataMap,
#    fill = "employment_rate_16_64_2011")

#using a base map for create a static map
#tmaplondon <- BoroughtDataMap %>%
  # st_bbox gives the bounding x and y coordinates 
#  st_bbox(.) %>%
#  tmaptools::read_osm(., type = "esri", zoom = NULL)

tmap_mode("view")

tmaplondon <- tm_shape(BoroughtDataMap) +
  tm_borders()

tmap_mode("plot")

tm_shape(BoroughtDataMap) +
tm_polygons(fill = "employment_rate_16_64_2011",
            fill.scale = tm_scale_intervals(values = "brewer.bu_pu",
                                            style = "jenks"),
            fill_alpha = 0.5,
            fill.legend = tm_legend(title = "Employment rate 2011",
                                    size = 0.8)) +
tm_compass(type = "arrow", position = c("left", "bottom")) +
tm_scalebar(position = c("left", "bottom")) +
tm_title("Employment rate 2011",
         size = 1,
         position = c("left", "top"))
