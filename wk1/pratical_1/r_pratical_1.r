# Load data 
## Import packages
library(sf)
library(tidyverse)
library(tmap)
library(readr)
library(SQLite)

## Declare variables
shape <- st_read("/Users/andrewerneck/Documents/URBAN_SPATIAL_SCIENCES/GIS/wk1/pratical_1/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")
mycsv <- read.csv("/Users/andrewerneck/Documents/URBAN_SPATIAL_SCIENCES/GIS/wk1/pratical_1/fly_tipping_borough_edit.csv")

# Join data
shape <- shape%>%
  merge(.,
        mycsv,
        by.x="GSS_CODE",
        by.y="code")

# Plot a tematic map
#! The plot only apeared when running this code in the console (is there an explaination for it?)
#! Also the prot just represent the values in different colours, and did not creat the intervals (ex: 0 to 5000, 5000 to 10000, etc). How can I do it?
tmap_mode("plot")
shape %>%
  qtm(., fill = "X2011.12")

# Export data
## Write data to a .gpkg
shape %>%
  st_write(., "/Users/andrewerneck/Documents/URBAN_SPATIAL_SCIENCES/GIS/wk1/pratical_1/r_pratical_1.gpkg",
           "london_boroughs_fly_tipping",
           delete_layer=TRUE)

## Connect to the .gpkg
con <- dbConnect(RSQLite::SQLite(),dbname="/Users/andrewerneck/Documents/URBAN_SPATIAL_SCIENCES/GIS/wk1/pratical_1/r_pratical_1.gpkg")

# List what is in the .gpkg
con %>%
  dbListTables()

# Add the original .csv
con %>%
  dbWriteTable(.,
               "original_csv",
               mycsv,
               overwrite=TRUE)

# disconnect from it
con %>% 
  dbDisconnect()

