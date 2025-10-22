library(sf)
library(RSQLite)
library(tidyverse)
library(tmap)
library(tmaptools)
library(readr)

shape_hw_1 <- st_read("/Users/andrewerneck/Documents/URBAN_SPATIAL_SCIENCES/GIS/wk1/homework_1/statsnz-territorial-authority-2018-generalised-SHP/territorial-authority-2018-generalised.shp")

csv_hw_1 <- read_csv("/Users/andrewerneck/Documents/URBAN_SPATIAL_SCIENCES/GIS/wk1/homework_1/nz_employment_status_census_2018.csv")

shape_hw_1 <- shape_hw_1 %>%
  merge(.,
        csv_hw_1,
        by.x="TA2018_V1_",
        by.y="Territorial authority code (2018 areas)")

tmap_mode("plot")

shape_hw_1 %>%
  qtm(., fill = "Paid employee")

shape_hw_1 %>%
  st_write(., "/Users/andrewerneck/Documents/URBAN_SPATIAL_SCIENCES/GIS/wk1/homework_1/r_homework_1.gpkg",
           "nz_employment_status_census_2018",
           delete_layer = TRUE)

connection <- dbCanConnect(RSQLite::SQLite(),dbname="/Users/andrewerneck/Documents/URBAN_SPATIAL_SCIENCES/GIS/wk1/homework_1/r_homework_1.gpkg")

connection %>%
  dbListTables()

connection %>%
  dbWriteTable(.,
               "original_csv",
               csv_hw_1,
               overwrite=TRUE)
# disconnect from it
connection %>% 
  dbDisconnect()

