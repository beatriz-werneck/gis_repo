#import libraries
library(dplyr)
library(janitor)
library(readr)
library(sf)
library(stringr)
library(tidyverse)
library(tmap)
library(tmaptools)

#read csv in straight from the web using read_csv 
#skipping over the 'n/a' entries
report_card_data <-  read_csv("https://data.wa.gov/api/views/5y3z-mgxd/rows.csv?accessType=DOWNLOAD",
                              na = c("NULL", "NA", "n/a", ""))

#exam data (class, type, summary, columns names, rows names)
class(report_card_data)

data_type_list <- report_card_data %>%
  summarise_all(class) %>%
  pivot_longer(everything(),
               names_to = "all_variables",
               values_to = "variable_class")
print(data_type_list, n=31)

summary(report_card_data)

report_card_data %>%
  colnames() %>%
  head(n=31)

report_card_data %>%
  rownames() %>%
  head(n=31)

report_card_data %>%
  distinct(County)

#create new df excluding non analysed data
science_report_card_data <- report_card_data[,c(3,18,20,22)] %>%
  filter(str_detect(TestSubject, "Science"))

#group by county, summarize by sum and calculate % that met standards 
county_science_average <- science_report_card_data %>%
  group_by(County) %>%
  summarise(
    county_std_expected = sum(`Count of Students Expected to Test`, na.rm = TRUE),
    county_std_met_standard = sum(CountMetStandard, na.rm = TRUE)
  ) %>%
  mutate(
    percent_met = case_when(
      county_std_expected == 0 & county_std_met_standard == 0 ~ NA_real_,
      county_std_expected > 0 ~ round((county_std_met_standard / county_std_expected)*100,2),
      TRUE ~ NA_real_
    )
  )

#remove NA values
county_science_average <- county_science_average %>%
  filter(!is.na(percent_met))

#calculate Washington average 
washington_avg <- mean(county_science_average$percent_met, na.rm = TRUE)

#add new columns with Washington avg and comparison
county_science_average <- county_science_average %>%
  mutate(
    washington_avg = washington_avg,
    comparison_washington_avg = case_when(
      percent_met > washington_avg ~ "above average",
      percent_met < washington_avg ~ "bellow average",
      TRUE ~ "equal to average"
    )
  )

#read shapefile 
washington_countie <- st_read("/Users/beatrizwerneck/Documents/URBAN_SPATIAL_SCIENCES/GIS/wk2/wk2_homework/wk2_homework_data/Washington_Counties_with_Natural_Shoreline___washsh_area")

#plot Washington counties map
tmap_mode("plot")
qtm(washington_countie)


#clean up the names with janitor
county_science_average <- clean_names(county_science_average)

#merge shapefile and data
counties_data_map <- washington_countie %>% 
  merge(.,
        county_science_average,
        by.x = "COUNTYLABE",
        by.y = "county",
        no.dups = TRUE) %>% 
  distinct(., COUNTYLABE,
           .keep_all = TRUE)

#plot thematic map
qtm(counties_data_map,
    fill = "comparison_washington_avg")
  