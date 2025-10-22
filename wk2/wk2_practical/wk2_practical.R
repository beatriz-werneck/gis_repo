#import packages
library(tidyverse)
library(tidyr)
library(dplyr)
library(here)
library(readr)
library(stringr)
library(janitor)
library(plotly)

#read data in R from a cleaned csv 
#LondonDataOSK <- read.csv("ward-profiles-excel-version.csv", 
#                          sep=",")

#read data direct from the web and clean it using tidyverse
LondonData <- read_csv("https://data.london.gov.uk/download/f33fb38c-cb37-48e3-8298-84c0d3cc5a6c/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a")

#check if the data has been read in correctly
Datatypelist <- LondonData %>%
  summarise_all(.,class) %>%
  pivot_longer(everything(),
               names_to="All_variables",
               values_to="Variable_class")

#selecting the london borough rows 
LondonBoroughs <- LondonData[626:658,] 
#LondonBoroughs <- LondonData %>%
#  filter(str_detect('New code', "^E09")) -> there is a coolest way of doing it: codes for London Boroughs start with E09 (the wards in the rest of the file start with E05).

#filtering the wards where female life expectancy is greater than 90
Femalelifeexp <- LondonBoroughs %>%
  filter('Female life expectancy -2009-13'>90)

#eliminating replicated rows 
LondonBoroughs <- LondonBoroughs %>%
  distinct()

#select columns 1,19,20 and 21
#LondonBoroughs_manualcols <- LondonBoroughs[,c(1,19,20,21)]
LondonBoroughs_dplyrcols <- LondonBoroughs %>%
  dplyr::select(c(1,19,20,21))

#select columns using contains 
LondonBoroughs_contains <- LondonBoroughs %>%
  dplyr::select(contains("expectancy"),
                contains("obese - 2011/12 to 2013/14"),
                contains("Ward name"))

#renaming columns 
LondonBoroughs <- LondonBoroughs %>%
  dplyr::rename(Borough='Ward name') %>%
  clean_names()

#changing names for every word having a capital letter
LondonBoroughs <- LondonBoroughs %>%
  clean_names(., case='big_camel')

#manipulating data 
Life_expectancy <- LondonBoroughs %>%
  #new column with average of male and female life expectancy
  mutate(averangelifeexpectancy = (FemaleLifeExpectancy2009_13 +
                                     MaleLifeExpectancy2009_13)/2) %>%
  #new column with normalised life expectancy
  mutate(normalisedlifeexpectancy = averangelifeexpectancy /
           mean(averangelifeexpectancy)) %>%
  #select only columns we want
  dplyr::select(NewCode, 
                Borough,
                averangelifeexpectancy,
                normalisedlifeexpectancy) %>%
  #arrange in descending order
  #ascending is the default and would be
  #arrange(normalisedlifeepectancy)
  arrange(desc(normalisedlifeexpectancy))

#visualizing the top of the data
slice_head(Life_expectancy, n=5)

#visualizing the bottom of the data
slice_tail(Life_expectancy, n=5)

#comparing data 
Life_expectancy_2 <- Life_expectancy %>%
  mutate(UKcompare = case_when(averangelifeexpectancy > 81.16 ~ "above UK average",
                               TRUE ~ "bellow UK average"))

#using statistics to analyse data 
Life_expectancy_2_group <- Life_expectancy_2 %>%
  mutate(UKdiff= averangelifeexpectancy - 81.16) %>%
  group_by(UKcompare) %>%
  summarise(range=max(UKdiff) - min(UKdiff), count=n(), Average=mean(UKdiff))

Life_expectancy_3 <- Life_expectancy %>%
  mutate(UKdiff = averangelifeexpectancy - 81.16) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  mutate(across(UKdiff, round, 0)) %>%
  mutate(UKcompare = case_when(averangelifeexpectancy >= 81 ~
                                 str_c("equal or above UK average by",
                                       UKdiff,
                                       "years",
                                       sep = " "),
                               TRUE ~ str_c("below UK average by",
                                            UKdiff,
                                            "years",
                                            sep = " "))) %>%
  group_by(UKcompare) %>%
  summarize(count=n())

Life_expectancy_4 <- Life_expectancy %>%
  mutate(UKdiff = averangelifeexpectancy - 81.16) %>%
  mutate(across(is.numeric, round, 3)) %>%
  mutate(across(UKdiff, round, 0))

#plotting 
plot(LondonBoroughs$MaleLifeExpectancy2009_13, 
     LondonBoroughs$PercentChildrenInReceptionYearWhoAreObese2011_12To2013_14)

#ploting with plotly
plot_ly(LondonBoroughs,
        x = ~ MaleLifeExpectancy2009_13,
        y = ~ PercentChildrenInReceptionYearWhoAreObese2011_12To2013_14,
        text = ~ Borough,
        type = "scatter",
        mode = "markers")
