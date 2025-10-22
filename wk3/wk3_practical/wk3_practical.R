#loading packages 
library(fs)
library(ggplot2)
library(here)
library(plotly)
library(raster)
library(sf)
library(terra)
library(tidyverse)

#reading geopackage (whole AUS)
st_layers(here("wk3_practical_data/", "gadm41_AUS.gpkg"))

#reading layer 0 from geopackge -> country boundary 
aus_outline <- st_read(here("wk3_practical_data/", "gadm41_AUS.gpkg"),
                       layer = 'ADM_ADM_0')

#checking the CRS -> WGS 84
print(aus_outline)

#alternative way to check CRS
st_crs(aus_outline)$proj4string

#changing CRS from geographical (WGS 84) to local projected (GDA94)
aus_outline_prj <- aus_outline %>% 
  st_transform(., 3112)

print(aus_outline_prj)

#load raster layer jan
jan <- terra::rast(here("wk3_practical_data/wc2/", "wc2.1_5m_tavg_01.tif"))

jan

plot(jan)

#re-project raster using project() from terra package - we must find the PROJ4 on the EPSG website
pr1 <- terra::project(jan, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
plot(pr1)

#return raster to WGS 84 - we must find the PROJ4 on the EPSG website
pr1 <- pr1 %>% 
  terra::project(., "+proj=longlat +datum=WGS84 +no_defs +type=crs")

plot(pr1)

#look in our folder, find the files that end with .tif
dir_info("wk3_practical_data/wc2/")

#use this data with dplyr to select the data we actually want
files_list <- dir_info("wk3_practical_data/wc2/") %>% 
  filter(str_detect(path, ".tif")) %>% 
  dplyr::select(path) %>% 
  pull()

files_list

#load all of the data straight into a SpatRaster. 
#SpatRaster is a collection of raster layers with the same spatial extent and resolution
world_clim_temp <- files_list %>% 
  terra::rast()

world_clim_temp

#access the january layer (nlyr = 1)
world_clim_temp[[1]]

#rename layers within stack
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(world_clim_temp) <- month

#get data for just January using our new layer name
world_clim_temp$Jan

#extract data from raster
#make a dataframe of some sample sites — Australian cities/towns
site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
          "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
          "Wollongong", "Logan City")

lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)

lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)

samples <- data.frame(site, lon, lat, row.names = "site")

aus_city_temp <- terra::extract(world_clim_temp, samples)

aus_city_temp_2 <- aus_city_temp %>% 
  as_tibble() %>% 
  add_column(Site = site, .before = "Jan")

#descriptive statistics 
#sub-set for Perth city and histogram of it's temperatures 
perth_temp <- aus_city_temp_2 %>% 
  filter(Site == 'Perth')

usebreak <- c(8,10,12,14,16,18,20,22,24,26)

t <- perth_temp %>% 
  dplyr::select(Jan:Dec)

hist(as.numeric(t), 
     breaks = usebreak,
     col = "pink",
     main = "Histogram of Perth's Temperature",
     xlab = "Temperature",
     ylab = "Frequency")

hist_info <- as.numeric(t) %>% 
  as.numeric() %>% 
  hist(.)

hist_info 

#distribution of temperatures for the whole of Australia in Jan X Perth
plot(aus_outline$geom)

#simplify aus_outline
aus_outline_simple <- aus_outline %>% 
  st_simplify(., dTolerance = 1000) %>% 
  st_geometry() %>% 
  plot()

#set our map extent = aus_outline then crop our WorldClim dataset to it.

#1st check if geom and data are in the same CRS
print(aus_outline)
crs(world_clim_temp)

#crop creates an extent box that cover the shp
aus_temp <- aus_outline %>% 
  terra::crop(world_clim_temp, .)

plot(aus_temp)

#mask create a perfect cut of the raster using the shp (quicker to use after the crop)
aus_temp_exact <- terra::mask(aus_temp, aus_outline)

plot(aus_temp_exact)

hist(aus_temp_exact[[3]], col="red", main = "March Temperature")

#histogram with ggplot
aus_temp_exact_df <- aus_temp_exact %>% 
  as.data.frame()

gghist <- ggplot(aus_temp_exact_df,
                 aes(x=Mar)) +
  geom_histogram(color="black",
                 fill="white") +
  labs(title = "Ggplot2 histogram of Australia March temperature",
       x = "Temperature",
       y = "Frequency")

gghist + geom_vline(aes(xintercept = mean(Mar,
                                          na.rm = TRUE)),
                    color = "blue",
                    linetype = "dashed",
                    size = 1) +
  theme(plot.title = element_text(hjust = 0.5))

#plotting multiple months of temperature data on the same histogram
squish_data <- aus_temp_exact_df %>% 
  pivot_longer(
    cols = 1:12,
    names_to = "Month",
    values_to = "Temp"
  )

#subset the data, selecting two months
two_months <- squish_data %>% 
  filter(., Month=="Jan" | Month=="Jun")

mean_two_months <- two_months %>% 
  group_by(Month) %>% 
  summarise(mean=mean(Temp, na.rm = TRUE))

mean_two_months

#plot with ggplot2
ggplot(two_months, aes(x = Temp, color = Month, fill = Month)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  geom_vline(data = mean_two_months,
             aes(xintercept = mean,
                 color = Month),
             linetype = "dashed") +
  labs(title = "Ggplot2 histogram of Australia Jan and Jun temperatures",
       x = "Temperatures",
       y = "Frequency") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

data_complete_cases <- squish_data %>% 
  drop_na() %>% 
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar",
                                          "Apr","May","Jun",
                                          "Jul","Aug","Sep",
                                          "Oct","Nov","Dec")))

ggplot(data_complete_cases, aes(x=Temp, na.rm = TRUE)) +
  geom_histogram(color = "black", binwidth = 5) +
  labs(title = "Ggplot 2 faceted histogram of Australian temperatures",
       x = "Temperatures",
       y = "Frequency") +
  facet_grid(Month ~ .) +
  theme(plot.title = element_text(hjust = 0.5))

#plot with plotly
jan <- squish_data %>% 
  drop_na() %>% 
  filter(., Month == "Jan")

jun <- squish_data %>% 
  drop_na() %>% 
  filter(., Month == "Jun")

x <- list (title = "Temperature")
y <- list (title = "Frequency")

xbinsno<-list(start=0, end=40, size = 2.5)

ihist<-plot_ly(alpha = 0.6) %>%
  add_histogram(x = jan$Temp,
                xbins=xbinsno, name="January") %>%
  add_histogram(x = jun$Temp,
                xbins=xbinsno, name="June") %>% 
  layout(barmode = "overlay", xaxis=x, yaxis=y)

ihist

#other descriptive statistics 

#mean per month
meanofall <- squish_data %>% 
  group_by(Month) %>%
  summarise(mean = mean(Temp, na.rm=TRUE))

head(meanofall, n=1)  

#standard deviation per month
sdofall <- squish_data %>%
  group_by(Month) %>%
  summarize(sd = sd(Temp, na.rm=TRUE))

#maximum per month
maxofall <- squish_data %>%
  group_by(Month) %>%
  summarize(max = max(Temp, na.rm=TRUE))

#minimum per month
minofall <- squish_data %>%
  group_by(Month) %>%
  summarize(min = min(Temp, na.rm=TRUE))

#Interquartlie range per month
IQRofall <- squish_data %>%
  group_by(Month) %>%
  summarize(IQR = IQR(Temp, na.rm=TRUE))

#perhaps you want to store multiple outputs in one list..
lotsofstats <- squish_data %>%
  group_by(Month) %>%
  summarize(IQR = IQR(Temp, na.rm=TRUE), 
            max=max(Temp, na.rm=T))

#or you want to know the mean (or some other stat) 
#for the whole year as opposed to each month...

meanwholeyear=squish_data %>%
  summarize(meanyear = mean(Temp, na.rm=TRUE))
