

# spatial join to get district case counts
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(terra)
library(tmap)
library(lubridate)

# load coords for cases
xy <- read.csv("RVFdata/UpdatedRVFLinelist28_June_2024_CTFINAL.csv")
xy_sf <- st_as_sf(xy, coords = c("x", "y"), crs = 4326) # WGS 84 CRS

# load district shpaefile
shp <- st_read("uganda_shapefiles/uga_admbnda_adm2_ubos_20200824.shp") %>%
  dplyr::select(ADM2_EN)

xy_with_id <- st_join(xy_sf, shp, left = TRUE) # spatial join
xy_with_id_df <- xy_with_id %>% 
  st_drop_geometry() %>%
  group_by(ADM2_EN) %>%
  summarise(Cases=n())

# merge case count to shp
shp2 <- shp %>%
  left_join(xy_with_id_df)

#### map ####
tm_shape(shp2) +
  tm_polygons("Cases",  # Replace with your case count column
              palette = "YlOrRd", 
              textNA = "No Cases",
              breaks = c(0,2, 5,15, 25,33),  # Specify custom bin ranges
              labels = c("1", "2-5", "6-15", "15-25","34"),  # Custom labels
              title = "RVF Cases 2016-2025") +
  tm_borders() +
  tm_layout(legend.position = c("left", "top")) +
  tm_compass(type = "4star", position = c("right", "top"),size=1.5) +  # North Arrow
  tm_scale_bar(position = c("right", "bottom"))  # Scale Bar


#### Time series ####
# get rain data
rain <- read.csv("C:/Users/carso/OneDrive - University of North Carolina at Chapel Hill/CDC Work Docs/Uganda/Longitudinal Study/GEE_exports/monthlyresultsCHIRPS_Livestock2017.csv")
rain <- rain %>%
  mutate(Date = paste0(month,"-01")) %>%
  mutate(Date = as.Date(Date),
         Month = month(Date)) %>%
  group_by(Month) %>%
  summarise(rain = mean(mean)) %>%
  mutate(Month = factor(Month, levels = 1:12, labels = month.name))


# time series plots of incidence
tsdf <- xy_with_id %>%
  mutate(Date = as.Date(arrivaldate, format = "%m/%d/%Y"),
         Month = month(Date),
         Year = year(Date))
tsdf$Month <- factor(tsdf$Month, levels = 1:12, labels = month.name)

library(ggplot2)
library(dplyr)
library(tidyr)

# Ensure Month is a factor with all 12 months
tsdf$Month <- factor(tsdf$Month, levels = month.name)  



# Summarise and fill missing months with 0 cases
monthplot <- tsdf %>%
  group_by(Month, Year) %>%
  summarise(Cases = n(), .groups = "drop") %>%
  complete(Month, Year, fill = list(Cases = 0)) %>%  # Ensure all months appear with 0 cases
  left_join(rain) %>%
  ggplot() +
  geom_line(aes(x = Month, y = Cases, group= factor(Year),col=factor(Year)), size=2) +
  labs(title = "Monthly Case Count", x = "Month", y = "Number of Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for clarity

monthplot


monthplot <- tsdf %>%
  group_by(Month, Year) %>%
  summarise(Cases = n(), .groups = "drop") %>%
  complete(Month, Year, fill = list(Cases = 0)) %>%  
  ggplot() +  # Keep Year as numeric
  geom_line(aes(x = Month, y = Cases, group = Year, col = Year), size = 2) +
  scale_color_viridis_c(direction=-1) +  # Smooth gradient for Year
  labs(title = "Monthly Case Count", x = "Month", y = "Number of Cases", color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for clarity

monthplot

monthplot <- tsdf %>%
  group_by(Month, Year) %>%
  summarise(Cases = n(), .groups = "drop") %>%
  complete(Month, Year, fill = list(Cases = 0)) %>%  
  left_join(rain) %>%
  ggplot() +  
  geom_col(aes(x = factor(Month), y = Cases, fill = Year, group = Year),
           position = position_dodge(width = 0.9)) +  # Side-by-side bars
  scale_fill_viridis_c(direction=-1) +  # Smooth gradient color scale
  labs(title = "Monthly Case Count", x = "Month", y = "Number of Cases", fill = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

monthplot


monthplot <- tsdf %>%
  group_by(Month, Year) %>%
  summarise(Cases = n(), .groups = "drop") %>%
  complete(Month, Year, fill = list(Cases = 0)) %>%  
  left_join(rain) %>%   # Ensure the join works correctly with Month and Year
  mutate(rain=rain/10) %>%
  ggplot() +  
  geom_col(aes(x = factor(Month), y = Cases, fill = Year, group = Year),
           position = position_dodge(width = 0.9)) +  # Side-by-side bars
  scale_fill_viridis_c(direction=-1) +  # Smooth gradient color scale
  geom_line(aes(x = factor(Month), y = rain, group = Year, color = "Mean\nRain(cm)/10"), size = 1) +  # Add the line for rainfall
  scale_color_manual(values = c("Mean\nRain(cm)/10" = "blue")) +  # Line color for rainfall
  labs(title = "Monthly Case Count with Rainfall", x = "Month", y = "Number of Cases", fill = "Year", color = "Mean\nRain(cm)/10") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        legend.position = c(0.85, 0.85),
        legend.text = element_text(size=13),
        legend.title = element_text(size=14))

monthplot






monthplot <- tsdf %>%
  group_by(Month, Year) %>%
  summarise(Cases = n(), .groups = "drop") %>%
  complete(Month, Year, fill = list(Cases = 0)) %>%  
  left_join(rain) %>%   # Ensure the join works correctly with Month and Year
  mutate(rain = rain / 10) %>%
  ggplot() +  
  geom_col(aes(x = factor(Month), y = Cases, fill = Year, group = Year),
           position = position_dodge(width = 0.9)) +  # Side-by-side bars
  scale_fill_viridis_c(direction = -1) +  # Smooth gradient color scale
  geom_line(aes(x = factor(Month), y = rain, group = Year, color = "Mean\nRain(cm)/10"), size = 1) +  # Add the line for rainfall
  scale_color_manual(values = c("Mean\nRain(cm)/10" = "blue")) +  # Line color for rainfall
  geom_text(aes(x = factor(Month), y = Cases, label = Year, group = Year), 
            position = position_dodge(width = 0.9), hjust = -0.2, size = 4, angle=90) +  # Labels for the bars
  labs(title = "Monthly Case Count with Rainfall", x = "Month", y = "Number of Cases", fill = "Year", color = "Mean\nRain(cm)/10") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = c(0.9, 0.85),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14))

monthplot



