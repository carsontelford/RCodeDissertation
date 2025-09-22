#### Part 2 prep covars ####

library(tidyverse)
library(terra)
library(sf)
library(exactextractr)
library(raster)
library(tidyr)

setwd("C:/Users/carso/OneDrive - University of North Carolina at Chapel Hill/Dissertation/Aim 1")
studyarea <- st_read("Shapefiles/SW_UG_StudyArea.shp")
st_crs(studyarea)

# create grid
grid <- st_make_grid(studyarea, cellsize = c(.0449, .0449), square = T)
plot(studyarea$geometry)
plot(grid, add=T)
grid_clipped <- grid
table(st_geometry_type(grid_clipped))
grid_clipped_sf <- st_as_sf(grid_clipped)
grid_clipped_sf$ID <- 1:nrow(grid_clipped_sf)
plot(grid_clipped_sf)
st_write(grid_clipped_sf, "Shapefiles/Gridunits_5km/grid5km_notclipped.shp")

grid_vect <- vect(grid_clipped_sf)

# Create template raster with desired resolution and extent
template_rast <- rast(ext(grid_vect), resolution = 0.0449, crs = crs(grid_vect))
grid_rast <- rasterize(grid_vect, template_rast, field = "ID")
plot(grid_rast)

grid_rast_df <- as.data.frame(grid_rast, xy=T)
testdf <- rasterFromXYZ(grid_rast_df)
plot(testdf)

#### Extract covars ####
##### Cropland #####
# mean
lcstackbin <- rast("Classified Images/Merged_annual_images/lcstack_binary.tif")
names(lcstackbin)
plot(lcstackbin[[1]])
# crop1624 <- aggregate(lcstackbin, fact=2, fun='mean', na.rm=T)

grid_clipped_sf <- st_as_sf(grid_vect)
gridcentroids <- st_centroid(grid_clipped_sf)
gridID <- as.data.frame(st_coordinates(gridcentroids))
gridID$ID <- 1:nrow(gridID)
croppct <- exact_extract(lcstackbin, grid_clipped_sf, 'mean',
                           force_df=T,
                           append_cols=T)  # NA values are skipped automatically

croppctID <- cbind(gridID, croppct)

write.csv(croppctID, "Covariate data/ExtractedDataFrames/croppct.csv")
croppctID <- read.csv("Covariate data/ExtractedDataFrames/croppct.csv") %>%
  dplyr::select(-c(X.1))
str(croppctID)

croppct_long <- croppctID %>%
  pivot_longer(
    cols = starts_with("mean.lc"),
    names_to = "year",
    values_to = "crop_percent"
  ) %>%
  mutate(year = as.integer(gsub("mean.lc", "", year)))

covardf <- croppct_long %>%
  mutate(
    crop_3yr_avg = rowMeans(cbind(lag(crop_percent), crop_percent, lead(crop_percent)), na.rm = TRUE)
  )


#### Crop cell count in each grid ####
#' since water is NA, i need to see how many total pixels of land there are
#' in each grid unit, so I dont include a grid unit with very few cells,
#' all of which happen to be crops
lcstackbin <- rast("Classified Images/Merged_annual_images/lcstack_binary.tif")
names(lcstackbin)
plot(lcstackbin[[1]])
landcover <- lcstackbin*0+1
plot(landcover$lc16, colNA="black")

grid_clipped_sf <- st_as_sf(grid_vect)
gridcentroids <- st_centroid(grid_clipped_sf)
gridID <- as.data.frame(st_coordinates(gridcentroids))
gridID$ID <- 1:nrow(gridID)
lcsum <- exact_extract(landcover, grid_clipped_sf, 'sum',
                           force_df=T,
                           append_cols=T)  # NA values are skipped automatically
write.csv(lcsum, "Covariate data/ExtractedDataFrames/landcoverpixels_sum.tif")

lcsum <- read.csv("Covariate data/ExtractedDataFrames/landcoverpixels_sum.tif") %>%
  dplyr::select(-c(X))

lcsum_long <- lcsum %>%
  pivot_longer(
    cols = starts_with("sum.lc"),
    names_to = "year",
    values_to = "land_pixels"
  ) %>%
  mutate(year = as.integer(gsub("sum.lc", "", year)))

covardf2 <- covardf %>%
  left_join(lcsum_long)


#### Cattle count ####
lcstackbin <- rast("Classified Images/Merged_annual_images/lcstack_binary.tif")
newres <- aggregate(lcstackbin$lc16, factor=5)
cattle <- rast("Covariate data/LivestockFAO/Cattle2020.tif")
plot(cattle)
cattlecrop <- crop(cattle, studyarea)
plot(cattlecrop, colNA="black")
cattleresamp <- resample(cattlecrop, newres, method = "bilinear")
plot(cattleresamp, colNA="black")
lines(studyarea)

cattledens <- exact_extract(cattleresamp, grid_clipped_sf, 'mean',
                           force_df=T,
                           append_cols=T)  # NA values are skipped automatically
write.csv(cattledens, "Covariate data/ExtractedDataFrames/cattledens.csv")

cattledens <- read.csv("Covariate data/ExtractedDataFrames/cattledens.csv")
cattledens <- cattledens %>%
  rename(cattledens = mean) %>%
  dplyr::select(-c(X))
covardf3 <- covardf2 %>%
  left_join(cattledens)


#### sheep dens ####
lcstackbin <- rast("Classified Images/Merged_annual_images/lcstack_binary.tif")
newres <- aggregate(lcstackbin$lc16, factor=5)
sheep <- rast("Covariate data/LivestockFAO/Sheep2020.tif")
sheepcrop <- crop(sheep, studyarea)
plot(sheepcrop, colNA="black")
sheepresamp <- resample(sheepcrop, newres, method = "bilinear")
plot(sheepresamp, colNA="black")
lines(studyarea)

sheepdens <- exact_extract(sheepresamp, grid_clipped_sf, 'mean',
                           force_df=T,
                           append_cols=T)  # NA values are skipped automatically
write.csv(sheepdens, "Covariate data/ExtractedDataFrames/sheepdens.csv")

sheepdens <- read.csv("Covariate data/ExtractedDataFrames/sheepdens.csv")
sheepdens <- sheepdens %>%
  rename(sheepdens = mean) %>%
  dplyr::select(-c(X))
covardf3 <- covardf3 %>%
  left_join(sheepdens)

#### Goat dens ####
goat <- rast("Covariate data/LivestockFAO/Goats2020.tif")
goatcrop <- crop(goat, studyarea)
plot(goatcrop, colNA="black")
goatresamp <- resample(goatcrop, newres, method = "bilinear")
plot(goatresamp, colNA="black")
lines(studyarea)

goatdens <- exact_extract(goatresamp, grid_clipped_sf, 'mean',
                           force_df=T,
                           append_cols=T)  # NA values are skipped automatically
write.csv(goatdens, "Covariate data/ExtractedDataFrames/goatdens.csv")

goatdens <- read.csv("Covariate data/ExtractedDataFrames/goatdens.csv")
goatdens <- goatdens %>%
  rename(goatdens = mean) %>%
  dplyr::select(-c(X))
covardf3 <- covardf3 %>%
  left_join(goatdens)

covardf3 <- covardf3 %>%
  mutate(livestockdens = cattledens+sheepdens+goatdens)

#### Elevation ####
library(dismo)
library(terra)

# Download SRTM tile containing Mbarara (lon = 30.7, lat = -0.6)
elev_rast1 <- getData('SRTM', lon = 30.7, lat = -0.6)
elev_rast_terra1 <- rast(elev_rast1)
plot(elev_rast_terra1, main = "SRTM Elevation")
lines(studyarea)

elev_rast2 <- getData('SRTM', lon = 30.7, lat = 0.2)
elev_rast_terra2 <- rast(elev_rast2)
plot(elev_rast_terra2, main = "SRTM Elevation")
lines(studyarea)

elev_rast3 <- getData('SRTM', lon = 29.7, lat = -0.6)
elev_rast_terra3 <- rast(elev_rast3)
plot(elev_rast_terra3, main = "SRTM Elevation")
lines(studyarea)

elev_rast4 <- getData('SRTM', lon = 29.7, lat = 0.6)
elev_rast_terra4 <- rast(elev_rast4)
plot(elev_rast_terra4, main = "SRTM Elevation")
lines(studyarea)

elevstitch <- merge(elev_rast_terra1, elev_rast_terra2,
                    elev_rast_terra3, elev_rast_terra4)
plot(elevstitch)
lines(studyarea)

elevstitchcrop <- crop(elevstitch, studyarea)
plot(elevstitchcrop)
lines(studyarea)
# writeRaster(elevstitchcrop,"Covariate data/Elevation/elevationraster30m.tif",
#             overwrite=T)

# elev <- rast("Covariate data/Elevation/elevationraster30m.tif")
elev <- elevstitchcrop
elevmean <- exact_extract(elev, grid_clipped_sf, 'mean',
                           force_df=T,
                           append_cols=T)  # NA values are skipped automatically
write.csv(elevmean, "Covariate data/ExtractedDataFrames/elevationmean.csv")

elevmean <- read.csv("Covariate data/ExtractedDataFrames/elevationmean.csv")
elevmean <- elevmean %>%
  rename(elevation = mean) %>%
  dplyr::select(-c(X))
covardf3 <- covardf3 %>%
  left_join(elevmean)



#### Slope ####
# elev <- rast("Covariate data/Elevation/elevationraster30m.tif")
slope <- terrain(elev, v = "slope", unit = "radians")
plot(slope)
slopemean <- exact_extract(slope, grid_clipped_sf, 'mean',
                           force_df=T,
                           append_cols=T)  # NA values are skipped automatically
slopemean <- slopemean %>%
  rename(slope = mean)
covardf3 <- covardf3 %>%
  left_join(slopemean)


#### TWI ####
# dem <- rast("Covariate data/Elevation/elevationraster30m.tif")
dem <- elev
lcstackbin <- rast("Classified Images/Merged_annual_images/lcstack_binary.tif")
watermask <- aggregate(lcstackbin$lc24, factor=4)
watermaskbin <- ifel(is.na(watermask), NA, 1)
watermaskbinres <- resample(watermaskbin, dem)

demmask <- mask(dem, watermaskbinres)
writeRaster(demmask, "Covariate data/Elevation/elevationraster30m_mask.tif")

library(whitebox)
wbt_fill_depressions("Covariate data/Elevation/elevationraster30m_mask.tif", "Covariate data/Elevation/dem_filled.tif")
wbt_d8_pointer("Covariate data/Elevation/dem_filled.tif", "Covariate data/Elevation/flow_dir.tif")
flowdir <- rast("Covariate data/Elevation/flow_dir.tif")
wbt_d8_flow_accumulation("Covariate data/Elevation/dem_filled.tif", "Covariate data/Elevation/flow_acc.tif", out_type = "cells")

library(terra)
dem <- rast("Covariate data/Elevation/dem_filled.tif")
dem2 <- rast("Covariate data/Elevation/elevationraster30m_mask.tif")
flowacc <- rast("Covariate data/Elevation/flow_acc.tif")
slope <- terrain(dem2, v = "slope", unit = "radians")
scale <- flowacc * res(dem2)[1] # specific catchment area- contirbuting area per unit width meters^2/m

# Avoid divide-by-zero by setting a floor for slope
safe_slope <- tan(slope)
safe_slope[safe_slope < 0.001] <- 0.001  # Adjust threshold as needed

# Avoid zero or tiny flow accumulation values
safe_acc <- flowacc
safe_acc[safe_acc < 1] <- 1  # This ensures a/log value ≥ 0

twi <- log(safe_acc / safe_slope)
plot(twi)
writeRaster(twi, "Covariate data/Elevation/twi_watermask.tif",
            overwrite=T)

twi <- rast("Covariate data/Elevation/twi_watermask.tif")
twimean <- exact_extract(twi, grid_clipped_sf, 'mean',
                           force_df=T,
                           append_cols=T)  # NA values are skipped automatically
twimean <- twimean %>%
  rename(twi = mean)
covardf3 <- covardf3 %>%
  left_join(twimean)


#### Landscan ####
ls <- read.csv("Covariate data/GEE Landscan/Annual_Population_Mean.csv")
ls <- ls %>%
  mutate(year=Year-2000,
         popmean = PopulationMean) %>%
  dplyr::select(ID, year, popmean)

covardf4 <- covardf3 %>%
  left_join(ls) %>%
  fill(popmean, .direction = "down") # fill 2024 with 2023 vals

#### CHIRPS monthly mean of daily vals ####
chirps <- read.csv("Covariate data/GEE exported covars/dailyresultsCHIRPS.csv")
chirps <- chirps %>%
  mutate(date = as.Date(date))
chirps2 <- chirps %>%
  arrange(ID) %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(year=year-2000) %>%
  group_by(ID,year) %>%
  summarise(chirpsmean = mean(mean),
            chirpsmean_sd = sd(mean),
            chirpsmean_cov = chirpsmean_sd/chirpsmean)

covardf4 <- covardf4 %>%
  left_join(chirps2)

#### CHIRPS monthly sum of daily vals ####
chirpssum <- read.csv("Covariate data/GEE exported covars/dailyresultsCHIRPSsum.csv")
chirpssum <- chirpssum %>%
  mutate(date = as.Date(date))
chirpssum2 <- chirpssum %>%
  arrange(ID) %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(year=year-2000)

chirpssum2 <- chirpssum2 %>%
  group_by(ID,year) %>%
  summarise(chirpssum = mean(sum),
            chirpssum_sd = sd(sum),
            chirpssum_cov = chirpssum_sd/chirpssum)

covardf4 <- covardf4 %>%
  left_join(chirpssum2)


#### Temp monthly means ####
temp <- read.csv("Covariate data/GEE exported covars/dailyresultsTEMP.csv")
temp <- temp %>%
  mutate(date = as.Date(date)) %>%
  mutate(mean = mean-273.15)
temp2 <- temp %>%
  arrange(ID) %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(year=year-2000) %>%
  group_by(ID,year) %>%
  summarise(tempmean = mean(mean),
            tempmean_sd = sd(mean),
            tempmean_cov = tempmean_sd/tempmean)

covardf5 <- covardf4 %>%
  left_join(temp2)


#### NDVI monthly means ####
ndvi <- read.csv("Covariate data/GEE exported covars/dailyresultsNDVI.csv")
ndvi <- ndvi %>%
  mutate(date = as.Date(date))
ndvi2 <- ndvi %>%
  arrange(ID) %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(year=year-2000) %>%
  group_by(ID,year) %>%
  summarise(ndvimean = mean(mean),
            ndvimean_sd = sd(mean),
            ndvimean_cov = ndvimean_sd/ndvimean)

covardf5 <- covardf5 %>%
  left_join(ndvi2)


#### Clean the dataset ####
#' remove grid units that are primarily water and miss observations
#' in the covariates

covardf6 <- covardf5 %>%
  mutate(n=1) %>%
  filter(!is.na(cattledens)) %>%
  mutate(pctland = land_pixels/249824.5) %>%
  filter(pctland > .05) %>%
  group_by(ID) %>%
  mutate(obscount = sum(n)) %>%
  filter(obscount == 9)  # only keep cells with full 9 years of land cover class >5%

# check to make sure i have full obs for each var
checkdf <- covardf6 %>%
  group_by(ID) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  ungroup() %>%
  dplyr::select(-c(ID))
checkdf$row_sum <- rowSums(checkdf, na.rm = TRUE)
summary(checkdf$row_sum) # no na vals, proceed


#### Neighbor smoothing ####
# merge to xyz grid
covardf7 <- covardf6 %>%
  left_join(grid_rast_df) %>%
  dplyr::select(-c(ID.1, X, Y))


library(dplyr)
library(terra)
library(purrr)

# Define Queen's case weight matrix once
w <- matrix(1, nrow = 3, ncol = 3)

# Create empty list to store smoothed data frames
smoothed_dfs <- list()

# Loop over years 2016 to 2024
for (yr in 16:24) {
  
  # Filter and prepare data
  df <- covardf7 %>%
    ungroup() %>%
    filter(year == yr) %>%
    dplyr::select(-c(n, obscount)) %>%
    dplyr::select(x, y, everything())
  
  # Convert to raster
  rast_yr <- rast(df, type = "xyz")
  
  # Apply focal smoothing
  rast_smooth <- focal(rast_yr, w = w, fun = mean, na.policy = "omit")
  plot(rast_smooth[[3]])
  
  # Convert to data frame and rename columns
  df_smooth <- as.data.frame(rast_smooth)
  names(df_smooth) <- paste0(names(df_smooth), "_smooth")
  
  # Restore ID and year columns (which were smoothed away)
  df_smooth <- df_smooth %>%
    rename(ID = ID_smooth,
           year = year_smooth)
  
  # Store in list
  smoothed_dfs[[as.character(yr)]] <- df_smooth
}

# Combine all years into a single data frame
raster_smooth_all <- bind_rows(smoothed_dfs)

# merge back to original dataset 
covardf8 <- covardf7 %>%
  left_join(raster_smooth_all)

testrastdf <- covardf8 %>%
  ungroup() %>%
  filter(year==24) %>%
  dplyr::select(x,y,crop_percent)
testrast <- rasterFromXYZ(testrastdf)
plot(testrast)


#### Bring in RVF data ####
rvf <- read.csv("C:/Users/carso/OneDrive - University of North Carolina at Chapel Hill/Dissertation/RVFdata/UpdatedRVFLinelist28_June_2024_CTFINAL.csv")
rvf <- rvf %>%
  dplyr::select(S.No, x,y,Latitude,Longitude, Year)

rvfsf <- st_as_sf(rvf, coords = c("x", "y"), crs = st_crs(grid_clipped_sf))
plot(testrast)
plot(rvfsf$geometry, add=T)

# plot grid and points
plot(st_geometry(rvfsf), col = 'red', pch = 16, cex = 0.8)
plot(st_geometry(grid_clipped_sf), col = NA, border = 'gray',add=T)

# spatial join to pull grid ID for each case rvfcase
rvf_withID <- st_join(rvfsf, grid_clipped_sf[, "ID"])
rvf_withID <- rvf_withID %>%
  dplyr::select(ID, Year, Longitude, Latitude) %>%
  mutate(rvf=1,
         Year=Year-2000) %>%
  group_by(ID,Year) %>%
  summarise(rvf = sum(rvf)) %>%
  st_drop_geometry() %>%
  rename(year=Year)

sum(rvf_withID$rvf)

# merge it into covar df 

fulldf <- covardf8 %>%
  left_join(rvf_withID) %>%
  mutate(rvfn = ifelse(is.na(rvf),0,rvf)) %>%
  mutate(rvfbin = ifelse(rvfn == 0, 0, 1)) %>%
  dplyr::select(-c(rvf))
table(fulldf$rvfn)
sum(fulldf$rvfn)
table(fulldf$rvfbin)


#### SAVE the final dataset ####

fulldf2 <- fulldf %>%
  dplyr::select(ID,year,rvfn,rvfbin, everything())

write.csv(fulldf2,
          "Covariate Data/Final Merged Dataset/mergeddf_21Jul25.csv")






