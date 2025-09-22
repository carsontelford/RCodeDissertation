#### Prep GEE-classified land cover ####

library(tidyverse)
library(terra)
library(sf)
getwd()
setwd("C:/Users/carso/OneDrive - University of North Carolina at Chapel Hill/Dissertation/Aim 1")
studyarea <- st_read("Shapefiles/SW_UG_StudyArea.shp")
plot(studyarea$geometry)


#' need to load in the 8 images for each years, merge them, then stack 
#' the annual images
#' then i am going to smooth out misclassification, considering change to
#' cropland only if it:
#'    Occurs after multiple years of non-crop
#'    Persists for 2+ consecutive years

#### 1. Load images, merge, and save merged versions ####
getwd()
setwd("C:/Users/carso/OneDrive - University of North Carolina at Chapel Hill/Dissertation/Aim 1/")


# function to process rasters by merging and reclassing to 5 classes
mergeclassFUNC <- function(r1, r2, r3, r4, r5, r6, r7, r8) {
  rmerge <- merge(r1,r2,r3,r4,r5,r6,r7,r8)
  
  # collapse land cover classes
  rcl <- matrix(c(
    0,0,1, # crops
    1,1,1, # crops
    2,2,1,# crops
    3,3,2, # grass/bush
    4,4,2, # grass/bush
    5,5,3, # trees/forest
    6,6,3, # trees/forest
    7,7,4, # built
    8,8,4, # built
    9,9,5 # Water
  ), ncol = 3, byrow = TRUE)
  
  # Apply with right = NA to do exact matching
  rmergeclass <- classify(rmerge, rcl, right = NA)
  
  return(rmergeclass)
}


# 2024 rasters
i2024_1 <- rast("Classified Images/GEE_images/classified_image1_2024.tif")
i2024_2 <- rast("Classified Images/GEE_images/classified_image2_2024.tif")
i2024_3 <- rast("Classified Images/GEE_images/classified_image3_2024.tif")
i2024_4 <- rast("Classified Images/GEE_images/classified_image4_2024.tif")
i2024_5 <- rast("Classified Images/GEE_images/classified_image5_2024.tif")
i2024_6 <- rast("Classified Images/GEE_images/classified_image6_2024.tif")
i2024_7 <- rast("Classified Images/GEE_images/classified_image7_2024.tif")
i2024_8 <- rast("Classified Images/GEE_images/classified_image8_2024.tif")


# 2023
i2023_1 <- rast("Classified Images/GEE_images/classified_image1_2023.tif")
i2023_2 <- rast("Classified Images/GEE_images/classified_image2_2023.tif")
i2023_3 <- rast("Classified Images/GEE_images/classified_image3_2023.tif")
i2023_4 <- rast("Classified Images/GEE_images/classified_image4_2023.tif")
i2023_5 <- rast("Classified Images/GEE_images/classified_image5_2023.tif")
i2023_6 <- rast("Classified Images/GEE_images/classified_image6_2023.tif")
i2023_7 <- rast("Classified Images/GEE_images/classified_image7_2023.tif")
i2023_8 <- rast("Classified Images/GEE_images/classified_image8_2023.tif")

# 2022
i2022_1 <- rast("Classified Images/GEE_images/classified_image1_2022.tif")
i2022_2 <- rast("Classified Images/GEE_images/classified_image2_2022.tif")
i2022_3 <- rast("Classified Images/GEE_images/classified_image3_2022.tif")
i2022_4 <- rast("Classified Images/GEE_images/classified_image4_2022.tif")
i2022_5 <- rast("Classified Images/GEE_images/classified_image5_2022.tif")
i2022_6 <- rast("Classified Images/GEE_images/classified_image6_2022.tif")
i2022_7 <- rast("Classified Images/GEE_images/classified_image7_2022.tif")
i2022_8 <- rast("Classified Images/GEE_images/classified_image8_2022.tif")

# 2021
i2021_1 <- rast("Classified Images/GEE_images/classified_image1_2021.tif")
i2021_2 <- rast("Classified Images/GEE_images/classified_image2_2021.tif")
i2021_3 <- rast("Classified Images/GEE_images/classified_image3_2021.tif")
i2021_4 <- rast("Classified Images/GEE_images/classified_image4_2021.tif")
i2021_5 <- rast("Classified Images/GEE_images/classified_image5_2021.tif")
i2021_6 <- rast("Classified Images/GEE_images/classified_image6_2021.tif")
i2021_7 <- rast("Classified Images/GEE_images/classified_image7_2021.tif")
i2021_8 <- rast("Classified Images/GEE_images/classified_image8_2021.tif")

# 2020
i2020_1 <- rast("Classified Images/GEE_images/classified_image1_2020.tif")
i2020_2 <- rast("Classified Images/GEE_images/classified_image2_2020.tif")
i2020_3 <- rast("Classified Images/GEE_images/classified_image3_2020.tif")
i2020_4 <- rast("Classified Images/GEE_images/classified_image4_2020.tif")
i2020_5 <- rast("Classified Images/GEE_images/classified_image5_2020.tif")
i2020_6 <- rast("Classified Images/GEE_images/classified_image6_2020.tif")
i2020_7 <- rast("Classified Images/GEE_images/classified_image7_2020.tif")
i2020_8 <- rast("Classified Images/GEE_images/classified_image8_2020.tif")

# 2019
i2019_1 <- rast("Classified Images/GEE_images/classified_image1_2019.tif")
i2019_2 <- rast("Classified Images/GEE_images/classified_image2_2019.tif")
i2019_3 <- rast("Classified Images/GEE_images/classified_image3_2019.tif")
i2019_4 <- rast("Classified Images/GEE_images/classified_image4_2019.tif")
i2019_5 <- rast("Classified Images/GEE_images/classified_image5_2019.tif")
i2019_6 <- rast("Classified Images/GEE_images/classified_image6_2019.tif")
i2019_7 <- rast("Classified Images/GEE_images/classified_image7_2019.tif")
i2019_8 <- rast("Classified Images/GEE_images/classified_image8_2019.tif")

# 2018
i2018_1 <- rast("Classified Images/GEE_images/classified_image1_2018.tif")
i2018_2 <- rast("Classified Images/GEE_images/classified_image2_2018.tif")
i2018_3 <- rast("Classified Images/GEE_images/classified_image3_2018.tif")
i2018_4 <- rast("Classified Images/GEE_images/classified_image4_2018.tif")
i2018_5 <- rast("Classified Images/GEE_images/classified_image5_2018.tif")
i2018_6 <- rast("Classified Images/GEE_images/classified_image6_2018.tif")
i2018_7 <- rast("Classified Images/GEE_images/classified_image7_2018.tif")
i2018_8 <- rast("Classified Images/GEE_images/classified_image8_2018.tif")

# 2017
i2017_1 <- rast("Classified Images/GEE_images/classified_image1_2017.tif")
i2017_2 <- rast("Classified Images/GEE_images/classified_image2_2017.tif")
i2017_3 <- rast("Classified Images/GEE_images/classified_image3_2017.tif")
i2017_4 <- rast("Classified Images/GEE_images/classified_image4_2017.tif")
i2017_5 <- rast("Classified Images/GEE_images/classified_image5_2017.tif")
i2017_6 <- rast("Classified Images/GEE_images/classified_image6_2017.tif")
i2017_7 <- rast("Classified Images/GEE_images/classified_image7_2017.tif")
i2017_8 <- rast("Classified Images/GEE_images/classified_image8_2017.tif")

# 2016
i2016_1 <- rast("Classified Images/GEE_images/classified_image1_2016.tif")
i2016_2 <- rast("Classified Images/GEE_images/classified_image2_2016.tif")
i2016_3 <- rast("Classified Images/GEE_images/classified_image3_2016.tif")
i2016_4 <- rast("Classified Images/GEE_images/classified_image4_2016.tif")
i2016_5 <- rast("Classified Images/GEE_images/classified_image5_2016.tif")
i2016_6 <- rast("Classified Images/GEE_images/classified_image6_2016.tif")
i2016_7 <- rast("Classified Images/GEE_images/classified_image7_2016.tif")
i2016_8 <- rast("Classified Images/GEE_images/classified_image8_2016.tif")


# merge and classify to collapsed groups
merge24 <- mergeclassFUNC(i2024_1, i2024_2, i2024_3, i2024_4,
                          i2024_5, i2024_6, i2024_7, i2024_8)

merge23 <- mergeclassFUNC(i2023_1, i2023_2, i2023_3, i2023_4,
                          i2023_5, i2023_6, i2023_7, i2023_8)

merge22 <- mergeclassFUNC(i2022_1, i2022_2, i2022_3, i2022_4,
                          i2022_5, i2022_6, i2022_7, i2022_8)

merge21 <- mergeclassFUNC(i2021_1, i2021_2, i2021_3, i2021_4,
                          i2021_5, i2021_6, i2021_7, i2021_8)

merge20 <- mergeclassFUNC(i2020_1, i2020_2, i2020_3, i2020_4,
                          i2020_5, i2020_6, i2020_7, i2020_8)

merge19 <- mergeclassFUNC(i2019_1, i2019_2, i2019_3, i2019_4,
                          i2019_5, i2019_6, i2019_7, i2019_8)

merge18 <- mergeclassFUNC(i2018_1, i2018_2, i2018_3, i2018_4,
                          i2018_5, i2018_6, i2018_7, i2018_8)

merge17 <- mergeclassFUNC(i2017_1, i2017_2, i2017_3, i2017_4,
                          i2017_5, i2017_6, i2017_7, i2017_8)

merge16 <- mergeclassFUNC(i2016_1, i2016_2, i2016_3, i2016_4,
                          i2016_5, i2016_6, i2016_7, i2016_8)

lcstack <- c(merge16, merge17, merge18, merge19, merge20,
             merge21, merge22, merge23, merge24)
lcstack <- mask(lcstack, studyarea)
plot(lcstack[[1]], colNA="black")
names(lcstack)
names(lcstack) <- c("lc16","lc17","lc18","lc19","lc20",
                    "lc21","lc22","lc23","lc24")
names(lcstack)

# save the classification stack classes 1-5
writeRaster(
  lcstack,
  "Classified Images/Merged_annual_images/lcstack_1to5.tif",
  datatype = "INT1U", # 1-byte unsigned integer (fits 1–5)
  overwrite = TRUE)


# reclass to binary 0 or 1 cropland, with water as NA
rcl <- matrix(c(
  1,1,1, # crops
  2,2,0, # grassbush
  3,3,0, # trees/forest
  4,4,0, # urban
  5,5,NA # water
), ncol = 3, byrow = TRUE)

# Apply with right = NA to do exact matching
lcstackbin <- classify(lcstack, rcl, right = NA)
plot(lcstackbin[[1]], colNA="black")

# export
writeRaster(
  lcstackbin,
  "Classified Images/Merged_annual_images/lcstack_binary.tif",
  datatype = "INT1U", # 1-byte unsigned integer (fits 1–5)
  overwrite = TRUE)


#### Check stability in class ######
# check % of cells that are unstable across years
lcstackbin <- rast("Classified Images/Merged_annual_images/lcstack_binary.tif")
library(sp)
sample_pts <- spatSample(lcstackbin, size = 1000000, method = "random",xy=T)
sample_pts <- sample_pts %>%
  filter(!is.na(lc16)) %>%
  filter(!is.na(lc17)) %>%
  filter(!is.na(lc18)) %>%
  filter(!is.na(lc19)) %>%
  filter(!is.na(lc20)) %>%
  filter(!is.na(lc21)) %>%
  filter(!is.na(lc22)) %>%
  filter(!is.na(lc23)) %>%
  filter(!is.na(lc24)) 

# Helper to compute longest run of same value
library(dplyr)
lc_cols <- paste0("lc", 16:24)

# Number of switches per row (0→1 or 1→0)
num_switches <- function(x) {
  # handle possible NAs: drop or treat them as non-switch depending on intent
  dx <- diff(x)
  sum(abs(dx), na.rm = TRUE)
}

# longest run func
longest_run <- function(x) {
  r <- rle(x)
  max(r$lengths, na.rm = TRUE)
}

sample_pts$longest_run <- apply(sample_pts[lc_cols], 1, longest_run)
sample_pts$num_switches <- apply(sample_pts[lc_cols], 1, num_switches)
sample_pts <- sample_pts %>%
  mutate(stablele2 = ifelse(num_switches>2, 0,1))
table(sample_pts$stablele2)



# aggregate to 5km res for % crops
getwd()
lcstackbin <- rast("Classified Images/Merged_annual_images/lcstack_binary.tif")
names(lcstackbin)
plot(lcstackbin)
crop1624 <- aggregate(lcstackbin, fact=500, fun='mean', na.rm=T)
plot(crop1624[[1]], colNA="black")
summary(crop1624[[1]])



#### 3. See time series of pixels ####
# Sample 1000 random cells
library(sp)
sample_pts <- spatSample(lcstack, size = 1000, method = "random",xy=T)
sample_pts <- sample_pts %>%
  filter(!is.na(lc16))
coordinates(sample_pts) <- ~x+y
sample_pts <- st_as_sf(sample_pts)

# Extract land cover values at those points
lc_vals <- extract(lcstack, sample_pts, xy=T)
lc_vals <- lc_vals %>%
  dplyr::select(ID,y,x,everything(.))

ncell(lcstack$lc24)
res(lcstack)

# # check common land changes
# early_mode <- modal(lcstack[[1:4]], na.rm = TRUE)
# recent_mode <- modal(lcstack[[6:9]], na.rm = TRUE)
# 


# Plot sampled points
plot(lcstack$lc16)
points(sample_pts, col = "red", pch = 20)

# extract classification at outbreak sites
rvf <- read.csv("C:/Users/carso/OneDrive - University of North Carolina at Chapel Hill/Dissertation/RVFdata/Updated RVF Linelist 28_June_2024_CTFINAL.csv")
rvf <- rvf %>%
  mutate(y=Latitude,
         x=Longitude) %>%
  filter(!is.na(y)) %>%
  distinct(Year,y,x, .keep_all = T)


library(sp)
coordinates(rvf) <- ~x+y
rvf <- st_as_sf(rvf)

vals <- extract(lcstack, rvf, xy=T)
vals <- vals %>%
  dplyr::select(ID,y,x,everything()) %>%
  filter(!is.na(lc24))
  

#' binary crops
vals2 <- vals %>%
  mutate(lc16 = case_when(lc16 == 1 ~ 1,
                         lc16 %in% c(2,3,4,5) ~ 0),
         lc17 = case_when(lc17 == 1 ~ 1,
                         lc17 %in% c(2,3,4,5) ~ 0),
         lc18 = case_when(lc18 == 1 ~ 1,
                         lc18 %in% c(2,3,4,5) ~ 0),
         lc19 = case_when(lc19 == 1 ~ 1,
                         lc19 %in% c(2,3,4,5) ~ 0),
         lc20 = case_when(lc20 == 1 ~ 1,
                         lc20 %in% c(2,3,4,5) ~ 0),
         lc21 = case_when(lc21 == 1 ~ 1,
                         lc21 %in% c(2,3,4,5) ~ 0),
         lc22 = case_when(lc22 == 1 ~ 1,
                         lc22 %in% c(2,3,4,5) ~ 0),
         lc23 = case_when(lc23 == 1 ~ 1,
                         lc23 %in% c(2,3,4,5) ~ 0),
         lc24 = case_when(lc24 == 1 ~ 1,
                         lc24 %in% c(2,3,4,5) ~ 0))

#' write a rule that if the first 2 years and last 2 years all the same (crop vs noncrop), 
#' then the values in between years should also be that value


