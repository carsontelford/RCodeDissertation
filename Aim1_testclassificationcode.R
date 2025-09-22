

#### Top ####
library(tidyverse)
library(terra)
library(sf)
library(ranger)
library(caret)
library(raster)

#### Load raster bands ####
band_files <- list.files("Aim 1/GEE_RawImageryBands/2024/", pattern = "*.tif$", full.names = TRUE)
rstack <- rast(band_files)
rstack <- rstack[[1:1]]

# Optional: aggregate to coarser resolution for speed
# rstack <- aggregate(rstack, fact = 5, fun = mean)  
# rstack <- aggregate(rstack, fact = 2, fun = mean)  
plot(rstack)
res(rstack)
# ncell(rstack$B11)
# global(!is.na(rstack$B11), "sum", na.rm = TRUE) # 761,368,329 land pixels


#### Load training polygons ####
#' start with the original ones
classes <- c("banana", "coffee", "groundcrops", "grass", "bush",
             "trees", "forest", "building", "road", "water")

class_codes <- 0:9

getwd()
train_polys_list <- list()
for (i in seq_along(classes)) {
  shp <- st_read(paste0("Aim 1/Shapefiles/Training Polygons/Training2022_2024_LONGLEAF/", classes[i], ".shp"))
  shp$class <- class_codes[i]
  shp <- shp %>% dplyr::select(Name, class)
  train_polys_list[[i]] <- shp
}

train_polys <- do.call(rbind, train_polys_list)

#' now add in the additional ones i created after
#' deciding to do classification in longleaf in r
classes <- c("bananaLL", "coffeeLL", "groundcropsLL", "grassLL", "bushLL",
             "treesLL", "forestLL", "buildingLL", "roadLL", "waterLL")

class_codes <- 0:9

getwd()
train_polys_listLL <- list()
for (i in seq_along(classes)) {
  shp <- st_read(paste0("Aim 1/Shapefiles/Training Polygons/Training2022_2024_LONGLEAF/", classes[i], ".shp"))
  shp$class <- class_codes[i]
  shp <- shp %>% dplyr::select(Name, class)
  train_polys_listLL[[i]] <- shp
}

train_polysLL <- do.call(rbind, train_polys_listLL)

# combine original and additional training polys
train_polys_merge <- rbind(train_polys, train_polysLL)
train_polys_merge$ID <- 1:nrow(train_polys_merge)


#### Extract raster values under polygons ####
training_data <- extract(rstack, train_polys_merge, df = TRUE)

# proportion of pixels included in training polys
nrow(training_data)/761368329*100

training_datamerge <- training_data %>%
  left_join(train_polys_merge, by = c("ID")) %>%
  dplyr::select(-c(geometry)) %>%
  na.omit()

class_props <- prop.table(table(training_datamerge$class)) %>%
  as.data.frame() %>%
  rename(class = Var1, proportion = Freq) %>%
  mutate(percent = round(proportion * 100, 2))
class_props

# collapsed props
class_props <- prop.table(table(training_datamerge$class))
df <- as.data.frame(class_props) %>%
  rename(class = Var1, proportion = Freq) %>%
  mutate(class = as.integer(as.character(class)))
collapsed <- df %>%
  mutate(group = case_when(
    class %in% 0:2 ~ "0-2",
    class %in% 3:4 ~ "3-4",
    class %in% 5:6 ~ "5-6",
    class %in% 7:8 ~ "7-8",
    class == 9    ~ "9"
  )) %>%
  group_by(group) %>%
  summarise(proportion = sum(proportion)) %>%
  mutate(percent = round(proportion * 100, 2))

collapsed

#' sample down the ground crop class as it really dominates\
#' in size rel to the others
class2 <- training_datamerge %>% 
  filter(class == 2) %>% 
  sample_frac(0.75)   # keep 90%
others <- training_datamerge %>% 
  filter(class != 2) # keep 100%

# recombine
training_datamerge2 <- bind_rows(class2, others)

# recalc class props
class_props <- prop.table(table(training_datamerge2$class)) %>%
  as.data.frame() %>%
  rename(class = Var1, proportion = Freq) %>%
  mutate(percent = round(proportion * 100, 2))
class_props

# collapsed
class_props <- prop.table(table(training_datamerge2$class))
df <- as.data.frame(class_props) %>%
  rename(class = Var1, proportion = Freq) %>%
  mutate(class = as.integer(as.character(class)))
collapsed <- df %>%
  mutate(group = case_when(
    class %in% 0:2 ~ "0-2",
    class %in% 3:4 ~ "3-4",
    class %in% 5:6 ~ "5-6",
    class %in% 7:8 ~ "7-8",
    class == 9    ~ "9"
  )) %>%
  group_by(group) %>%
  summarise(proportion = sum(proportion)) %>%
  mutate(percent = round(proportion * 100, 2))

collapsed



#### Split into train/test ####
set.seed(123)
training_datamerge <- training_datamerge %>%
  dplyr::select(-c(ID,Name))
train_index <- createDataPartition(training_datamerge$class, p = 0.7, list = FALSE)
train_set <- training_datamerge[train_index, ]
test_set  <- training_datamerge[-train_index, ]

#### Train Random Forest using ranger ####
rf_model <- ranger(
  formula = as.factor(class) ~ .,
  data = train_set,
  num.trees = 500,
  importance = "impurity",
  num.threads = 4, # Adjust for Longleaf cores
  verbose = TRUE
)

# Save variable importance
var_imp <- rf_model$variable.importance
var_imp
write.csv(var_imp, "Aim 1/ClassificationOutput/variable_importance.csv")

#### Predict training and test set ####
train_set$pred_class <- predict(rf_model, data = train_set)$predictions
test_set$pred_class <- predict(rf_model, data = test_set)$predictions

# Save prediction results
write.csv(train_set, "Aim 1/ClassificationOutput/train_set_classresults.csv")
write.csv(test_set, "Aim 1/ClassificationOutput/test_set_classresults.csv")

#### Confusion matrix function ####
calc_cm <- function(df, class_map) {
  cm_long <- df %>%
    mutate(Observed_new = class_map[as.character(class)],
           Predicted_new = class_map[as.character(pred_class)]) %>%
    group_by(Observed_new, Predicted_new) %>%
    summarise(Freq = n(), .groups = "drop")
  
  cm_wide <- cm_long %>%
    pivot_wider(names_from = Predicted_new, values_from = Freq, values_fill = 0)
  
  expanded_obs <- rep(cm_long$Observed_new, cm_long$Freq)
  expanded_pred <- rep(cm_long$Predicted_new, cm_long$Freq)
  
  cm_final <- confusionMatrix(factor(expanded_pred, levels = unique(expanded_obs)),
                              factor(expanded_obs, levels = unique(expanded_obs)))
  
  list(long = cm_long, wide = cm_wide, cm = cm_final)
}

class_map <- c(
  "0" = "Crops", "1" = "Crops", "2" = "Crops",
  "3" = "GrassBush", "4" = "GrassBush",
  "5" = "Trees", "6" = "Trees",
  "7" = "Urban", "8" = "Urban",
  "9" = "Water"
)

cm_train <- calc_cm(train_set, class_map)
cm_test  <- calc_cm(test_set, class_map)
cm_test
write.csv(cm_train$wide, "Aim 1/ClassificationOutput/cm_train_confusionmatrix.csv")
write.csv(cm_test$wide, "Aim 1/ClassificationOutput/cm_test_confusionmatrix.csv")

print(cm_train$cm)
print(cm_test$cm)

#### Classify entire raster stack ####
# terra::predict will process in blocks (memory efficient)
classified_raster <- predict(
  rstack,
  rf_model,
  fun = function(model, ...) predict(model, data = as.data.frame(...))$predictions,
  filename = "Aim 1/ClassificationOutput/ClassifiedImages/classification2024.tif",
  overwrite = TRUE
)

plot(classified_raster, main = "Classified Raster")











#### OLD ####

library(tidyverse)
library(terra)
library(sf)
library(randomForest)
getwd()

#### Test image classification in R ####

#### Load bands ####
b2 <- rast("Aim 1/GEE_RawImageryBands/2024/final_B2_float32_compressed.tif")
b3 <- rast("Aim 1/GEE_RawImageryBands/2024/final_B3_float32_compressed.tif")
b4 <- rast("Aim 1/GEE_RawImageryBands/2024/final_B4_float32_compressed.tif")
b5 <- rast("Aim 1/GEE_RawImageryBands/2024/final_B5_float32_compressed.tif")
b6 <- rast("Aim 1/GEE_RawImageryBands/2024/final_B6_float32_compressed.tif")
b7 <- rast("Aim 1/GEE_RawImageryBands/2024/final_B7_float32_compressed.tif")
b8 <- rast("Aim 1/GEE_RawImageryBands/2024/final_B8_float32_compressed.tif")
b8A <- rast("Aim 1/GEE_RawImageryBands/2024/final_B8A_float32_compressed.tif")
b11 <- rast("Aim 1/GEE_RawImageryBands/2024/final_B11_float32_compressed.tif")
b12 <- rast("Aim 1/GEE_RawImageryBands/2024/final_B12_float32_compressed.tif")
NDVI <- rast("Aim 1/GEE_RawImageryBands/2024/final_NDVI_float32_compressed.tif")
NDVISD <- rast("Aim 1/GEE_RawImageryBands/2024/final_NDVISD_float32_compressed.tif")
NIRfocal <- rast("Aim 1/GEE_RawImageryBands/2024/final_NIRfocal_float32_compressed.tif")
Redfocal <- rast("Aim 1/GEE_RawImageryBands/2024/final_Redfocal_float32_compressed.tif")
Redfocal2 <- rast("Aim 1/GEE_RawImageryBands/2024/final_Redfocal2_float32_compressed.tif")

image <- c(b2, b3, b4, b5, b6) #, b7, b8, b8A, b11, b12,
           # NDVI, NDVISD, NIRfocal, Redfocal, Redfocal2)
image <- aggregate(image, fact = 10, fun = mean)  
image <- aggregate(image, fact = 5, fun = mean)  
plot(image)
res(image)


#### Read in training polys ####
# 1. Load your raster stack
rstack <- image

# 2. Load polygons from separate shapefiles and assign class codes
banana <- st_read("Aim 1/Shapefiles/Training Polygons/Training2022_2024_LONGLEAF/banana22_24.shp")
coffee <- st_read("Aim 1/Shapefiles/Training Polygons/Training2022_2024_LONGLEAF/coffee22_24.shp")
groundcrops <- st_read("Aim 1/Shapefiles/Training Polygons/Training2022_2024_LONGLEAF/groundcrops22_24.shp")
grass <- st_read("Aim 1/Shapefiles/Training Polygons/Training2022_2024_LONGLEAF/grass22_24.shp")
bush <- st_read("Aim 1/Shapefiles/Training Polygons/Training2022_2024_LONGLEAF/bush22_24.shp")
trees <- st_read("Aim 1/Shapefiles/Training Polygons/Training2022_2024_LONGLEAF/trees22_24.shp")
forest <- st_read("Aim 1/Shapefiles/Training Polygons/Training2022_2024_LONGLEAF/forest22_24.shp")
building <- st_read("Aim 1/Shapefiles/Training Polygons/Training2022_2024_LONGLEAF/building22_24.shp")
road <- st_read("Aim 1/Shapefiles/Training Polygons/Training2022_2024_LONGLEAF/road22_24.shp")
water <- st_read("Aim 1/Shapefiles/Training Polygons/Training2022_2024_LONGLEAF/water.shp")

banana$class <- 0
coffee$class <- 1
groundcrops$class <- 2
grass$class <- 3
bush$class <- 4
trees$class <- 5
forest$class <- 6
building$class <- 7
road$class <- 8
water$class <- 9

banana <- banana %>% dplyr::select(Name, class)
coffee <- coffee %>% dplyr::select(Name, class)
groundcrops <- groundcrops %>% dplyr::select(Name, class)
grass <- grass %>% dplyr::select(Name, class)
bush <- bush %>% dplyr::select(Name, class)
trees <- trees %>% dplyr::select(Name, class)
forest <- forest %>% dplyr::select(Name, class)
building <- building %>% dplyr::select(Name, class)
road <- road %>% dplyr::select(Name, class)
water <- water %>% dplyr::select(Name, class)

# 3. Merge all polygons into one training dataset
train_polys <- rbind(banana, coffee, groundcrops, grass,
                     bush, trees, forest, building, road,
                     water)
train_polys$ID <- 1:nrow(train_polys)
  

#### Create data frame for classification ####
# 4. Extract raster values under polygons
training_data <- extract(rstack, train_polys, df = TRUE)

    # Add class labels (from polygons) to extracted data
training_datamerge <- training_data %>%
  left_join(train_polys) %>%
  dplyr::select(-c(geometry)) %>%
  dplyr::select(-c(ID,Name))

    # Drop ID and remove NA rows
training_datamerge <- na.omit(training_datamerge)
summary(training_datamerge)


# 5. Split data into training (70%) and test (30%)
library(caret)
set.seed(123)  # for reproducibility
train_index <- createDataPartition(training_datamerge$class, p = 0.7, list = FALSE)
train_set <- training_datamerge[train_index, ]
test_set  <- training_datamerge[-train_index, ]


#### Run model ####
# 6. Train Random Forest on training set
rf_model <- randomForest(as.factor(class) ~ ., data = train_set)
print(rf_model)


#### Confusion matrices ####

##### CM training #####
# 7. Predict on training set
train_set$pred_class <- predict(rf_model, newdata = train_set)
confusionMatrix(factor(train_set$pred_class),
                            factor(train_set$class))

# save train set df
write.csv(train_set,
          "Aim 1/ClassificationOutput/train_set_classresults.csv")

# 8. consolidate classes for train set
class_map <- c(
  "0" = "Crops",
  "1" = "Crops",
  "2" = "Crops",
  "3" = "GrassBush",
  "4" = "GrassBush",
  "5" = "Trees",
  "6" = "Trees",
  "7" = "Urban",
  "8" = "Urban",
  "9" = "Water"
)

cm_trainlong <- data.frame(table(Predicted = train_set$pred_class, 
                      Observed = train_set$class))
cm_trainlong <- cm_trainlong %>%
  mutate(
    Observed_new = class_map[as.character(Observed)],
    Predicted_new = class_map[as.character(Predicted)]
  )

cm_consolidated <- cm_trainlong %>%
  group_by(Observed_new, Predicted_new) %>%
  summarise(Freq = sum(Freq), .groups = "drop")

cm_wide <- cm_consolidated %>%
  pivot_wider(names_from = Predicted_new, values_from = Freq, values_fill = 0)

write.csv(cm_wide,
          "Aim 1/ClassificationOutput/cm_train_confusionmatrix.csv")

# get consolidated conf matrix indices
expanded_obs <- rep(cm_consolidated$Observed_new, cm_consolidated$Freq)
expanded_pred <- rep(cm_consolidated$Predicted_new, cm_consolidated$Freq)

cm_final <- confusionMatrix(factor(expanded_pred, levels = unique(expanded_obs)),
                            factor(expanded_obs, levels = unique(expanded_obs)))

print(cm_final)


##### CM test set #####
# 8. Predict on test set
test_set$pred_class <- predict(rf_model, newdata = test_set)

# save it
write.csv(test_set,
          "Aim 1/ClassificationOutput/test_set_classresults.csv")

confusionMatrix(factor(test_set$pred_class),
                factor(test_set$class))
  

# 9. consolidate classes for test set
cm_testlong <- data.frame(table(Predicted = test_set$pred_class, 
                                 Observed = test_set$class))
cm_testlong <- cm_testlong %>%
  mutate(
    Observed_new = class_map[as.character(Observed)],
    Predicted_new = class_map[as.character(Predicted)]
  )

cm_consolidated_test <- cm_testlong %>%
  group_by(Observed_new, Predicted_new) %>%
  summarise(Freq = sum(Freq), .groups = "drop")

cm_wide_test <- cm_consolidated_test %>%
  pivot_wider(names_from = Predicted_new, values_from = Freq, values_fill = 0)
write.csv(cm_wide_test,
          "Aim 1/ClassificationOutput/cm_test_confusionmatrix.csv")

# get consolidated conf matrix indices
expanded_obs_test <- rep(cm_consolidated_test$Observed_new, cm_consolidated_test$Freq)
expanded_pred_test <- rep(cm_consolidated_test$Predicted_new, cm_consolidated_test$Freq)

cm_final_test <- confusionMatrix(factor(expanded_pred_test, levels = unique(expanded_obs_test)),
                            factor(expanded_obs_test, levels = unique(expanded_obs_test)))

print(cm_final_test)



#### Run mod on full dataset ####
# 10. Classify entire raster stack on model trained on everything
rf_modelFull <- randomForest(as.factor(class) ~ ., data = training_datamerge)
print(rf_modelFull)

##### Predict raster #####
classified <- predict(rstack, rf_modelFull, na.rm=TRUE)
plot(classified, main = "Classified Raster")

write.csv(classified,
          "Aim 1/ClassificationOutput/ClassifiedImages/classification2024.tif")







