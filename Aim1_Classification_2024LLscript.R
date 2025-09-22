

#### Top ####
library(tidyverse)
library(terra)
library(sf)
library(ranger)
library(caret)
library(raster)

#### Terra options ####
terraOptions(
  progress = 1,
  tempdir = Sys.getenv("TMPDIR"),  # fast, job-specific scratch
  memfrac = 0.7,
  threads = 24
)

#### Load raster bands ####
band_files <- list.files("/users/c/t/ctelford/Rasters/Year2024/", pattern = "*.tif$", full.names = TRUE)
rstack <- rast(band_files)


# # Optional: aggregate to coarser resolution for speed
# rstack <- aggregate(rstack, fact = 5, fun = mean)
# rstack <- aggregate(rstack, fact = 2, fun = mean)
# plot(rstack)
# res(rstack)
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
  shp <- st_read(paste0("/users/c/t/ctelford/TrainingPolygons/Training2022_2024/", classes[i], ".shp"))
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
  shp <- st_read(paste0("/users/c/t/ctelford/TrainingPolygons/Training2022_2024/", classes[i], ".shp"))
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

# # proportion of pixels included in training polys
# nrow(training_data)/761368329*100

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
  num.threads = 24, # Adjust for Longleaf cores
  verbose = TRUE
)

# Save variable importance
var_imp <- rf_model$variable.importance
var_imp
write.csv(var_imp, "/users/c/t/ctelford/ClassificationOutput/Year2024/variable_importance.csv")

#### Predict training and test set ####
train_set$pred_class <- predict(rf_model, data = train_set)$predictions
test_set$pred_class <- predict(rf_model, data = test_set)$predictions

# Save prediction results
write.csv(train_set, "/users/c/t/ctelford/ClassificationOutput/Year2024/train_set_classresults.csv")
write.csv(test_set, "/users/c/t/ctelford/ClassificationOutput/Year2024/test_set_classresults.csv")

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
write.csv(cm_train$wide, "/users/c/t/ctelford/ClassificationOutput/Year2024/cm_train_confusionmatrix.csv")
write.csv(cm_test$wide, "/users/c/t/ctelford/ClassificationOutput/Year2024/cm_test_confusionmatrix.csv")

# print(cm_train$cm)
# print(cm_test$cm)


#### Classify entire raster stack ####
# terra::predict will process in blocks (memory efficient)

classified_raster <- predict(
  rstack,
  rf_model,
  na.rm = TRUE,
  filename = "/users/c/t/ctelford/ClassificationOutput/Year2024/classification2024.tif",
  overwrite = TRUE,
  wopt = list(
    datatype = "INT1U",  # 1 byte per pixel
    gdal = c(
      "COMPRESS=DEFLATE",  # good compression
      "TILED=YES",          # enable tiles
      "BLOCKXSIZE=1024",    # larger tiles
      "BLOCKYSIZE=1024",
      "ZLEVEL=9"            # max compression
    )
  )
)

# plot(classified_raster, main = "Classified Raster")




