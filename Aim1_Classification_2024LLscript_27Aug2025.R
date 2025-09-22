#### Top ####

# Use your personal library on Longleaf
.libPaths("/nas/longleaf/home/ctelford/R/x86_64-pc-linux-gnu-library/4.4")

# Load packages
library(tidyverse)
library(ranger)
library(caret)
library(terra)
library(sf)
library(raster)


#### Output folder (persistent, original location) ####
output_folder <- "/users/c/t/ctelford/ClassificationOutput/Year2024/"
raster_folder <- "/users/c/t/ctelford/Rasters/Year2024/"
poly_folder <- "/users/c/t/ctelford/TrainingPolygons/Training2022_2024/"

dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(raster_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(poly_folder, recursive = TRUE, showWarnings = FALSE)


#### Terra options (fast scratch) ####
terraOptions(
  progress = 1,
  tempdir = Sys.getenv("TMPDIR"),  # fast, job-specific scratch
  memfrac = 0.5,
  threads = 16
)
cat("Step 1: Terra tempdir set to", Sys.getenv("TMPDIR"), "\n")

#### Load raster bands ####
band_files <- list.files(raster_folder, pattern = "*.tif$", full.names = TRUE)
rstack <- rast(band_files)
# rstack <- rstack[[1:3]]
# rstack <- aggregate(rstack, fact=10)
# rstack <- aggregate(rstack, fact=2)
cat("Step 2: Loaded raster stack with", nlyr(rstack), "bands\n")

#### Load training polygons ####
classes <- c("banana", "coffee", "groundcrops", "grass", "bush",
             "trees", "forest", "building", "road", "water")
class_codes <- 0:9

train_polys_list <- list()
for (i in seq_along(classes)) {
  shp <- st_read(paste0(poly_folder, classes[i], ".shp"))
  shp$class <- class_codes[i]
  shp <- shp %>% dplyr::select(Name, class)
  train_polys_list[[i]] <- shp
}
train_polys <- do.call(rbind, train_polys_list)

# Additional LL polygons
classesLL <- c("bananaLL", "coffeeLL", "groundcropsLL", "grassLL", "bushLL",
               "treesLL", "forestLL", "buildingLL", "roadLL", "waterLL")
train_polys_listLL <- list()
for (i in seq_along(classesLL)) {
  shp <- st_read(paste0(poly_folder, classesLL[i], ".shp"))
  shp$class <- class_codes[i]
  shp <- shp %>% dplyr::select(Name, class)
  train_polys_listLL[[i]] <- shp
}
train_polysLL <- do.call(rbind, train_polys_listLL)

# Combine polygons
train_polys_merge <- rbind(train_polys, train_polysLL)
train_polys_merge$ID <- 1:nrow(train_polys_merge)
cat("Step 3: Loaded training polygons\n")



#### Extract raster values under polygons ####
training_data <- extract(rstack, train_polys_merge, df = TRUE)
training_datamerge <- training_data %>%
  left_join(train_polys_merge, by = c("ID")) %>%
  dplyr::select(-c(geometry)) %>%
  na.omit()
cat("Step 4: Extracted raster values under polygons\n")

# Sample down dominant class (ground crops, class 2)
class2 <- training_datamerge %>% filter(class == 2) %>% sample_frac(0.75)
others <- training_datamerge %>% filter(class != 2)
training_datamerge2 <- bind_rows(class2, others)
cat("Step 5: Resampled dominant classes\n")

#### Split into train/test ####
set.seed(123)
training_datamerge2 <- training_datamerge2 %>% dplyr::select(-c(ID, Name))
train_index <- createDataPartition(training_datamerge2$class, p = 0.7, list = FALSE)
train_set <- training_datamerge2[train_index, ]
test_set  <- training_datamerge2[-train_index, ]
cat("Step 6: Split into train/test sets\n")

#### Train Random Forest using ranger ####
library(doParallel)
n_cores <- 16
cl <- makeCluster(n_cores)
registerDoParallel(cl)

train_ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,  # Print progress
  allowParallel = TRUE  # Enable caret parallelization
)

rf_grid <- expand.grid(
  mtry = c(3,5,7),  # Adjust based on predictors
  splitrule = "gini",
  min.node.size = c(10,20) # Include min.node.size for tuning
)

# Train with caret
rf_model <- train(
  as.factor(class) ~ ., 
  data = train_set,
  method = "ranger",
  trControl = train_ctrl,
  tuneGrid = rf_grid,
  num.trees = 300,
  importance = "impurity",
  num.threads = n_cores   # allows ranger to use multiple cores per model
)
print(rf_model)

stopCluster(cl)
registerDoSEQ()

cat("Step 7: Trained random forest\n")


#### Predict training and test set ####
train_set$pred_class <- predict(rf_model, newdata=train_set)
test_set$pred_class <- predict(rf_model, newdata = test_set)

write.csv(train_set, file.path(output_folder, "train_set_classresults.csv"))
write.csv(test_set, file.path(output_folder, "test_set_classresults.csv"))
cat("Step 9: Saved train/test predictions\n")

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
cm_train
cm_test
write.csv(cm_train$wide, file.path(output_folder, "cm_train_confusionmatrix.csv"))
write.csv(cm_test$wide, file.path(output_folder, "cm_test_confusionmatrix.csv"))
saveRDS(cm_test$cm, file.path(output_folder, "cm_testconfusionmatrix.rds"))
saveRDS(cm_train$cm, file.path(output_folder, "cm_trainconfusionmatrix.rds"))
cat("Step 10: Saved confusion matrices\n")

#### Classify entire raster stack ####
# Remove only specific objects
rm(training_datamerge,
   training_data,
   train_polys_merge,
   train_polysLL,
   train_set, 
   test_set,
   cm_train,
   cm_test)

# Free memory
gc()

# #### OLD: clasify without tile splitting ####
# classified_raster <- predict(
#   rstack,
#   rf_model,
#   na.rm = TRUE,
#   filename = file.path(output_folder, "classification2024.tif"),
#   overwrite = TRUE,
#   wopt = list(
#     datatype = "INT1U",
#     gdal = c("COMPRESS=DEFLATE", "TILED=YES",
#              "BLOCKXSIZE=512", "BLOCKYSIZE=512", "ZLEVEL=9")
#   )
# )
# plot(classified_raster)
# cat("Step 11: Finished classifying raster stack\n")



##### divide into tiles #####
library(terra)
library(parallel)
nrows <- 10
ncols <- 10
ext_full <- ext(rstack)

# Calculate tile extents
xmins <- seq(ext_full[1], ext_full[2], length.out = ncols + 1)[1:ncols]
xmaxs <- seq(ext_full[1], ext_full[2], length.out = ncols + 1)[2:(ncols + 1)]
ymins <- seq(ext_full[3], ext_full[4], length.out = nrows + 1)[1:nrows]
ymaxs <- seq(ext_full[3], ext_full[4], length.out = nrows + 1)[2:(nrows + 1)]

tile_extents <- expand.grid(i = 1:nrows, j = 1:ncols)
tile_extents$k <- seq_len(nrow(tile_extents))

# Save model to disk once
model_file <- file.path(output_folder, "rf_model.rds")
saveRDS(rf_model, model_file)

# Save raster pointer to disk if not already (terra works best with filenames)
rstack_file <- file.path(output_folder, "rstack.tif")
writeRaster(rstack, rstack_file, overwrite = TRUE)

# Function to process one tile
process_tile <- function(idx, tile_extents, xmins, xmaxs, ymins, ymaxs, rstack_file, model_file, output_folder) {
  library(terra)  # load in worker
  
  i <- tile_extents$i[idx]
  j <- tile_extents$j[idx]
  k <- tile_extents$k[idx]
  
  # reload objects inside worker
  rstack <- rast(rstack_file)
  rf_model <- readRDS(model_file)
  
  tile_ext <- ext(xmins[j], xmaxs[j], ymins[i], ymaxs[i])
  
  if (xmins[j] >= xmaxs[j] || ymins[i] >= ymaxs[i]) {
    stop(paste("Invalid extent for tile", k))
  }
  
  rstack_tile <- crop(rstack, tile_ext)
  tile_file <- file.path(output_folder, paste0("tile_", k, "_classification2024.tif"))
  
  if (file.exists(tile_file)) file.remove(tile_file)
  
  predict(
    rstack_tile,
    rf_model,
    na.rm = TRUE,
    filename = tile_file,
    overwrite = TRUE,
    wopt = list(
      datatype = "INT1U",
      gdal = c("COMPRESS=DEFLATE", "TILED=YES",
               "BLOCKXSIZE=512", "BLOCKYSIZE=512", "ZLEVEL=9")
    )
  )
  
  return(tile_file)
}

##### run in parallel #####
n_cores <- 16
cl <- makeCluster(n_cores)
clusterExport(cl, "process_tile")
clusterExport(cl, c("tile_extents", "xmins", "xmaxs", "ymins", "ymaxs", 
                    "rstack_file", "model_file", "output_folder"))

tile_files <- parLapply(
  cl,
  seq_len(nrow(tile_extents)),
  function(idx) {
    process_tile(idx, tile_extents, xmins, xmaxs, ymins, ymaxs, rstack_file, model_file, output_folder)
  }
)

stopCluster(cl)

##### merge results #####
mytiles <- lapply(tile_files, rast)
mysprc <- sprc(mytiles)

classified_raster <- merge(
  mysprc,
  filename = file.path(output_folder, "classification2024.tif"),
  overwrite = TRUE
)


