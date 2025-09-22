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
class2 <- training_datamerge %>% filter(class == 2) %>% sample_frac(0.8)
class4 <- training_datamerge %>% filter(class == 4) %>% sample_frac(0.8)
others <- training_datamerge %>% filter(!class %in% c(2,4))
table(others$class)
training_datamerge2 <- bind_rows(class2, class4, others)
table(training_datamerge2$class)
prop.table(table(training_datamerge2$class))
cat("Step 5: Resampled dominant classes\n")

#### Split into train/test ####
#' going to assign to train/test by polygon ID
set.seed(123)

# 1. get polygon ids with their corresponding class
poly_info <- training_datamerge2 %>%
  distinct(ID, class)
prop.table(table(poly_info$class))

# 2. for each class, randomly assign 70% of polygons to training
train_polys <- poly_info %>%
  group_by(class) %>%
  sample_frac(0.7) %>%
  ungroup()
prop.table(table(train_polys$class))

# 3. split dataset based on polygon membership
train_set <- training_datamerge2 %>% filter(ID %in% train_polys$ID)
test_set  <- training_datamerge2 %>% filter(!ID %in% train_polys$ID)
prop.table(table(train_set$class))
prop.table(table(test_set$class))

# 4. double check that none of the IDs in training are in testing
train_ids <- unique(train_set$ID)
test_ids  <- unique(test_set$ID)
intersect_ids <- intersect(train_ids, test_ids)
length(intersect_ids) 

cat("Step 6: Split into train/test sets\n")


#### Train Random Forest using ranger ####
# define 5 folds by poly ID
unique_polys <- unique(train_set$ID)
folds <- createFolds(unique_polys, k = 5, returnTrain = TRUE)

# Map back from polygon IDs to row indices in train_set
index <- lapply(folds, function(poly_ids) {
  which(train_set$ID %in% unique_polys[poly_ids])
})

# remove ID and name
train_set <- train_set %>% dplyr::select(-c(ID, Name))
test_set <- test_set %>% dplyr::select(-c(ID, Name))

# run model selecting hyperparams that optimize 5 fold cv
library(doParallel)
n_cores <- 16
cl <- makeCluster(n_cores)
registerDoParallel(cl)

train_ctrl <- trainControl(
  method = "cv",
  number = 5,
  index = index, # my custom defined folds
  verboseIter = TRUE,  # Print progress
  allowParallel = TRUE  # Enable caret parallelization
)

rf_grid <- expand.grid(
  mtry = c(3,5,7),  # Adjust based on predictors
  splitrule = "gini",
  min.node.size = c(10,15,20) # Include min.node.size for tuning
)

# Train with caret
rf_model <- train(
  as.factor(class) ~ ., 
  data = train_set,
  method = "ranger",
  trControl = train_ctrl,
  tuneGrid = rf_grid,
  num.trees = 400,
  importance = "impurity",
  num.threads = n_cores   # allows ranger to use multiple cores per model
)

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





# #### Classify entire raster stack ####
# ##### divide into tiles #####
# library(terra)
# library(parallel)
# nrows <- 2
# ncols <- 2
# ext_full <- ext(rstack)
# 
# # Calculate tile extents
# xmins <- seq(ext_full[1], ext_full[2], length.out = ncols + 1)[1:ncols]
# xmaxs <- seq(ext_full[1], ext_full[2], length.out = ncols + 1)[2:(ncols + 1)]
# ymins <- seq(ext_full[3], ext_full[4], length.out = nrows + 1)[1:nrows]
# ymaxs <- seq(ext_full[3], ext_full[4], length.out = nrows + 1)[2:(nrows + 1)]
# 
# tile_extents <- expand.grid(i = 1:nrows, j = 1:ncols)
# tile_extents$k <- seq_len(nrow(tile_extents))
# 
# tile_ext1 <- ext(xmins[1], xmaxs[1], ymins[1], ymaxs[1])
# tile_ext2 <- ext(xmins[2], xmaxs[2], ymins[1], ymaxs[1])
# tile_ext3 <- ext(xmins[1], xmaxs[1], ymins[2], ymaxs[2])
# tile_ext4 <- ext(xmins[2], xmaxs[2], ymins[2], ymaxs[2])
# 
# rstack_tile1 <- crop(rstack, tile_ext1)
# rstack_tile2 <- crop(rstack, tile_ext2)
# rstack_tile3 <- crop(rstack, tile_ext3)
# rstack_tile4 <- crop(rstack, tile_ext4)
# 
# # save raster tiles and then clear memory
# rstack_file1 <- file.path(output_folder, "rstack_tile1.tif")
# rstack_file2 <- file.path(output_folder, "rstack_tile2.tif")
# rstack_file3 <- file.path(output_folder, "rstack_tile3.tif")
# rstack_file4 <- file.path(output_folder, "rstack_tile4.tif")
# 
# writeRaster(rstack_tile1, rstack_file1, overwrite = TRUE)
# writeRaster(rstack_tile2, rstack_file2, overwrite = TRUE)
# writeRaster(rstack_tile3, rstack_file3, overwrite = TRUE)
# writeRaster(rstack_tile4, rstack_file4, overwrite = TRUE)
# 
# # Remove only specific objects
# rm(training_datamerge,
#    training_data,
#    train_polys_merge,
#    train_polysLL,
#    train_set, 
#    test_set,
#    cm_train,
#    cm_test,
#    others,
#    train_ctrl,
#    train_polys_list,
#    train_polys_listLL,
#    training_datamerge2,
#    rstack,
#    rstack_tile1,
#    rstack_tile2,
#    rstack_tile3,
#    rstack_tile4)
# 
# # Free memory
# gc()
# 
# 
# #### Load each tile individually, predict, then save pred tile ####
# ##### Tile 1 #####
# mytile <- list.files(output_folder, pattern = "*.tif$", full.names = TRUE)
# rstack_tile1 <- rast(rstack_file1)
# 
# classified_tile1 <- predict(
#   rstack_tile1,
#   rf_model,
#   na.rm = TRUE,
#   filename = file.path(output_folder, "classification_2024_tile1.tif"),
#   overwrite = TRUE,
#   wopt = list(
#     datatype = "INT1U",
#     gdal = c("COMPRESS=DEFLATE", "TILED=YES",
#              "BLOCKXSIZE=512", "BLOCKYSIZE=512", "ZLEVEL=9")
#   )
# )
# 
# # Remove specific objects to free memory
# rm(classified_tile1, rstack_tile1)
# gc()
# 
# 
# ##### Tile 2 #####
# mytile <- list.files(output_folder, pattern = "*.tif$", full.names = TRUE)
# rstack_tile2 <- rast(rstack_file2)
# 
# classified_tile2 <- predict(
#   rstack_tile2,
#   rf_model,
#   na.rm = TRUE,
#   filename = file.path(output_folder, "classification_2024_tile2.tif"),
#   overwrite = TRUE,
#   wopt = list(
#     datatype = "INT1U",
#     gdal = c("COMPRESS=DEFLATE", "TILED=YES",
#              "BLOCKXSIZE=512", "BLOCKYSIZE=512", "ZLEVEL=9")
#   )
# )
# # Remove specific objects to free memory
# rm(classified_tile2, rstack_tile2)
# gc()
# 
# ##### Tile 3 #####
# mytile <- list.files(output_folder, pattern = "*.tif$", full.names = TRUE)
# rstack_tile3 <- rast(rstack_file3)
# 
# classified_tile3 <- predict(
#   rstack_tile3,
#   rf_model,
#   na.rm = TRUE,
#   filename = file.path(output_folder, "classification_2024_tile3.tif"),
#   overwrite = TRUE,
#   wopt = list(
#     datatype = "INT1U",
#     gdal = c("COMPRESS=DEFLATE", "TILED=YES",
#              "BLOCKXSIZE=512", "BLOCKYSIZE=512", "ZLEVEL=9")
#   )
# )
# # Remove specific objects to free memory
# rm(classified_tile3, rstack_tile3)
# gc()
# 
# ##### Tile 4 #####
# mytile <- list.files(output_folder, pattern = "*.tif$", full.names = TRUE)
# rstack_tile4 <- rast(rstack_file4)
# 
# classified_tile4 <- predict(
#   rstack_tile4,
#   rf_model,
#   na.rm = TRUE,
#   filename = file.path(output_folder, "classification_2024_tile4.tif"),
#   overwrite = TRUE,
#   wopt = list(
#     datatype = "INT1U",
#     gdal = c("COMPRESS=DEFLATE", "TILED=YES",
#              "BLOCKXSIZE=512", "BLOCKYSIZE=512", "ZLEVEL=9")
#   )
# )
# # Remove specific objects to free memory
# rm(classified_tile4, rstack_tile4)
# gc()
# 
# ##### load and merge the 4 tiles #####
# mytile <- list.files(output_folder, pattern = "*.tif$", full.names = TRUE)
# pred_tile1 <- rast(mytile[grep("classification_2024_tile1", mytile)])
# pred_tile2 <- rast(mytile[grep("classification_2024_tile2", mytile)])
# pred_tile3 <- rast(mytile[grep("classification_2024_tile3", mytile)])
# pred_tile4 <- rast(mytile[grep("classification_2024_tile4", mytile)])
# 
# # merge the tiles
# classification2024 <- mosaic(pred_tile1, 
#                             pred_tile2,
#                             pred_tile3,
#                             pred_tile4)
# 
# class_file <- file.path(output_folder, "merged_class_tile_2024.tif")
# writeRaster(classification2024, class_file, overwrite = TRUE)
# 
