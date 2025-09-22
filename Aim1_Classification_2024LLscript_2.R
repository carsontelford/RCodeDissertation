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
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

#### Terra options (fast scratch) ####
terraOptions(
  progress = 1,
  tempdir = Sys.getenv("TMPDIR"),  # fast, job-specific scratch
  memfrac = 0.5,
  threads = 16
)
cat("Step 1: Terra tempdir set to", Sys.getenv("TMPDIR"), "\n")

#### Load raster bands ####
band_files <- list.files("/users/c/t/ctelford/Rasters/Year2024/", pattern = "*.tif$", full.names = TRUE)
rstack <- rast(band_files)
cat("Step 2: Loaded raster stack with", nlyr(rstack), "bands\n")

#### Load training polygons ####
classes <- c("banana", "coffee", "groundcrops", "grass", "bush",
             "trees", "forest", "building", "road", "water")
class_codes <- 0:9

train_polys_list <- list()
for (i in seq_along(classes)) {
  shp <- st_read(paste0("/users/c/t/ctelford/TrainingPolygons/Training2022_2024/", classes[i], ".shp"))
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
  shp <- st_read(paste0("/users/c/t/ctelford/TrainingPolygons/Training2022_2024/", classesLL[i], ".shp"))
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
rf_model <- ranger(
  formula = as.factor(class) ~ .,
  data = train_set,
  num.trees = 500,
  importance = "impurity",
  num.threads = 16,
  min.node.size = 5,
  verbose = TRUE
)
cat("Step 7: Trained random forest\n")

# Save variable importance
var_imp <- rf_model$variable.importance
write.csv(var_imp, file.path(output_folder, "variable_importance.csv"))
cat("Step 8: Saved variable importance\n")

#### Predict training and test set ####
train_set$pred_class <- predict(rf_model, data = train_set)$predictions
test_set$pred_class <- predict(rf_model, data = test_set)$predictions

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
write.csv(cm_train$wide, file.path(output_folder, "cm_train_confusionmatrix.csv"))
write.csv(cm_test$wide, file.path(output_folder, "cm_test_confusionmatrix.csv"))
write.csv(cm_test$cm, file.path(output_folder, "cm_confusionmatrix.csv"))
cat("Step 10: Saved confusion matrices\n")

#### Classify entire raster stack ####

# Free up memory by removing unnecessary large objects
rm(train_set, test_set, training_datamerge2, class2, others, training_datamerge, training_data,
   train_polys_list, train_polys, train_polys_listLL, train_polysLL, train_polys_merge,
   cm_train, cm_test, cm_long, cm_wide)  # Adjust if any other large temp objects exist
gc()  # Trigger garbage collection to reclaim memory
cat("Memory cleared before raster prediction\n")

classified_raster <- predict(
  rstack,
  rf_model,
  na.rm = TRUE,
  filename = file.path(output_folder, "classification2024.tif"),
  overwrite = TRUE,
  wopt = list(
    datatype = "INT1U",
    gdal = c("COMPRESS=DEFLATE", "TILED=YES",
             "BLOCKXSIZE=512", "BLOCKYSIZE=512", "ZLEVEL=9")
  )
)
cat("Step 11: Finished classifying raster stack\n")


# # try doingit by pieces
# 
# # Get extent of full raster
# ext_full <- ext(rstack)
# 
# # Define number of tiles (2x2 grid, adjust as needed)
# n_tiles_x <- 2
# n_tiles_y <- 2
# tile_width <- (ext_full$xmax - ext_full$xmin) / n_tiles_x
# tile_height <- (ext_full$ymax - ext_full$ymin) / n_tiles_y
# 
# # Initialize vector to store temporary tile filenames
# tile_files <- vector("character", n_tiles_x * n_tiles_y)
# 
# # Loop through tiles
# for (i in 1:n_tiles_x) {
#   for (j in 1:n_tiles_y) {
#     tile_idx <- (i-1) * n_tiles_y + j
#     # Define extent for this tile
#     ext_tile <- ext(
#       ext_full$xmin + (i-1) * tile_width,
#       ext_full$xmin + i * tile_width,
#       ext_full$ymin + (j-1) * tile_height,
#       ext_full$ymin + j * tile_height
#     )
#     # Crop raster to tile extent
#     rstack_tile <- crop(rstack, ext_tile)
#     # Define output file for tile
#     tile_file <- file.path(output_folder, paste0("tile_", tile_idx, ".tif"))
#     # Predict on tile
#     predict(
#       rstack_tile,
#       rf_model,
#       na.rm = TRUE,
#       filename = tile_file,
#       overwrite = TRUE,
#       wopt = list(
#         datatype = "INT1U",
#         gdal = c("COMPRESS=DEFLATE", "TILED=YES",
#                  "BLOCKXSIZE=512", "BLOCKYSIZE=512", "ZLEVEL=9")
#       )
#     )
#     # Store filename
#     tile_files[tile_idx] <- tile_file
#     # Clean up tile to free memory
#     rm(rstack_tile)
#     gc()
#     cat("Finished classifying tile", tile_idx, "\n")
#   }
# }
# 
# # Mosaic tiles into final raster
# tiles <- sprc(tile_files)
# classified_raster <- mosaic(tiles, fun = "first",  # Use "first" for classification
#                             filename = file.path(output_folder, "classification2024.tif"),
#                             overwrite = TRUE,
#                             wopt = list(
#                               datatype = "INT1U",
#                               gdal = c("COMPRESS=DEFLATE", "TILED=YES",
#                                        "BLOCKXSIZE=512", "BLOCKYSIZE=512", "ZLEVEL=9")
#                             ))
# # Clean up temporary tile files
# file.remove(tile_files)
# cat("Step 11: Finished classifying and mosaicking raster stack\n")
