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
library(xgboost)
library(dplyr)
library(stats)
library(doParallel)

#### Output folder (persistent, original location) ####
output_folder <- "/users/c/t/ctelford/ClassificationOutput/Year2024/"
raster_folder <- "/users/c/t/ctelford/Rasters/Year2024/"
poly_folder <- "/users/c/t/ctelford/TrainingPolygons/Training2022_2024/"
stack_folder <- "/users/c/t/ctelford/Rasters/FinalStacks/"


dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(raster_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(poly_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(stack_folder, recursive = TRUE, showWarnings = FALSE)


#### Terra options (fast scratch) ####
terraOptions(
  progress = 1,
  tempdir = Sys.getenv("TMPDIR"),  # fast, job-specific scratch
  memfrac = 0.5,
  threads = 48
)
cat("Step 1: Terra tempdir set to", Sys.getenv("TMPDIR"), "\n")

# #### Load raster bands ####
# band_files <- list.files(raster_folder, pattern = "*.tif$", full.names = TRUE)
# rstack <- rast(band_files)
# # rstack <- rstack[[1:3]]
# rstack <- aggregate(rstack, fact=2)
# cat("Step 2: Loaded raster stack with", nlyr(rstack), "bands\n")
# 
# # add focal SD layers
# w5 <- matrix(1,5,5)  # 5x5 window
# w7 <- matrix(1,7,7)  # 5x5 window
# w9 <- matrix(1,9,9)  # 5x5 window
# 
# 
# B11focal   <- focal(rstack$B11, w5, fun = mean,   na.policy = "omit")
# B12focal   <- focal(rstack$B12, w5, fun = mean,   na.policy = "omit")
# B2focal   <- focal(rstack$B2, w9, fun = mean,   na.policy = "omit")
# B3focal   <- focal(rstack$B3, w5, fun = mean,   na.policy = "omit")
# B4focal   <- focal(rstack$B4, w7, fun = mean,   na.policy = "omit")
# B5focal   <- focal(rstack$B5, w9, fun = mean,   na.policy = "omit")
# B6focal   <- focal(rstack$B6, w7, fun = mean,   na.policy = "omit")
# B8focal   <- focal(rstack$B8, w5, fun = mean,   na.policy = "omit")
# NDVIfocal   <- focal(rstack$NDVI, w5, fun = mean,   na.policy = "omit")
# NDVISDfocal   <- focal(rstack$NDVI_SD, w7, fun = mean,   na.policy = "omit")
# B4focalSD   <- focal(rstack$B4, w9, fun = sd,   na.policy = "omit")
# B8focalSD   <- focal(rstack$B8, w7, fun = sd,   na.policy = "omit")
# 
# NDVIdryfocal   <- focal(rstack$NDVI_dry, w5, fun = mean,   na.policy = "omit")
# NDVIwetfocal   <- focal(rstack$NDVI_wet, w5, fun = mean,   na.policy = "omit")
# B5wetfocal   <- focal(rstack$B5_wet, w5, fun = mean,   na.policy = "omit")
# B5dryfocal   <- focal(rstack$B5_dry, w5, fun = mean,   na.policy = "omit")
# 
# 
# names(B11focal) <- c("B11focal")
# names(B12focal) <- c("B12focal")
# names(B2focal) <- c("B2focal")
# names(B3focal) <- c("B3focal")
# names(B4focal) <- c("B4focal")
# names(B5focal) <- c("B5focal")
# names(B6focal) <- c("B6focal")
# names(B8focal) <- c("B8focal")
# names(NDVIfocal) <- c("NDVIfocal")
# names(NDVISDfocal) <- c("NDVISDfocal")
# names(B4focalSD) <- c("B4focalSD")
# names(B8focalSD) <- c("B8focalSD")
# names(NDVIdryfocal) <- c("NDVIdryfocal")
# names(NDVIwetfocal) <- c("NDVIwetfocal")
# names(B5wetfocal) <- c("B5wetfocal")
# names(B5dryfocal) <- c("B5dryfocal")
# 
# 
# 
# 
# rstack <- c(rstack, B11focal, B12focal, B2focal, B3focal, B4focal,
#             B5focal, B6focal, B8focal, NDVIfocal, NDVISDfocal,
#             B4focalSD, B8focalSD, 
#             NDVIdryfocal, NDVIwetfocal,
#             B5dryfocal, B5wetfocal)
# names(rstack)
# 
# # remove from environment unwanted standalone rasters
# rm(B11focal, B12focal, B2focal, B3focal, B4focal,
#    B5focal, B6focal, B8focal, NDVIfocal, NDVISDfocal,
#    B4focalSD, B8focalSD, B5dryfocal, B5wetfocal,NDVIdryfocal, NDVIwetfocal)
# gc()
# 
# # save the stack
# rstack_file <- file.path(stack_folder, "rstack_Aggx2_2024.tif")
# writeRaster(
#   rstack,
#   rstack_file,
#   overwrite = TRUE,
#   wopt = list(
#     datatype = "FLT4S",
#     gdal = c("COMPRESS=DEFLATE", "TILED=YES",
#              "BLOCKXSIZE=512", "BLOCKYSIZE=512", "ZLEVEL=9")
#   )
# )


#### Load precomputed stack ####
myrstack_file <- list.files(stack_folder, pattern = "*.tif$", full.names = TRUE)
myrstack_file
specificfile <- myrstack_file[grepl("2024", myrstack_file)]
rstack <- rast(specificfile)
names(rstack)


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


# Additional Large Area polygons (LAP)
classesLAP <- c("bananaLAP", "coffeeLAP", "groundcropsLAP", "grassLAP", "bushLAP",
                "treesLAP", "forestLAP","buildingLAP","roadLL","waterLAP")
train_polys_listLAP <- list()
for (i in seq_along(classesLAP)) {
  shp <- st_read(paste0(poly_folder, classesLAP[i], ".shp"))
  shp$class <- class_codes[i]
  shp <- shp %>% dplyr::select(Name, class)
  train_polys_listLAP[[i]] <- shp
}
train_polysLAP <- do.call(rbind, train_polys_listLAP)
# i had to put roadLL as a placeholder, but dont want it duplicated so removing those ones now
train_polysLAP <- train_polysLAP %>%
  filter(class != 8)




# Combine polygons
train_polys_merge <- rbind(train_polys, train_polysLL,train_polysLAP)
train_polys_merge$ID <- 1:nrow(train_polys_merge)
cat("Step 3: Loaded training polygons\n")



#### Extract raster values under polygons ####
training_data <- extract(rstack, train_polys_merge, df = TRUE)
training_datamerge <- training_data %>%
  left_join(train_polys_merge, by = c("ID")) %>%
  na.omit()

# rename classes 0:9 to class0:class9
training_datamerge <- training_datamerge %>%
  mutate(class2 = paste0("class",class)) %>%
  dplyr::select(-c(class)) %>%
  rename(class=class2) %>%
  mutate(class = as.factor(class))
levels(training_datamerge$class)


# add long lat columns
cents <- st_centroid(training_datamerge$geometry)
coords <- st_coordinates(cents)
training_datamerge$long <- coords[,1]
training_datamerge$lat <- coords[,2]
training_datamerge <- training_datamerge %>%
  dplyr::select(-c(geometry))

cat("Step 4: Extracted raster values under polygons\n")


#### Define folds and test/train ####
#' going to do spatial blocking so i dont have so much spatial auto-
#' correlation that makes the model overfi

# get centroids for each train set pixel

centsf <- st_centroid(train_polys_merge)
cent <- data.frame(st_coordinates(centsf)) 
k <- 5
set.seed(123)
km <- kmeans(scale(cent), centers = k)
centsf$fold <- km$cluster

# ggplot(centsf) +
#   geom_sf(aes(color = as.factor(fold)), size = .6, alpha = 0.8) +
#   scale_color_brewer(palette = "Set1", name = "Class") +
#   theme_minimal() +
#   theme(legend.position = "right")

centsf <- centsf %>%
  dplyr::select(ID,fold) %>%
  st_drop_geometry()

training_datamerge <- training_datamerge %>%
  left_join(centsf)


# Sample down ultra dominant classes (ground crops, bush)
class4 <- training_datamerge %>% filter(class == "class4") %>% sample_frac(0.7)
class6 <- training_datamerge %>% filter(class == "class6") %>% sample_frac(0.85)
others <- training_datamerge %>% filter(!class %in% c("class4","class6"))
table(others$class)
training_datamerge2 <- bind_rows(class4,class6, others)
table(training_datamerge2$class)
prop.table(table(training_datamerge2$class))
cat("Step 5: Resampled dominant classes\n")



# assess correlation 
colnames(training_datamerge2)
cordf <- training_datamerge2 %>%
  dplyr::select(B11,B12,B2,B3,B4,B5,B6,B7,B8,B8A, 
                NDVI_SD,
                B11focal, B12focal, B2focal, B3focal,
                B4focal, B5focal, B6focal, B8focal,
                NDVIfocal, NDVISDfocal,
                B4focalSD, B8focalSD,
                NDVIdryfocal, NDVIwetfocal,
                B5dryfocal, B5wetfocal)
cordfout <- data.frame(cor(cordf))
write.csv(cordfout, file.path(output_folder, "cordfout.csv"))


#### Split into train/test ####
#' going to assign to train/test by polygon ID
set.seed(123)

# 1. get polygon ids with their corresponding class
poly_info <- training_datamerge2 %>%
  distinct(ID, class)

# 2. for each class, randomly assign 70% of polygons to training
train_polys <- poly_info %>%
  group_by(class) %>%
  sample_frac(0.7) %>%
  ungroup()

# 3. split dataset based on polygon membership
train_set <- training_datamerge2 %>% filter(ID %in% train_polys$ID)
test_set  <- training_datamerge2 %>% filter(!ID %in% train_polys$ID)

# 4. double check that none of the IDs in training are in testing
train_ids <- unique(train_set$ID)
test_ids  <- unique(test_set$ID)
intersect_ids <- intersect(train_ids, test_ids)
length(intersect_ids) 

cat("Step 6: Split into train/test sets\n")



#### Train XGB ####
# define index for cross validation folds
k <- length(unique(train_set$fold))
index <- lapply(1:k, function(i) which(train_set$fold != i))
indexOut <- lapply(1:k, function(i) which(train_set$fold == i))


# fit model

# Parallel setup
n_cores <- 48
cl <- makeCluster(n_cores)
registerDoParallel(cl)


X <- train_set %>% dplyr::select(B11,B12,B2,B3,B4,B5,B6,B7,B8,B8A, 
                                 NDVI_SD,
                                 B11focal, B12focal, B2focal, B3focal,
                                 B4focal, B5focal, B6focal, B8focal,
                                 NDVIfocal, NDVISDfocal,
                                 B4focalSD, B8focalSD,
                                 NDVIdryfocal, NDVIwetfocal,
                                 B5dryfocal, B5wetfocal)
Y <- as.factor(train_set$class)  # caret handles factor outcomes for multiclass
str(X)
str(Y)
table(Y)
prop.table(table(Y))
levels(Y)

# Custom summary function for F1 score of collapsed crop class
customF1collapsed <- function(data, lev = NULL, model = NULL) {
  # Ensure data is in the correct format
  obs <- factor(data$obs, levels = lev)
  pred <- factor(data$pred, levels = lev)
  
  # Define crop classes
  crop_classes <- c("class0", "class1", "class2")
  
  # Collapse crop classes: map banana, coffee, groundcrops to "crop"
  obs_collapsed <- ifelse(obs %in% crop_classes, "crop", "non_crop")
  pred_collapsed <- ifelse(pred %in% crop_classes, "crop", "non_crop")
  
  # Convert to factors with levels "crop" and "non_crop"
  obs_collapsed <- factor(obs_collapsed, levels = c("crop", "non_crop"))
  pred_collapsed <- factor(pred_collapsed, levels = c("crop", "non_crop"))
  
  # Compute confusion matrix for collapsed classes
  cm <- table(pred = pred_collapsed, obs = obs_collapsed)
  
  # Calculate F1 for the "crop" class only
  cls <- "crop"
  
  # True positives (TP), false positives (FP), false negatives (FN)
  TP <- ifelse(!is.na(cm[cls, cls]), cm[cls, cls], 0)
  FP <- ifelse(sum(cm[cls, ]) > 0, sum(cm[cls, ]) - TP, 0)
  FN <- ifelse(sum(cm[, cls]) > 0, sum(cm[, cls]) - TP, 0)
  
  # Precision = TP / (TP + FP), handle division by zero
  precision <- ifelse(TP + FP > 0, TP / (TP + FP), 0)
  
  # Recall = TP / (TP + FN), handle division by zero
  recall <- ifelse(TP + FN > 0, TP / (TP + FN), 0)
  
  # F1 = 2 * (precision * recall) / (precision + recall), handle division by zero
  f1 <- ifelse(precision + recall > 0, 
               2 * (precision * recall) / (precision + recall), 
               0)
  
  # upweight ppv: beta < 1 favors precision, beta > 1 favors recall
  beta <- 0.5 # doubles weight of ppv relative to sens
  f1_beta <- ifelse((beta^2 * precision + recall) > 0,
                    (1 + beta^2) * (precision * recall) / (beta^2 * precision + recall),
                    0)
  
  # Return named vector for caret
  out <- c(CropF1 = f1,
           CropF1_weight = f1_beta,
           CropPPV = precision,
           CropSens = recall)
  out
}


# TrainControl with 5-fold CV and custom summary function
train_ctrl <- trainControl(
  method = "cv",
  index = index,  # your custom defined folds
  indexOut = indexOut,
  verboseIter = TRUE,
  allowParallel = TRUE,
  summaryFunction = customF1collapsed,  # Use custom F1 metric
  classProbs = TRUE  # Required for multi:softprob, though not used in metric
)


# XGBoost tuning grid
xgb_grid <- expand.grid(
  nrounds = c(800),        # number of boosting rounds
  max_depth = c(4),
  eta = c(.08),          # learning rate
  gamma = c(4),
  colsample_bytree = c(.6),
  min_child_weight = c(3),
  subsample = c(.5)
)

# Train XGBoost model
xgb_model <- train(
  x = X,
  y = Y,
  method = "xgbTree",
  trControl = train_ctrl,
  tuneGrid = xgb_grid,
  objective = "multi:softprob",   # multiclass
  num_class = length(levels(Y)),
  metric = "CropF1",  # Use custom metric for optimization
  maximize = TRUE      # Explicitly maximize the F1 score
)


stopCluster(cl)
registerDoSEQ()


# print(xgb_model$bestTune)  # Best hyperparameters
# print(xgb_model$results)   # CropF1 scores for each hyperparameter combination

pred <- predict(xgb_model, newdata = train_set)
table(pred, train_set$class)

# save the model
saveRDS(xgb_model, file.path(output_folder, "xgb_model.rds"))


# var importance
xgb_varimp <- varImp(xgb_model, scale = TRUE)  # scale = TRUE scales importance 0-100
print(xgb_varimp)
saveRDS(xgb_varimp, file.path(output_folder, "varImportance.rds"))

cat("Step 7: Trained XGBoost multiclass model\n")

# Predict on training and test set
train_set$pred_classxgb <- predict(xgb_model, newdata = X)
test_set$pred_classxgb <- predict(xgb_model, newdata = test_set[, setdiff(names(test_set), "class")])


##### Confusion matrix function ####
calc_cmxgb <- function(df, class_map) {
  
  # --- Uncollapsed (original classes) ---
  cm_long_raw <- df %>%
    group_by(Observed = class, Predicted = pred_classxgb) %>%
    summarise(Freq = n(), .groups = "drop")
  
  cm_wide_raw <- cm_long_raw %>%
    pivot_wider(names_from = Predicted, values_from = Freq, values_fill = 0)
  
  expanded_obs_raw <- rep(cm_long_raw$Observed, cm_long_raw$Freq)
  expanded_pred_raw <- rep(cm_long_raw$Predicted, cm_long_raw$Freq)
  
  cm_final_raw <- confusionMatrix(
    factor(expanded_pred_raw, levels = sort(unique(c(expanded_obs_raw, expanded_pred_raw)))),
    factor(expanded_obs_raw, levels = sort(unique(c(expanded_obs_raw, expanded_pred_raw))))
  )
  
  # --- Collapsed (mapped classes) ---
  cm_long <- df %>%
    mutate(Observed_new = class_map[as.character(class)],
           Predicted_new = class_map[as.character(pred_classxgb)]) %>%
    group_by(Observed_new, Predicted_new) %>%
    summarise(Freq = n(), .groups = "drop")
  
  cm_wide <- cm_long %>%
    pivot_wider(names_from = Predicted_new, values_from = Freq, values_fill = 0)
  
  expanded_obs <- rep(cm_long$Observed_new, cm_long$Freq)
  expanded_pred <- rep(cm_long$Predicted_new, cm_long$Freq)
  
  cm_final <- confusionMatrix(
    factor(expanded_pred, levels = sort(unique(c(expanded_obs, expanded_pred)))),
    factor(expanded_obs, levels = sort(unique(c(expanded_obs, expanded_pred))))
  )
  
  list(
    raw = list(long = cm_long_raw, wide = cm_wide_raw, cm = cm_final_raw),
    collapsed = list(long = cm_long, wide = cm_wide, cm = cm_final)
  )
}


class_map <- c(
  "class0" = "Crops", 
  "class1" = "Crops", 
  "class2" = "Crops",
  "class3" = "GrassBush", 
  "class4" = "GrassBush",
  "class5" = "Trees", 
  "class6" = "Trees",
  "class7" = "Urban", 
  "class8" = "Urban",
  "class9" = "Water"
)
cm_trainxgb <- calc_cmxgb(train_set, class_map)
cm_testxgb  <- calc_cmxgb(test_set, class_map)

cm_testxgb$collapsed$cm
cm_testxgb$raw$cm


write.csv(train_set, file.path(output_folder, "train_set.csv"))
write.csv(test_set, file.path(output_folder, "test_set.csv"))
saveRDS(cm_testxgb$raw$cm, file.path(output_folder, "cm_raw_testconfusionmatrix.rds"))
saveRDS(cm_testxgb$collapsed$cm, file.path(output_folder, "cm_collapsed_testconfusionmatrix.rds"))

saveRDS(cm_trainxgb$raw$cm, file.path(output_folder, "cm_raw_trainconfusionmatrix.rds"))
saveRDS(cm_trainxgb$collapsed$cm, file.path(output_folder, "cm_collapsed_trainconfusionmatrix.rds"))
cat("Step 10: Saved confusion matrices\n")




 
#### Classify entire raster stack ####
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

# Generate tile extents dynamically
tile_extents <- list()
k <- 1
for (i in 1:nrows) {
  for (j in 1:ncols) {
    tile_extents[[k]] <- ext(xmins[j], xmaxs[j], ymins[i], ymaxs[i])
    k <- k + 1
  }
}

# Loop through extents to crop and save tiles
for (k in seq_along(tile_extents)) {
  tile <- crop(rstack, tile_extents[[k]])
  out_file <- file.path(output_folder, paste0("rstack_tile", k, ".tif"))

  writeRaster(tile, out_file, overwrite = TRUE)

  # Clear memory
  rm(tile)
  gc()
}

# Remove only specific objects
rm(training_datamerge,
   training_data,
   train_polys_merge,
   train_polysLL,
   train_set,
   test_set,
   cm_train,
   cm_test,
   others,
   train_ctrl,
   train_polys_list,
   train_polys_listLL,
   training_datamerge2,
   rstack)

# Free memory
gc()


##### Predict tiles #####
# direct to the stack tiles that are saved
tile_files <- list.files(output_folder, pattern = "^rstack_tile.*\\.tif$", full.names = TRUE)

# read in each tile invidiaully, predict, then save prediction tile
for (tile_path in tile_files) {
  # Load the tile
  rstack_tile <- rast(tile_path)

  # Message to tell what tile I am on
  cat("Classifying:", tile_path, "\n")
  flush.console()

  # Build output filename by replacing "rstack_tile" with "classification_2024_tile"
  out_path <- gsub("rstack_tile", "classification_2024_tile", tile_path)

  # Predict on the tile
  classified_tile <- predict(
    rstack_tile,
    xgb_model,
    na.rm = TRUE,
    filename = out_path,
    overwrite = TRUE,
    wopt = list(
      datatype = "INT1U",
      gdal = c("COMPRESS=DEFLATE", "TILED=YES",
               "BLOCKXSIZE=512", "BLOCKYSIZE=512", "ZLEVEL=9")
    )
  )

  # Clear memory
  rm(classified_tile, rstack_tile)
  gc()
}


##### Mosaic classified tiles #####
library(terra)

# List classified tile rasters
classified_files <- list.files(
  output_folder,
  pattern = "^classification_2024_tile.*\\.tif$",
  full.names = TRUE
)

# Load tiles into a SpatRaster collection
classified_tiles <- lapply(classified_files, rast)

# Mosaic them (terra::mosaic handles overlaps automatically)
classified_mosaic <- do.call(mosaic, c(classified_tiles, fun = "first"))
plot(classified_mosaic)

# Save the final mosaic
final_file <- file.path(output_folder, "classification_2024_mosaic.tif")
writeRaster(
  classified_mosaic,
  final_file,
  overwrite = TRUE,
  wopt = list(
    datatype = "INT1U",
    gdal = c("COMPRESS=DEFLATE", "TILED=YES",
             "BLOCKXSIZE=512", "BLOCKYSIZE=512", "ZLEVEL=9")
  )
)

# Free memory
rm(classified_tiles, classified_mosaic)
gc()



