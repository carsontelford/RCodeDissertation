#### Top ####


trainclass <- readRDS("C:/Users/carso/Downloads/cm_collapsed_trainconfusionmatrix.rds")
testclass <- readRDS("C:/Users/carso/Downloads/cm_collapsed_testconfusionmatrix.rds")
trainclass
testclass

trainraw <- readRDS("C:/Users/carso/Downloads/cm_raw_trainconfusionmatrix.rds")
testraw <- readRDS("C:/Users/carso/Downloads/cm_raw_testconfusionmatrix.rds")
trainraw
testraw

varimp <- readRDS("C:/Users/carso/Downloads/varImportance.rds")
varimp



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
output_folder <- "Aim 1/ClassificationOutput/"
raster_folder <- "Aim 1/GEE_RawImageryBands/2024/"
poly_folder <- "Aim 1/Shapefiles/Training Polygons/Training2022_2024/"
stack_folder <- "Aim 1/GEE_RawImageryBands/FinalStacks/"

dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(raster_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(poly_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(stack_folder, recursive = TRUE, showWarnings = FALSE)


#### Terra options (fast scratch) ####
terraOptions(
  progress = 1,
  tempdir = Sys.getenv("TMPDIR"),  # fast, job-specific scratch
  memfrac = 0.5,
  threads = 2
)
cat("Step 1: Terra tempdir set to", Sys.getenv("TMPDIR"), "\n")

#### Load raster bands ####
band_files <- list.files(raster_folder, pattern = "*.tif$", full.names = TRUE)
rstack <- rast(band_files)
names(rstack)
rstack <- rstack[[1:8]]
rstack <- aggregate(rstack, fact=10)
cat("Step 2: Loaded raster stack with", nlyr(rstack), "bands\n")

# add focal SD layers
w3 <- matrix(1,3,3)  # 3x3 window
w5 <- matrix(1,5,5)  # 3x3 window

names()
redfocalSD   <- focal(rstack$B4, w5, fun = sd,   na.policy = "omit")
nirfocalSD   <- focal(rstack$B6, w5, fun = sd,   na.policy = "omit")
B11focal5   <- focal(rstack$B11, w5, fun = mean,   na.policy = "omit")
B5focal5   <- focal(rstack$B5, w5, fun = mean,   na.policy = "omit")
NDVIfocal3   <- focal(rstack$NDVI, w3, fun = mean,   na.policy = "omit")
redfocal5   <- focal(rstack$B4, w5, fun = mean,   na.policy = "omit")
NDVISDfocal3   <- focal(rstack$NDVI_SD, w5, fun = mean,   na.policy = "omit")

names(redfocalSD) <- c("redfocalSD")
names(nirfocalSD) <- c("nirfocalSD")
names(B11focal5) <- c("B11focal5")
names(B5focal5) <- c("B5focal5")
names(NDVIfocal3) <- c("NDVIfocal3")
names(redfocal5) <- c("redfocal5")
names(NDVISDfocal3) <- c("NDVISDfocal3")

rstack <- c(rstack,redfocalSD, nirfocalSD, B11focal5, B5focal5,
            NDVIfocal3, redfocal5, NDVISDfocal3)
names(rstack)

# remove from environment unwanted standalone rasters
rm(redfocalSD,nirfocalSD,B11focal5,B5focal5,NDVIfocal3,redfocal5,NDVISDfocal3)
gc()

# save the stack
rstack_file <- file.path(stack_folder, "rstack_Aggx2.tif")
writeRaster(
  rstack,
  rstack_file,
  overwrite = TRUE,
  wopt = list(
    datatype = "FLT4S",
    gdal = c("COMPRESS=DEFLATE", "TILED=YES",
             "BLOCKXSIZE=512", "BLOCKYSIZE=512", "ZLEVEL=9")
  )
)


#### Load precomputed stack ####
myrstack_file <- list.files(stack_folder, pattern = "*.tif$", full.names = TRUE)
myrstack_file
rstack <- rast(myrstack_file)
names(rstack)
# # drop layers i dont want to use due to correlation and non-importance in prediction
# rstack <- rstack[[!names(rstack) %in% c("B4","B7","B8","nirfocal","NDVI")]]


# # see correlation between bands
# set.seed(123)
# sample_vals <- spatSample(rstack, size = 1000, method = "random", na.rm = TRUE)
# cor_matrix <- cor(sample_vals)
# write.csv(cor_matrix, file.path(output_folder, "cormatrix_covars.csv"))


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
  # dplyr::select(-c(geometry)) %>%
  na.omit()

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

ggplot(centsf) +
  geom_sf(aes(color = as.factor(fold)), size = .6, alpha = 0.8) +
  scale_color_brewer(palette = "Set1", name = "Class") +
  theme_minimal() +
  theme(legend.position = "right")

centsf <- centsf %>%
  dplyr::select(ID,fold) %>%
  st_drop_geometry()

training_datamerge <- training_datamerge %>%
  left_join(centsf)


# Sample down dominant class (bush and grass)
class3 <- training_datamerge %>% filter(class == 3) %>% sample_frac(.7)
class4 <- training_datamerge %>% filter(class == 4) %>% sample_frac(.65)
others <- training_datamerge %>% filter(!class %in% c(3,4))
prop.table(table(others$class))
training_datamerge2 <- bind_rows(class3, class4, others)
prop.table(table(training_datamerge2$class))

cat("Step 5: Resampled dominant classes\n")

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
n_cores <- 2
cl <- makeCluster(n_cores)
registerDoParallel(cl)


X <- train_set %>% dplyr::select(B11,B12,B2,B3,B4,B5,B6,B7)
Y <- as.factor(train_set$class)  # caret handles factor outcomes for multiclass

# TrainControl with 5-fold CV
train_ctrl <- trainControl(
  method = "cv",
  index = index, # my custom defined folds
  indexOut = indexOut,
  verboseIter = TRUE,
  allowParallel = TRUE
)

# XGBoost tuning grid
xgb_grid <- expand.grid(
  nrounds = c(100),        # number of boosting rounds
  max_depth = c(4),
  eta = c(.06),           # learning rate
  gamma = 0,
  colsample_bytree = c(.6),
  min_child_weight = 1,
  subsample = c(.6)
)

# Train XGBoost model
xgb_model <- train(
  x = X,
  y = Y,
  method = "xgbTree",
  trControl = train_ctrl,
  tuneGrid = xgb_grid,
  objective = "multi:softprob",   # multiclass
  num_class = length(levels(Y))
)

stopCluster(cl)
registerDoSEQ()

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
  "0" = "Crops", "1" = "Crops", "2" = "Crops",
  "3" = "GrassBush", "4" = "GrassBush",
  "5" = "Trees", "6" = "Trees",
  "7" = "Urban", "8" = "Urban",
  "9" = "Water"
)

cm_trainxgb <- calc_cmxgb(train_set, class_map)
cm_testxgb  <- calc_cmxgb(test_set, class_map)

cm_trainxgb$collapsed$cm
cm_trainxgb$raw$cm

cm_testxgb$collapsed$cm
cm_testxgb$raw$cm



saveRDS(cm_testxgb$raw$cm, file.path(output_folder, "cm_raw_testconfusionmatrix.rds"))
saveRDS(cm_testxgb$collapsed$cm, file.path(output_folder, "cm_collapsed_testconfusionmatrix.rds"))

saveRDS(cm_trainxgb$raw$cm, file.path(output_folder, "cm_raw_trainconfusionmatrix.rds"))
saveRDS(cm_trainxgb$collapsed$cm, file.path(output_folder, "cm_collapsed_trainconfusionmatrix.rds"))
cat("Step 10: Saved confusion matrices\n")





#### Classify entire raster stack ####
##### divide into tiles #####
library(terra)
library(parallel)
nrows <- 2
ncols <- 2
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
tile_extents

# Loop through extents to crop and save tiles
for (k in seq_along(tile_extents)) {
  tile <- crop(rstack, tile_extents[[k]])
  out_file <- file.path(output_folder, paste0("rstack_tile", k, ".tif"))
  
  writeRaster(tile, out_file, overwrite = TRUE)
  
  # Clear memory
  rm(tile)
  gc()
}

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
#    rstack)
# 
# # Free memory
# gc()


##### Predict tiles #####
# direct to the stack tiles that are saved
tile_files <- list.files(output_folder, pattern = "^rstack_tile.*\\.tif$", full.names = TRUE)
tile_files

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
classified_files

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


