
#### Top ####
library(tidyverse)
library(terra)
library(sf)
library(ranger)
library(caret)
library(raster)

#### Output folder (persistent, original location) ####
output_folder <- "Aim 1/ClassificationOutput/"
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# #### Terra options (fast scratch) ####
# terraOptions(
#   progress = 1,
#   tempdir = Sys.getenv("TMPDIR"),  # fast, job-specific scratch
#   memfrac = 0.7,
#   threads = 24
# )
cat("Step 1: Terra tempdir set to", Sys.getenv("TMPDIR"), "\n")

#### Load raster bands ####
band_files <- list.files("Aim 1/GEE_RawImageryBands/2024/", pattern = "*.tif$", full.names = TRUE)
rstack <- rast(band_files)
rstack <- rstack[[1:3]]
rstack <- aggregate(rstack,fact=4, fun='mean')
cat("Step 2: Loaded raster stack with", nlyr(rstack), "bands\n")
plot(rstack)

#### Load training polygons ####
classes <- c("banana", "coffee", "groundcrops", "grass", "bush",
             "trees", "forest", "building", "road", "water")
class_codes <- 0:9

train_polys_list <- list()
for (i in seq_along(classes)) {
  shp <- st_read(paste0("Aim 1/Shapefiles/Training Polygons/Training2022_2024_LONGLEAF/", classes[i], ".shp"))
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
  shp <- st_read(paste0("Aim 1/Shapefiles/Training Polygons/Training2022_2024_LONGLEAF/", classesLL[i], ".shp"))
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
train_ctrl <- trainControl(
  method = "cv",       # cross-validation
  number = 3,          # number of folds
  verboseIter = TRUE
)

# Train with caret
rf_model <- train(
  as.factor(class) ~ ., 
  data = train_set,
  method = "ranger",   # caret’s wrapper for ranger
  trControl = train_ctrl,
  tuneLength = 5,      # number of hyperparameter combos to try
  num.trees = 500,     # passes to ranger
  importance = "impurity"
)
print(rf_model)

predrast <- predict(rstack,rf_model, na.rm=T)
plot(predrast)



# try with cores
library(caret)
library(ranger)
library(terra)
library(doParallel)
# Detect number of cores (or set manually)
n_cores <- 4
cl <- makeCluster(n_cores)
registerDoParallel(cl)
# 
train_ctrl <- trainControl(
  method = "cv",
  number = 3,
  verboseIter = TRUE,
  allowParallel = TRUE  # important for caret to use doParallel
)

rf_model <- train(
  as.factor(class) ~ ., 
  data = train_set,
  method = "ranger",
  trControl = train_ctrl,
  tuneLength = 5,
  num.trees = 500,
  importance = "impurity",
  num.threads = n_cores   # allows ranger to use multiple cores per model
)

print(rf_model)

# write directly to disk (recommended for large rasters)
predrast <- predict(
  rstack, rf_model,
  na.rm = TRUE,
  filename = "Aim 1/ClassificationOutput/ClassifiedImages/rf_prediction.tif",
  overwrite = TRUE
)

plot(predrast)



table(rf_model$predictions)
rf_model$variable.importance
cat("Step 7: Trained random forest\n")

# Save variable importance
var_imp <- rf_model$variable.importance
var_imp
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
cm_test
cm_train
write.csv(cm_train$wide, file.path(output_folder, "cm_train_confusionmatrix.csv"))
write.csv(cm_test$wide, file.path(output_folder, "cm_test_confusionmatrix.csv"))
cat("Step 10: Saved confusion matrices\n")

#### Classify entire raster stack ####
rf_model$variable.importance
names(rstack)
ncell(rstack)

rstackdf <- as.data.frame(rstack, xy=T)
mypred <- predict(rf_model, rstackdf)$predictions

# Copy raster structure
pred_raster <- rstack[[1]]  # take first layer as template
values(pred_raster) <- mypred
plot(pred_raster)



library(terra)
cat("Step 1: Masking NA values in raster stack...\n")
plot(rstack)
rstack <- mask(rstack, mask = rstack$B11, maskvalue = NA, updatevalue = NA)
cat("Step 1.1: Verifying NA values after masking...\n")
print(global(rstack, fun = "isNA"))

library(doParallel)
library(foreach)
cat("Step 2: Setting up parallel processing...\n")
cl <- makeCluster(detectCores() - 14)  # Use all cores minus one
registerDoParallel(cl)

predict_ranger <- function(model, data, ...) {
  data_df <- as.data.frame(data)
  colnames(data_df) <- c("B11")  # Ensure correct column names
  pred <- predict(model, data = data_df, type = "response")$predictions
  as.numeric(as.character(pred))  # Convert factor to numeric (0:9)
}
cat("Step 3: Defined custom prediction function for ranger\n")




# Get block sizes
block_info <- blocks(rstack)
nblocks <- 8
cat("Step 4: Dividing raster into", nblocks, "blocks\n")

# Initialize an empty SpatRaster for predictions
pred_raster <- rast(rstack, nlyrs = 1)
values(pred_raster) <- NA
cat("Step 4.1: Initialized prediction raster\n")

# Process blocks in parallel
results <- foreach(i = 1:nblocks, .packages = c("terra", "ranger")) %dopar% {
  # Read block data
  block_data <- readValues(rstack, row = block_info$row[i], nrows = block_info$nrows[i], col = 1, ncols = ncol(rstack))
  block_df <- as.data.frame(block_data)
  colnames(block_df) <- c("B11", "B12", "B2")
  
  # Identify non-NA rows
  na_mask <- complete.cases(block_df)
  if (sum(na_mask) == 0) return(NULL)  # Skip empty blocks
  
  # Predict on non-NA data
  block_df_valid <- block_df[na_mask, ]
  pred <- predict_ranger(rf_model, block_df_valid)
  
  # Create output vector with NAs
  out <- rep(NA, nrow(block_df))
  out[na_mask] <- pred
  
  # Return predictions with block info
  list(row = block_info$row[i], nrows = block_info$nrows[i], predictions = out)
}

# Write predictions to raster
cat("Step 4.2: Combining predictions into raster...\n")
for (res in results) {
  if (!is.null(res)) {
    writeValues(pred_raster, res$predictions, start = res$row, nrows = res$nrows)
  }
}






