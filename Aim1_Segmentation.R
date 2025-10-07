

#### Top ####

# Use your personal library on Longleaf
.libPaths("/nas/longleaf/home/ctelford/R/x86_64-pc-linux-gnu-library/4.4")

# Load libraries
library(terra)
library(sf)

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

#### Load raster bands ####
band_files <- list.files(raster_folder, pattern = "*.tif$", full.names = TRUE)
rstack <- rast(band_files)
rstack <- rstack[[1:3]]
rstack <- aggregate(rstack, fact=2)
cat("Step 2: Loaded raster stack with", nlyr(rstack), "bands\n")



# 1. Load your NDVI raster
ndvi <- rast("C:/Users/carso/OneDrive - University of North Carolina at Chapel Hill/Dissertation/Aim 1/GEE_RawImageryBands/2024/final_NDVI_wet_float32_compressed.tif")
ndvi <- aggregate(ndvi,fact=5,fun="mean")

# 2. Optional: smooth raster to reduce noise
ndvi_smooth <- focal(ndvi, w=matrix(1,3,3), fun=mean, na.policy="omit")

plot(ndvi)
plot(ndvi_smooth)

# 3. Edge detection using Sobel filters
# Horizontal and vertical Sobel kernels
sobel_x <- matrix(c(-1,0,1,-2,0,2,-1,0,1), nrow=3, byrow=TRUE)
sobel_y <- matrix(c(-1,-2,-1,0,0,0,1,2,1), nrow=3, byrow=TRUE)

# Apply focal filter
edge_x <- focal(ndvi_smooth, w=sobel_x, fun=sum, na.policy="omit", pad=TRUE)
edge_y <- focal(ndvi_smooth, w=sobel_y, fun=sum, na.policy="omit", pad=TRUE)

# Combine horizontal and vertical edges
edges <- sqrt(edge_x^2 + edge_y^2)
plot(edges)

writeRaster(edges,"C:/Users/carso/Downloads/test.tif",
            overwrite=T)

# 4. Convert to binary edge map (adjust threshold as needed)
threshold <- 0.2  # try different values
edge_binary <- edges > threshold

# 5. Convert raster edges to polygons
# First, label connected edge regions
edge_poly <- as.polygons(edge_binary, dissolve=F)

# 6. Convert to sf object for saving
edge_sf <- st_as_sf(edge_poly)

# 7. Optional: filter very small polygons
# edge_sf <- edge_sf[st_area(edge_sf) > 10, ]  # keep polygons > 10 sq units

# 8. Save as shapefile
st_write(edge_sf, "C:/Users/carso/Downloads/ndvi_edges.shp")
