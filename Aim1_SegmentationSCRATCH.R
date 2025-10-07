
#### Top ####


# Load libraries
library(terra)
library(sf)

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
  # tempdir = Sys.getenv("TMPDIR"),  # fast, job-specific scratch
  memfrac = 0.5,
  threads = 4
)
cat("Step 1: Terra tempdir set to", Sys.getenv("TMPDIR"), "\n")


#### Load raster bands ####
myrstack_file <- list.files(stack_folder, pattern = "*.tif$", full.names = TRUE)
myrstack_file
rstack <- rast(myrstack_file)
names(rstack)
rstack <- rstack[[1]]
names(rstack)

# rstack <- aggregate(rstack,fact=2, fun="mean")
# rstack <- aggregate(rstack,fact=2, fun="mean")
plot(rstack)

rstack_mask <- rstack
rstack_mask[is.na(rstack_mask)] <- 0  # replace NA with 0

r_smooth <- focal(rstack_mask, w=3, fun=mean, na.policy="omit", pad=TRUE)


# Sobel kernels
sobel_x <- matrix(c(
  -1, 0, 1,
  -2, 0, 2,
  -1, 0, 1
  ), nrow=3, byrow=TRUE)

sobel_y <- matrix(c(
  -1, -2, -1,
  0,  0,  0,
  1,  2,  1
), nrow=3, byrow=TRUE)

# Convolve raster with Sobel kernels
grad_x <- focal(r_smooth, w = sobel_x, fun = sum, na.policy = "omit", pad = TRUE)
grad_y <- focal(r_smooth, w = sobel_y, fun = sum, na.policy = "omit", pad = TRUE)

sobel_mag <- sqrt(grad_x^2 + grad_y^2)
plot(sobel_mag, main="Sobel Edge Magnitude")
summary(sobel_mag)

threshold <- quantile(values(sobel_mag), 0.8, na.rm = TRUE)
edges <- sobel_mag > threshold  # threshold depends on raster scale
plot(edges)

# Invert edges to get regions
regions_rast <- !edges  # TRUE = non-edge areas
plot(regions_rast)

# Convert to integer (required for clumps)
regions_int <- ifel(regions_rast, 1, 0)  # TRUE -> 1, FALSE -> 0

p <- patches(regions_int,direction=8)
plot(p)
freq(p)  # shows how many patches were found


