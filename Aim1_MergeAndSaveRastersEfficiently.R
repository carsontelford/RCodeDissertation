#### Top ####
#' purpose of this is to read in the GEE rasters exports 
#' that were divided into pieces due to size. Each band is saved 
#' individually. I will merge the full images, then save again so I can
#' upload to longleaf. Then in longleaf I will stack them all and 
#' do the classification.

library(terra)
library(tidyverse)


#### All bands ####

# function to merge and write rasters for a given band + year
process_band <- function(band, year, outdir = "Aim 1/GEE_RawImageryBands/2016") {
  indir <- outdir   # remove year subfolder
  
  # build regex pattern
  pattern <- paste0("^", band, "_", year, ".*\\.tif$")
  
  files <- list.files(indir, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) {
    warning("No files found for ", band, "_", year)
    return(NULL)
  }
  
  rasters <- lapply(files, rast)
  mr <- do.call(merge, rasters)
  
  outfile <- file.path(indir, paste0("final_", band, "_float32_compressed.tif"))
  
  writeRaster(mr, outfile,
              overwrite = TRUE,
              datatype = "FLT4S",
              filetype = "GTiff",
              gdal = c("COMPRESS=ZSTD", "PREDICTOR=3", "LEVEL=9"))
  
  message("✅ Saved: ", outfile)
  return(outfile)
}

# ---- run for multiple bands/years ----
bands_years <- list(
  c("B2",   "2016"),
  c("B3",   "2016"),
  c("B4",   "2016"),
  c("B5",   "2016"),
  c("B6",   "2016"),
  c("B7",   "2016"),
  c("B8",   "2016"),
  c("B8A",  "2016"),
  c("B11",  "2016"),
  c("B12",  "2016"),
  c("NDVI",  "2016"),
  c("NDVI_SD",  "2016"),
  c("NDVI_dry",  "2016"),
  c("NDVI_wet",  "2016"),
  c("B5_dry", "2016"),
  c("B5_wet", "2016")
)

list.files("Aim 1/GEE_RawImageryBands/2016", pattern = "\\.tif$")

# loop through and process
lapply(bands_years, function(x) process_band(x[1], x[2]))


t <- rast("Aim 1/GEE_RawImageryBands/2018/final_B11_float32_compressed.tif")
plot(t)








