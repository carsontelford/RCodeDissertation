
library(tidyverse)
library(terra)
library(sf)


shp <- st_read("C:/Users/carso/OneDrive - University of North Carolina at Chapel Hill/Dissertation/Shapefiles/SouthwesternDistrict_StudyArea/sw_dissolvedbuffered_RVFregion.shp")
plot(shp$geometry)
st_crs(shp)

# Reproject the shapefile to UTM Zone 36S (EPSG:32736)
shp_utm <- st_transform(shp, crs = 32736)
st_crs(shp_utm)
plot(shp_utm$geometry)


# Define the desired grid size (e.g., 5x5 km)
grid_size <- 5000 # in meters

# Create a grid over the shapefile's bounding box
bounding_box <- st_bbox(shp_utm)
grid <- st_make_grid(
  shp_utm,
  cellsize = c(grid_size, grid_size), # Grid cell size
  square = TRUE                       # Ensure square grid cells
)

# Clip the grid to the shapefile boundary
grid_clipped <- st_intersection(st_as_sf(grid), shp_utm)

# Plot the shapefile and grid
plot(st_geometry(shp_utm), col = 'lightblue', border = 'blue')
plot(st_geometry(grid_clipped), add = TRUE, border = 'red')






