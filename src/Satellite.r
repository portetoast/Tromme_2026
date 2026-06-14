################################################################################
# SIMPLE MANUAL WORKFLOW:
# EXPLORE MODIS PRODUCTS, DOWNLOAD NDVI MANUALLY, READ IT IN R,
# EXTRACT VALUES AT POINT LOCATIONS, AND ADD THEM TO THE DATA TABLE
################################################################################

# ==============================================================================
# 1. Load required packages
# ==============================================================================

#install.packages('luna', repos='https://rspatial.r-universe.dev')
#install.packages("MODIStsp")
#install.packages("remotes")

library(remotes)
#install_github("ropensci/MODIStsp")
#install.packages("appeears")
library(luna)
library(MODIStsp)
library(appeears)
library(terra)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)

# ==============================================================================
# 2. Explore available MODIS products
# ==============================================================================
# List all products available through AppEEARS
products <- rs_products()

# Display the first rows
head(products)


getProducts("^MOD|^MYD|^MCD")

MODIStsp_get_prodlayers("M*D13Q1")

product <- "MOD09A1" #surface spectral reflectance of Terra
#product <- "MOD13Q1" # NDVI

productInfo(product)

# IMPORTANT:
# Look for the NDVI layer name in the printed list.
# This is the layer you will select manually in AppEEARS.

# ==============================================================================
# 4. Export the Switzerland polygon for manual upload in AppEEARS
# ==============================================================================
# This file can be uploaded directly in the AppEEARS web interface
# when creating an area request.

switzerland_sf <- ne_countries(
  scale = "medium",
  country = "Switzerland",
  returnclass = "sf"
)

dir.create(".data", showWarnings = FALSE)

st_write(
  switzerland_sf,
  ".data/switzerland.geojson",
  delete_dsn = TRUE
)

plot(st_geometry(switzerland_sf), col = "lightgray", main = "Switzerland")

# ------------------------------------------------------------------------------
# MANUAL STEP IN APP EEARS
# ------------------------------------------------------------------------------
# 1. Open the AppEEARS website
# 2. Create an AREA request
# 3. Upload the file: .data/switzerland.geojson
# 4. Select product: MOD13Q1.061
# 5. Select layer: NDVI
# 6. Select the desired date range
# 7. Choose GeoTIFF as output format if available
# 8. Submit the task
# 9. Download the resulting NDVI raster manually
# 10. Save it in the folder: .data/appeears_manual_download
# ------------------------------------------------------------------------------


# ==============================================================================
# 5. Read the manually downloaded NDVI raster
# ==============================================================================
manual_path <- ".data/appeears_manual_download"

# List all tif files in the folder
manual_tif <- list.files(
  manual_path,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

print(manual_tif)

# Read the raster
ndvi_raster <- rast(manual_tif[2])

#Note: manual_tif[8], 9 , 10  good quality
# Check raster information
print(ndvi_raster)

# Plot the raster

plot(ndvi_raster, main = "Manually downloaded NDVI raster")

###Mean value of NDVI data

# Read ALL NDVI rasters
ndvi_stack <- rast(manual_tif[2:61])

#Quantile value of NDVI data 
ndvi_quant <- quantile(ndvi_stack, probs=(0.9), na.rm = TRUE)
ndvi_raster <- ndvi_quant

# ==============================================================================
# 6. Clip the raster to the exact Switzerland border
# ==============================================================================
switzerland_vect <- vect(switzerland_sf)

# Reproject the Switzerland polygon to the raster CRS
switzerland_vect <- project(switzerland_vect, crs(ndvi_raster))

# Crop and mask
ndvi_switzerland <- crop(ndvi_raster, switzerland_vect)
ndvi_switzerland <- mask(ndvi_switzerland, switzerland_vect)

# Plot the clipped raster

plot(ndvi_switzerland, main = "NDVI raster clipped to Switzerland")
plot(switzerland_vect, add = TRUE, border = "black", lwd = 1)


# ==============================================================================
# 7. Convert the sampling table to spatial points
# ==============================================================================
# We assume your data frame is called matrix_full_eco
# and contains longitude and latitude columns.

points_vect <- vect(
  matrix_full_el,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

# Reproject the points to the raster CRS
points_vect <- project(points_vect, crs(ndvi_switzerland))

# Plot the points on top of the raster

plot(ndvi_switzerland, main = "Sampling points over NDVI raster")
plot(points_vect, add = TRUE, col = "red", pch = 16)


# ==============================================================================
# 8. Extract NDVI values at point locations
# ==============================================================================
ndvi_values <- terra::extract(ndvi_switzerland, points_vect)

# Check extracted values
head(ndvi_values)


# ==============================================================================
# 9. Add NDVI values to the original data frame
# ==============================================================================
# The first column returned by terra::extract() is usually the point ID
# and the second column contains the extracted raster value.

matrix_full_el$NDVI <- ndvi_values[, 2]

# Check the updated table
head(matrix_full_el)


#Rename the full matrix with all the data
matrix_full_eco_elev_clim_sat <- matrix_full_el

head(matrix_full_eco_elev_clim_sat)


# ==============================================================================
# 10. Simple control plot
# ==============================================================================

  ggplot(matrix_full_eco_elev_clim_sat, aes(x = NDVI, fill = Landcover )) +
  geom_density(alpha = 0.5, adjust = 3) +  # smoothed density curves
  labs(
    title = "NDVI Distribution by Climate",
    x = "NDVI",
    y = "Density"
  ) +
  theme_minimal()

