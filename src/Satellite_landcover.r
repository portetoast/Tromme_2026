#Here is a small bonus satellite data analysis of landcover raster provided by MODIS, to compare with the ecosystem landcover.

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

getProducts("^MOD|^MYD|^MCD")

MODIStsp_get_prodlayers("M*D13Q1")

productInfo(product)

# ==============================================================================
# 3. Export the Switzerland polygon for manual upload in AppEEARS
# ==============================================================================

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

# ==============================================================================
# 5. Read the manually downloaded Landcover raster
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
landcover_raster <- rast(manual_tif[1])

# Check raster information
print(landcover_raster)

# Plot the raster
windows()
plot(landcover_raster, main = "Manually downloaded landcover raster")

switzerland_vect <- vect(switzerland_sf)

# Reproject the Switzerland polygon to the raster CRS
switzerland_vect <- project(switzerland_vect, crs(landcover_raster))

# Crop and mask
landcover_switzerland <- crop(landcover_raster, switzerland_vect)
landcover_switzerland <- mask(landcover_switzerland, switzerland_vect)

# Plot the clipped raster
windows()
plot(landcover_switzerland, main = "Landcover raster clipped to Switzerland")
plot(switzerland_vect, add = TRUE, border = "black", lwd = 1)

points_vect <- vect(
  matrix_full_eco_elev_clim_sat,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

# Reproject the points to the raster CRS
points_vect <- project(points_vect, crs(landcover_switzerland))

# Plot the points on top of the raster
windows()
plot(landcover_switzerland, main = "Sampling points over Landcover raster")
plot(points_vect, add = TRUE, col = "red", pch = 16)

lc_values <- terra::extract(landcover_raster, points_vect)

matrix_full_eco_elev_clim_sat$LandCoverName <- lc_values[,2]

#Transform the code numbers into names 
lc_labels <- c(
  "Water",
  "Evergreen Needleleaf Forest",
  "Evergreen Broadleaf Forest",
  "Deciduous Needleleaf Forest",
  "Deciduous Broadleaf Forest",
  "Mixed Forest",
  "Closed Shrublands",
  "Open Shrublands",
  "Woody Savannas",
  "Savannas",
  "Grasslands",
  "Permanent Wetlands",
  "Croplands",
  "Urban",
  "Cropland/Natural Mosaic",
  "Snow/Ice",
  "Barren",
  "Water"
)

#Integration of the data in the matrix
matrix_full_eco_elev_clim_sat$LandCoverName <- lc_labels[matrix_full_eco_elev_clim_sat$LandCoverName + 1]

head(matrix_full_eco_elev_clim_sat)

View(matrix_full_eco_elev_clim_sat)

windows()
pp <- ggplot(matrix_full_eco_elev_clim_sat, aes(x = LandCoverName, fill = species)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Count of Observations of Each Species by Modis Landcover",
    x = "Lancover category",
    y = "Number of observations"
  ) +
  theme_minimal()

  print(pp)

  # Comparing this plot with the ecosystem plot of the distribution of the 2 species,
  # We can see that this one splits landcover types in more different categories and that 
  # Myotis myotis is still predominantly found in croplands, but also in a new categorie, the savanna, and
  # Myotis blythii is more present in woody savannas and in second position the mixed forests.
  # The main analysis will remain on the data from ecosystem raster, but it's interesting to see how much variations there are
  # depending on the data you chose to work with.