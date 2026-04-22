################################################################################
# EXTRACTING ELEVATION DATA IN SWITZERLAND AND VISUALIZATION
################################################################################

# =========================
# 1. Load required packages
# =========================
library(sf)        # modern spatial data handling (simple features)
library(elevatr)   # download elevation data
library(raster)    # raster data manipulation (maps)
library(ggplot2)   # data visualization

# Disable s2 geometry engine (can avoid issues in some spatial operations)
sf_use_s2(FALSE)


# =========================
# 2. Load Switzerland boundaries
# =========================
# Retrieve country borders from Natural Earth
Switzerland <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Switzerland"
)


# =========================
# 3. Download elevation data
# =========================
# z controls resolution (higher = more detail but slower)
elevation_switzerland <- get_elev_raster(Switzerland, z = 8)

# Quick visualization of the elevation raster
windows()
plot(elevation_switzerland)


# =========================
# 4. Prepare sampling points
# =========================
# We assume your dataset contains:
# - longitude
# - latitude

# Convert coordinates into a spatial object (SpatialPoints format)
spatial_points <- SpatialPoints(
  coords = matrix_full_eco[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)


# =========================
# 5. Extract elevation values
# =========================
# Extract raster values at each point location
elevation <- raster::extract(elevation_switzerland, spatial_points)

##Good to plot to control
# =========================
# 6. Add elevation to the dataset
# =========================
matrix_full_elev <- data.frame(
  matrix_full_eco,
  elevation = elevation
)


# =========================
# 7. Visualization: elevation distribution
# =========================
# Compare elevation distributions across climate categories

p3 <- ggplot(matrix_full_elev, aes(x = elevation, fill = Climate_Re)) +
  geom_density(alpha = 0.5, adjust = 3) +  # smoothed density curves
  labs(
    title = "Elevation Distribution by Climate",
    x = "Elevation (m)",
    y = "Density"
  ) +
  theme_minimal()

# Display the plot
print(p3)


# Compare elevation distributions across bat species
p4 <- ggplot(matrix_full_elev, aes(x = elevation, fill = species)) +
  geom_density(alpha = 0.5, adjust = 3) +  # smoothed density curves
  labs(
    title = "Elevation Distribution by species",
    x = "Elevation (m)",
    y = "Density"
  ) +
  theme_minimal()

# Display the plot
print(p4)