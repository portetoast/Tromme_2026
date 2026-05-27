#Necessary packages
library(raster)
library(sf)
library(rnaturalearth)
library(ggplot2)

#------------------------------------------------------------------------------
# LOAD THE ECOSYSTEM RASTER
#------------------------------------------------------------------------------

# Define the path to the GeoTIFF file
file_path <- "./.data/Ecosystem_metadata/WorldEcosystem.tif"

# Read the raster layer
# This raster contains ecosystem categories coded as numeric values
ecosystem_raster <- raster(file_path)

# Display basic information about the raster
print(ecosystem_raster)

# Optional: plot the full raster
#plot(ecosystem_raster, main = "Original Ecosystem Raster")

#------------------------------------------------------------------------------
# LOAD THE BOUNDARY OF SWITZERLAND
#------------------------------------------------------------------------------

# Download the country boundary as an sf object
Switzerland <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Switzerland"
)

# Plot the country boundary
plot(st_geometry(Switzerland), main = "Boundary of Switzerland")

#------------------------------------------------------------------------------
# CROP AND MASK THE RASTER TO SWITZERLAND
#------------------------------------------------------------------------------

# crop() keeps only the rectangular extent around Switzerland
r2 <- crop(ecosystem_raster, extent(Switzerland))

# mask() keeps only the pixels that fall inside the country boundary
ecosystem_switzerland <- mask(r2, Switzerland)

# Plot the cropped and masked raster
plot(ecosystem_switzerland, main = "Ecosystem Raster Restricted to Switzerland")

#------------------------------------------------------------------------------
# CONVERT SPECIES COORDINATES INTO SPATIAL POINTS
#------------------------------------------------------------------------------

# Convert the coordinate columns into spatial points
# The CRS used here is WGS84, which is the standard geographic coordinate system
spatial_points <- SpatialPoints(
  coords = matrix_no_dup[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

# Add the occurrence points on top of the ecosystem map
windows()
plot(ecosystem_switzerland, main = "Species Occurrences on Ecosystem Map")
plot(spatial_points, add = TRUE, pch = 16, cex = 1.2)

#------------------------------------------------------------------------------
# EXTRACT ECOSYSTEM VALUES AT EACH OCCURRENCE POINT
#------------------------------------------------------------------------------

# extract() retrieves the raster value at the location of each point
# Each point receives the ecosystem code of the raster cell where it falls
eco_values <- raster::extract(ecosystem_switzerland, spatial_points)

# Check the extracted values
head(eco_values)

#------------------------------------------------------------------------------
# ADD THE EXTRACTED ECOSYSTEM VALUES TO THE ORIGINAL DATA FRAME
#------------------------------------------------------------------------------

# Create a new data frame by adding the extracted ecosystem values
matrix_full_eco <- data.frame(matrix_no_dup, eco_values)

# Inspect the result
head(matrix_full_eco)

#------------------------------------------------------------------------------
# 8) LOAD THE ECOSYSTEM METADATA TABLE
#------------------------------------------------------------------------------

# This metadata table links the numeric raster code to descriptive ecosystem names
metadata_eco <- read.delim("./.data/Ecosystem_metadata/WorldEcosystem.metadata.tsv")

# Inspect the metadata table
head(metadata_eco)

#------------------------------------------------------------------------------
# 9) MERGE THE EXTRACTED VALUES WITH THE METADATA
#------------------------------------------------------------------------------

# Merge the occurrence table with the metadata table
# by.x = "eco_values" means the ecosystem code in our occurrence table
# by.y = "Value" means the corresponding code column in the metadata table
matrix_full_eco <- merge(
  matrix_full_eco,
  metadata_eco,
  by.x = "eco_values",
  by.y = "Value"
)

# Inspect the enriched table
head(matrix_full_eco)

#------------------------------------------------------------------------------
# 10) VISUALIZE THE NUMBER OF OBSERVATIONS PER CLIMATE CATEGORY AND SPECIES
#------------------------------------------------------------------------------

# Create a bar plot showing how many observations of each species
# are found in each climate category
p2 <- ggplot(matrix_full_eco, aes(x = Climate_Re, fill = species)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Count of Observations of Each Species by Climate",
    x = "Climate category",
    y = "Number of observations"
  ) +
  theme_minimal()

# Display the plot
print(p2)

#This plot shows that Myotis myotis is found in the higher majority in Cool temparate moist climate and that
#Myotis blythii is found only in Cool temperate mosit climate

#Barplot of the observations of each species per landcover types
p3 <- ggplot(matrix_full_eco, aes(x = Landcover, fill = species)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Count of Observations of Each Species by landcover",
    x = "Lancover category",
    y = "Number of observations"
  ) +
  theme_minimal()

# Display the plot
print(p3)

colnames(matrix_full_eco_elev_clim_sat)
#This plot shows that Myotis myotis is found at different land types but in majority in croplands.
#Myotis blythii is also found in different types of landcover but more in forests. 