# Load required packages
library(Rchelsa)  # Main package for CHELSA
library(terra)    # Spatial data manipulation
library(dplyr)    # Tabular data manipulation
library(ggplot2)  # Visualisations
library(tidyr)


matrix_final <- matrix_full_eco_elev_clim_sat

# Créer un identifiant unique par ligne
matrix_final <- matrix_final %>%
  mutate(occurrence_id = row_number())

species_df <- matrix_final
print(species_df)

# =========================
# 4) MONTHLY MAXIMUM TEMPERATURE (Tmax)
# =========================

# Recréer pts_v et coords_df (comme avant, mais maintenant occurrence_id existe)
pts_v <- terra::vect(
  species_df,
  geom = c("longitude", "latitude"),
  crs  = "EPSG:4326"
)

coords_df <- as.data.frame(terra::geom(pts_v)[, c("x", "y")]) %>%
  rename(
    longitude = x,
    latitude  = y
  ) %>%
  mutate(occurrence_id = species_df$occurrence_id)

print(coords_df)

tmax_raw <- getChelsa(
  var       = "tasmax",
  coords    = coords_df %>% dplyr::select(longitude, latitude),
  startdate = as.Date("2020-01-01"),
  enddate   = as.Date("2021-01-01"),  # Exclusive: January 2018 to December 2018
  dataset   = "chelsa-monthly"
)


names(tmax_raw)[names(tmax_raw) != "time"] <- paste0("occ_", coords_df$occurrence_id)

tmax_long <- tmax_raw %>%
  pivot_longer(
    cols = starts_with("occ_"),
    names_to = "occurrence_id",
    values_to = "tasmax_K",
    names_prefix = "occ_"
  ) %>%
  mutate(
    tasmax_C = tasmax_K - 273.15,
    occurrence_id = as.integer(occurrence_id)
  )

# Joindre tout ensemble : climat + espèce + date + variables écologiques
data_climate <- tmax_long %>%
  left_join(matrix_final, by = "occurrence_id")

head(data_climate)

# Calculate annual mean for each point
# colMeans() computes the mean of each column
tmax_df <- tmax_long %>%
  group_by(occurrence_id) %>%
  summarise(tmax_mean_c = mean(tasmax_C, na.rm = TRUE))
matrix_final <- matrix_final %>%
  left_join(tmax_df, by = "occurrence_id")

head(matrix_final)


# =========================
# MONTHLY PRECIPITATION
# =========================
precip_raw <- getChelsa(
  var       = "pr",
  coords    = coords_df %>% dplyr::select(longitude, latitude),
  startdate = as.Date("2020-01-01"),
  enddate   = as.Date("2021-01-01"),
  dataset   = "chelsa-monthly"
)

# Renommer les colonnes selon occurrence_id
names(precip_raw)[names(precip_raw) != "time"] <- paste0("occ_", coords_df$occurrence_id)

# Passer en format long
precip_long <- precip_raw %>%
  pivot_longer(
    cols = starts_with("occ_"),
    names_to = "occurrence_id",
    values_to = "precip_mm",
    names_prefix = "occ_"
  ) %>%
  mutate(occurrence_id = as.integer(occurrence_id))

head(precip_long)

# Moyenne annuelle de précipitation par point
precip_df <- precip_long %>%
  group_by(occurrence_id) %>%
  summarise(precip_mean = mean(precip_mm, na.rm = TRUE))

print(precip_df)

# Ajouter à matrix_final
matrix_final <- matrix_final %>%
  left_join(precip_df, by = "occurrence_id")

head(matrix_final)

# CHECKING THE RESULTS
# =========================

cat("Dimensions of the initial table: ",  dim(species_df), "\n")
cat("Dimensions of the enriched table: ", dim(matrix_final), "\n")
cat("Added columns: ", setdiff(names(matrix_final), names(species_df)), "\n\n")

print(summary(matrix_final$tmax_mean_c))

print(summary(matrix_final$precip_mean))


# =========================
# VISUALISATIONS OF CURRENT DATA
# =========================

# Plot 1: Distribution of maximum temperature
p1 <- ggplot(matrix_final, aes(x = tmax_mean_c)) +
  geom_density(color = "darkred", fill = "salmon", alpha = 0.6, adjust = 1.5) +
  geom_rug(color = "darkred") +
  theme_classic(base_size = 12) +
  labs(
    title    = "Distribution of mean maximum temperature",
    subtitle = "Myotis myotis - Year 2020",
    x = "Mean annual maximum temperature (°C)",
    y = "Density"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(p1)

# Plot 2: Distribution of precipitation
p2 <- ggplot(matrix_final, aes(x = precip_mean)) +
  geom_density(color = "black", fill = "darkgreen", alpha = 0.6, adjust = 1.5) +
  geom_rug(color = "darkgreen") +
  theme_classic(base_size = 12) +
  labs(
    title    = "Distribution of mean precipitation",
    subtitle = "Myotis myotis - Year 2020",
    x = "Mean annual precipitation (mm)",
    y = "Density"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(p2)

# Plot 3: Temperature-precipitation relationship
p3 <- ggplot(matrix_final, aes(x = tmax_mean_c, y = precip_mean)) +
  geom_point(size = 3, color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  theme_classic(base_size = 12) +
  labs(
    title    = "Temperature-precipitation relationship",
    subtitle = "Myotis myotis - year 2020",
    x = "Mean maximum temperature (°C)",
    y = "Mean precipitation (mm)"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(p3)


###############################################################################
# PART 2: CURRENT CLIMATE VS FUTURE CLIMATE
###############################################################################

# =========================
# 10) CURRENT CLIMATE: July temperature (climatology 1981-2010)
# =========================

tas_current_july <- getChelsa(
  var     = "tas",
  coords  = coords_df %>% dplyr::select(longitude, latitude),
  date    = c(7, 1981, 2010),   # Month 7 (July), period 1981-2010
  dataset = "chelsa-climatologies"
)

str(tas_current_july)

cat("For CURRENT CLIMATE (dataset = 'chelsa-climatologies'):\n")
cat("  └─ Dates are FIXED and cannot be changed\n")
cat("  └─ Reference period: 1981-2010\n")
cat("  └─ Format: date = c(MONTH, 1981, 2010)\n")
cat("  └─ Examples:\n")
cat("     • date = c(1, 1981, 2010)  → January\n")
cat("     • date = c(7, 1981, 2010)  → July\n")
cat("     • date = c(12, 1981, 2010) → December\n\n")



# Renommer les colonnes climatiques par occurrence_id
names(tas_current_july)[names(tas_current_july) != "time"] <- 
  paste0("occ_", coords_df$occurrence_id)

# Garder seulement la première ligne (les 3 lignes sont identiques)
current_july_df <- tas_current_july %>%
  slice(1) %>%
  dplyr::select(-time) %>%
  pivot_longer(
    cols = everything(),
    names_to = "occurrence_id",
    values_to = "tas_K",
    names_prefix = "occ_"
  ) %>%
  mutate(
    current_july_temp_c = tas_K - 273.15,
    occurrence_id = as.integer(occurrence_id)
  ) %>%
  dplyr::select(occurrence_id, current_july_temp_c)

print(current_july_df)

# =========================
# 11) FUTURE CLIMATE: July temperature in 2050 (SSP126 scenario)
# =========================

tas_future_july <- getChelsa(
  var     = "tas",
  coords  = coords_df %>% dplyr::select(longitude, latitude),
  date    = as.Date("2051-07-01"),
  dataset = "chelsa-climatologies",
  ssp     = "ssp126",           # Emission scenario
  forcing = "MPI-ESM1-2-HR"     # Climate model
)


cat("For FUTURE PROJECTIONS (dataset = 'chelsa-climatologies'):\n")
cat("  └─ Prediction dates are LIMITED to specific periods\n")
cat("  └─ Available periods: 2011-2040, 2041-2070, 2071-2100\n")
cat("  └─ Format: date = as.Date('YEAR-MONTH-01')\n")
cat("  └─ Examples of valid dates:\n")
cat("     • as.Date('2030-07-01') → Represents the period 2011-2040\n")
cat("     • as.Date('2050-07-01') → Represents the period 2041-2070\n")
cat("     • as.Date('2080-07-01') → Represents the period 2071-2100\n\n")

cat("  ⚠️  WARNING: The exact year is just an indicator!\n")
cat("      - 2030 = average of 2011-2040\n")
cat("      - 2050 = average of 2041-2070\n")
cat("      - 2080 = average of 2071-2100\n\n")


# Vérifier la structure d'abord
str(tas_future_july)

# Renommer les colonnes climatiques par occurrence_id
names(tas_future_july)[names(tas_future_july) != "time"] <- 
  paste0("occ_", coords_df$occurrence_id)

# Create a table with future temperatures
future_july_df <- tas_future_july %>%
  slice(1) %>%
  dplyr::select(-time) %>%
  pivot_longer(
    cols = everything(),
    names_to = "occurrence_id",
    values_to = "tas_K",
    names_prefix = "occ_"
  ) %>%
  mutate(
    future_july_temp_c = tas_K - 273.15,
    occurrence_id = as.integer(occurrence_id)
  ) %>%
  dplyr::select(occurrence_id, future_july_temp_c)

print(future_july_df)

# =========================
# MERGING AND CALCULATING CHANGE
# =========================
species_climate_future_df <- matrix_final %>%
  left_join(current_july_df, by = "occurrence_id") %>%
  left_join(future_july_df,  by = "occurrence_id") %>%
  dplyr::mutate(
    july_temp_change_c = future_july_temp_c - current_july_temp_c
  )

print(species_climate_future_df)

# Statistics on temperature change
print(summary(species_climate_future_df$july_temp_change_c))
cat("\nMean projected change: ", 
    round(mean(species_climate_future_df$july_temp_change_c), 2), "°C\n\n")

# =========================
# 13) VISUALISATIONS OF FUTURE PROJECTIONS
# =========================

# Plot 4: Current vs future comparison
p4 <- ggplot(species_climate_future_df, 
             aes(x = current_july_temp_c, y = future_july_temp_c)) +
  geom_point(size = 4, color = "steelblue", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", 
              color = "gray40", linewidth = 1) +
  annotate("text", x = min(species_climate_future_df$current_july_temp_c) + 0.5,
           y = max(species_climate_future_df$future_july_temp_c) - 0.5,
           label = "1:1 line\n(no change)", 
           color = "gray40", size = 3.5) +
  theme_classic(base_size = 12) +
  labs(
    title    = "July temperature: Current vs Future (2050)",
    subtitle = "Pinus sylvestris - SSP126 scenario",
    x = "Current July temperature (°C) [1981-2010]",
    y = "Future July temperature (°C) [2050]"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(p4)

# Plot 5: Distribution of temperature change
p5 <- ggplot(species_climate_future_df, aes(x = july_temp_change_c)) +
  geom_histogram(bins = 10, fill = "orange", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mean(species_climate_future_df$july_temp_change_c),
             color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", 
           x = mean(species_climate_future_df$july_temp_change_c) + 0.1,
           y = Inf,
           label = paste0("Mean: ", 
                         round(mean(species_climate_future_df$july_temp_change_c), 2), 
                         "°C"),
           color = "red", vjust = 2, hjust = 0) +
  theme_classic(base_size = 12) +
  labs(
    title    = "Distribution of projected temperature change",
    subtitle = "Difference between 2050 (SSP126) and 1981-2010",
    x = "July temperature change (°C)",
    y = "Number of occurrences"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(p5)

# Plot 6: Map of changes
p6 <- ggplot(species_climate_future_df, 
             aes(x = longitude, y = latitude, color = july_temp_change_c)) +
  geom_point(size = 5, alpha = 0.8) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red",
                        midpoint = mean(species_climate_future_df$july_temp_change_c),
                        name = "Δ Temp (°C)") +
  theme_classic(base_size = 12) +
  labs(
    title    = "Spatial distribution of temperature change",
    subtitle = "July 2050 (SSP126) vs 1981-2010",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

print(p6)