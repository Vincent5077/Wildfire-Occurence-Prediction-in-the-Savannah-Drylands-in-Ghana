library(terra)

# Load NDVI raster
ndvi_2000 <- rast("D:/EDU DOCS/Peoples Project/Fire Modelling/raster/ndvi_2025.tif")

# Inspect NDVI values (optional)
summary(ndvi_2000)

# -----------------------------------
# NDVI reclassification matrix
# (Adjust values if needed)
# -----------------------------------

rcl <- matrix(c(
  -1.0, 0.0,  1,   # Water
  0.0, 0.18, 2,   # Settlement / bare
  0.18, 0.30, 3,  # Shrubs
  0.30, 0.50, 4,  # Agriculture
  0.50, 1.00, 5   # Woodland
), ncol = 3, byrow = TRUE)

# Reclassify NDVI to LULC
lulc <- classify(ndvi_2000, rcl)

# -----------------------------------
# Assign class names
# -----------------------------------

levels(lulc) <- data.frame(
  ID = 1:5,
  Class = c(
    "Water",
    "Settlement",
    "Shrubs",
    "Agriculture",
    "Woodland"
  )
)

# -----------------------------------
# Natural color scheme
# -----------------------------------

lulc_cols <- c(
  "#2166AC",  # Water - blue
  "#B2182B",  # Settlement - red
  "#FDD835",  # Shrubs - yellow
  "#7CB342",  # Agriculture - light green
  "#1B5E20"   # Woodland - dark green
)

# -----------------------------------
# Plot LULC
# -----------------------------------

plot(lulc, col = lulc_cols, legend = FALSE)

legend(
  "bottomleft",
  #legend = levels(lulc_2000)[[1]]$Class,
  fill = lulc_cols,
  title = "LULC Classes (2000)",
  bty = "n",
  cex = 0.7
)

writeRaster(lulc, "D:/EDU DOCS/Peoples Project/Fire Modelling/raster/2025.tif")
# ============================================================
# DESCRIPTIVE ANALYSIS: TEMPERATURE & PRECIPITATION (2000–2025)
# ============================================================

# Libraries


library(data.table)
library(ggplot2)
library(lubridate)  # for easy date handling

# Load data
data <- fread("D:/EDU DOCS/Peoples Project/Fire Modelling/merged_fire_data_2025_2000.csv")

# Convert date column to Date type
# Replace 'fire_date' with your actual column name
data[, acq_date := as.Date(acq_date, format = "%d/%m/%Y")]

# Extract year
data[, year := year(acq_date)]  # lubridate::year()

# Filter study years
years_used <- c(2000, 2005, 2010, 2015, 2020, 2025)
data_sub <- data[year %in% years_used]

# Check
head(data_sub[, .(acq_date, year)])

# ------------------------------------------------------------
# 3. DESCRIPTIVE STATISTICS
# ------------------------------------------------------------

# Temperature statistics
temp_stats <- data_sub[, .(
  Mean   = mean(Temperature_C, na.rm = TRUE),
  Median = median(Temperature_C, na.rm = TRUE),
  SD     = sd(Temperature_C, na.rm = TRUE),
  Min    = min(Temperature_C, na.rm = TRUE),
  Max    = max(Temperature_C, na.rm = TRUE)
), by = year]

print("Temperature Descriptive Statistics")
print(temp_stats)

# Precipitation statistics
precip_stats <- data_sub[, .(
  Mean   = mean(precip, na.rm = TRUE),
  Median = median(precip, na.rm = TRUE),
  SD     = sd(precip, na.rm = TRUE),
  Min    = min(precip, na.rm = TRUE),
  Max    = max(precip, na.rm = TRUE)
), by = year]

print("Precipitation Descriptive Statistics")
print(precip_stats)

# ------------------------------------------------------------
# 4. BOXPLOTS (THESIS-RECOMMENDED)
# ------------------------------------------------------------

# Temperature boxplot
ggplot(data_sub, aes(x = factor(year), y = Temperature_C)) +
  geom_boxplot(fill = "orange", alpha = 0.7, outlier.size = 0.8) +
  labs(
    x = "Year",
    y = "Temperature (°C)",
    title = "Annual Temperature Distribution (2000–2025)"
  ) +
  theme_minimal()

# Precipitation boxplot
ggplot(data_sub, aes(x = factor(year), y = precip)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7, outlier.size = 0.8) +
  labs(
    x = "Year",
    y = "Precipitation",
    title = "Annual Precipitation Distribution (2000–2025)"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 5. OPTIONAL: VIOLIN + BOXPLOT (APPENDIX USE)
# ------------------------------------------------------------

# Temperature violin
temp <- ggplot(data_sub, aes(x = factor(year), y = Temperature_C)) +
  geom_violin(fill = "orange", alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 0.6) +
  labs(
    x = "Year",
    y = "Temperature (°C)",
    title = "Temperature Distribution by Year"
  ) +
  theme_minimal()

# Precipitation violin
p_vio <- ggplot(data_sub, aes(x = factor(year), y = precip)) +
  geom_violin(fill = "skyblue", alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 0.6) +
  labs(
    x = "Year",
    y = "Precipitation",
    title = "Precipitation Distribution by Year"
  ) +
  theme_minimal()
# # =========================================================
 ggsave(
   filename = "D:/EDU DOCS/Peoples Project/Fire Modelling/temperature.png",
   plot = temp,
   width = 11,
   height = 6,
   dpi = 600
)
p_vio