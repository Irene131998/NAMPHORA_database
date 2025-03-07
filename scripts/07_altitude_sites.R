## Script for adding altitude values to sites without altitude information
  
# 0. Load libraries and functions ----

source("functions.R")

libraries <- c("dplyr","terra","readr")

# Install missing packages
invisible(lapply(libraries, install_if_missing))

# Load the libraries
lapply(libraries, require, character.only = TRUE)


# 1. Load data ----

# Sites
sites <- read_csv(normalizePath("../metadata/pollen_data/database.csv"), locale = locale(encoding = "latin1"))

# Elevation
elevation <- rast(normalizePath("../data/raw_data/mapping_data/elevation.tiff"))


# 2. Add altitude values and save ----

# Extract altitude for all sites
coords <- cbind(sites$Longitude, sites$Latitude)
altitudes <- extract(elevation, coords)

# Add altitude values where missing
sites_w_altitude <- sites %>%
  mutate(Altitude = ifelse(is.na(Altitude), altitudes[,"mean"], Altitude))  # Replace missing values

# Save updated database
write.table(
  sites_w_altitude, 
  file = normalizePath("../metadata/pollen_data/database.csv", mustWork = FALSE),
  sep = ",",         
  row.names = FALSE, 
  fileEncoding = "latin1" 
)

