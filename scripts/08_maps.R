## Script for data visualisation. Creation of maps of all the fossil and modern pollen records compiled in this database
  
# 0. Load libraries and functions----

source("scripts/functions.R")

libraries <-c("sf", "terra", "ggplot2", "dplyr", "readr", "leaflet", "htmlwidgets", "RColorBrewer")


# Install missing packages
invisible(lapply(libraries, install_if_missing))

# Load the libraries
lapply(libraries, require, character.only = TRUE)

# 1) Read data----

## Biomes Olson (2001)----
biomes <- sf::st_read(normalizePath("data/raw_data/mapping_data/EcoregionsWWF_2017/wwf_terr_ecos.shp"))
biomes <- sf::st_make_valid(biomes)
biomes <- sf::st_crop(biomes, ext(c(-19,62,0,46.5)))
biomes$BIOME <- as.factor(biomes$BIOME)

biome_definitions <- read.csv(normalizePath("data/raw_data/mapping_data/EcoregionsWWF_2017/Biome_definitions.csv"))
colnames(biome_definitions) <- c("BIOME","BIOME_definition")

# Merge the biome definitions with the shapefile 
biomes <- merge(biomes, biome_definitions, by = "BIOME", all.x = TRUE)

# Filter out rows where BIOME is 98 and BIOME_definition is NA or empty
biomes <- biomes[!(biomes$BIOME == 98 &  (is.na(biomes$BIOME_definition) | biomes$BIOME_definition == "")), ]

## Regions (modified from Olson 2001)----
regions <- sf::st_read(normalizePath("data/raw_data/mapping_data/Regions_WWF_2017/Ecoregions_Europe_Arabian_Peninsula.shp"))
regions <- sf::st_make_valid(regions)
regions <- sf::st_crop(regions, ext(c(-19,62,0,46.5)))
regions$Reg_names <- as.factor(regions$Reg_names)

##  African vegetation White (1983)----
phytogeographic_regions_White <- sf::st_read(normalizePath("data/raw_data/mapping_data/Africa_Vegetation_White_1983/afwhite_2.shp"))
phytogeographic_regions_White <- sf::st_make_valid(phytogeographic_regions_White)

# Check if the crop extent is in the same CRS
crs_phytogeographic <- crs(phytogeographic_regions_White)
crs_extent <- st_crs(4326)  # Assuming the extent is in WGS84
if (!identical(crs_phytogeographic, crs_extent)) {
  phytogeographic_regions_White <- st_transform(phytogeographic_regions_White, crs = crs_extent)
}

phytogeographic_regions_White <- sf::st_crop(phytogeographic_regions_White, ext(c(-19,55,0,40)))
phytogeographic_regions_White$PHYTOCHO_1 <- as.factor(phytogeographic_regions_White$PHYTOCHO_1)


# Change names
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("^[IVXLCDM]+\\.\\s*", "", phytogeographic_regions_White$PHYTOCHO_1)

phytogeographic_regions_White$PHYTOCHO_1 <- gsub("Mediterranean Region", "Mediterranean Africa", phytogeographic_regions_White$PHYTOCHO_1)

phytogeographic_regions_White$PHYTOCHO_1 <- gsub("VIII and IX. Afromontane and Afroalpine Regions", "Afromontane and Afroalpine Regions", phytogeographic_regions_White$PHYTOCHO_1)

phytogeographic_regions_White$PHYTOCHO_1 <- gsub("Guinca-Congolia/Zambczia Regional Transition Zone", "Guinea-Congolia/Zambezia Regional Transition Zone", phytogeographic_regions_White$PHYTOCHO_1)

phytogeographic_regions_White$PHYTOCHO_1 <- gsub("Sahara Regional TransitionZone", "Sahara Regional Transition", phytogeographic_regions_White$PHYTOCHO_1)

phytogeographic_regions_White$PHYTOCHO_1 <- gsub("Guinea-Congolia/Zambezia Regional Transition Zone", "Guineo-Congolian/Zambezian Transition ", phytogeographic_regions_White$PHYTOCHO_1)

phytogeographic_regions_White$PHYTOCHO_1 <- gsub("Guinea-Congolia/Sudania Regional Transition Zone", "Guineo-Congolian/Sudanian Transition", phytogeographic_regions_White$PHYTOCHO_1)

phytogeographic_regions_White$PHYTOCHO_1 <- gsub("Mediterranean/Sahara Regional Transition Zone", "Mediterranean/Sahara Transition", phytogeographic_regions_White$PHYTOCHO_1)

phytogeographic_regions_White$PHYTOCHO_1 <- gsub("Zanzibar-Inhambane Regional Mosaic", "Zanzibar-Inhambane Mosaic", phytogeographic_regions_White$PHYTOCHO_1)


##  Combine regions into one shapefile
# Check the CRS of both shapefiles
crs_regions <- st_crs(regions)
crs_phytogeographic <- st_crs(phytogeographic_regions_White)

# If the CRS are different, transform one to match the other
if (!identical(crs_regions, crs_phytogeographic)) {
  # Transform phytogeographic_regions_White CRS to match regions CRS
  phytogeographic_regions_White <- st_transform(phytogeographic_regions_White, crs = crs_regions)
}

# Ensure both shapefiles have matching columns
colnames(phytogeographic_regions_White)[colnames(phytogeographic_regions_White) == "PHYTOCHO_1"] <- "Region_Name"
colnames(regions)[colnames(regions) == "Reg_names"] <- "Region_Name"

phytogeographic_regions_White <- phytogeographic_regions_White |> select(Region_Name,geometry)
regions <- regions |>  select(Region_Name,geometry)

#  Merge both shapefiles
merged_phytogeographic_regions <- rbind(regions, phytogeographic_regions_White)

##  Sites----
sites <- read_csv(normalizePath("metadata/pollen_data/database.csv"))
sites$Latitude <- as.numeric(sites$Latitude)

### Fossil sites
fossil_sites <- sites |> filter(Pollen=="Fossil")  |>  select(Site_name_machine_readable,Longitude, Latitude,Dated, `Link to database`)
fossil_sites <- na.omit(fossil_sites)

### Modern sites
modern_sites <- sites |> filter(Pollen=="Modern") |>  select(Site_name_machine_readable,Longitude, Latitude,Dated,`Link to database`)

## Elevation----
elevation <- rast(normalizePath("data/raw_data/mapping_data/elevation.tiff"))
elevation_crop <- crop(elevation,ext(c(-19,62,0,46.5)))

### Calculate hillshade for further plotting
slopes <- terrain(elevation_crop, "slope", unit = "radians")
aspect <- terrain(elevation_crop, "aspect", unit = "radians")
hs <- shade(slopes, aspect) # base shade for elevation plotting


# Convert the raster to a data frame for plotting with ggplot2
elevation_df <- as.data.frame(elevation_crop, xy = TRUE)
colnames(elevation_df) <- c("x", "y", "value")


# 2) Plot fossil pollen records----

## 2.1) Interactive map----

# Create a color palette based on the "Dated" column
color_palette <- colorFactor(palette = c("red", "blue"), domain = fossil_sites$Dated)

# Create leaflet map 
fossil_sites_map <- leaflet(fossil_sites) %>%
  addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
  addCircleMarkers(
    lng = ~Longitude, lat = ~Latitude,  # Coordinates
    color = ~color_palette(Dated),  # Color based on "Dated"
    radius = 2, fillOpacity = 0.8,
    popup = ~paste( "<b>Name:</b>", Site_name_machine_readable, "<br>",
                    "<b>Dated:</b>", Dated, "<br>",
                    "<b>Link:</b>", `Link to database`, "<br>")  # Show name of site, date info and link
  ) %>%
  addLegend(
    position = "bottomright", 
    pal = color_palette, values = ~Dated, 
    title = "Dated", opacity = 1
  )

# Save as an HTML file
saveWidget(fossil_sites_map,normalizePath("outputs/maps/fossil_sites_interactive_map.html"), selfcontained = TRUE)


## 2.2) Static map (ggplot)----

fossil_sites_plot <- ggplot() +
  geom_raster(data = elevation_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colors = terrain.colors(100)) +  # Terrain color scale
  geom_sf(data = biomes_crop, aes(geometry = geometry), fill = NA, color = "black", lwd = 0.5) +
  geom_point(data = fossil_sites, mapping = aes(x = Longitude, y = Latitude, color = as.factor(Dated)), size = 2) +
  theme_minimal() +
  labs(fill = "Elevation (m)", color = "Dated") +
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(fossil_sites$Dated)), "Set1"))


ggsave(normalizePath("outputs/maps/fossil_sites_ggplot.png"), fossil_sites_plot, width = 17, height = 10, dpi = 300)

fossil_sites_plot


## 2.3) Static map (base R)----
png(normalizePath("outputs/maps/fossil_sites_plot.png"),  
    width = 4, 
    height = 3.5,   
    units = "cm",    
    res = 1200,    
    pointsize = 4)

# Plot the raster
plot(hs, col = gray(0:100 / 100), legend = FALSE, axes = TRUE)

# Overlay with elevation
plot(elevation_crop, col = terrain.colors(25), alpha = 0.5, legend = FALSE, axes = FALSE, add = TRUE)

# Add the points
points(fossil_sites$Longitude[fossil_sites$Dated == "Yes"],  
       fossil_sites$Latitude[fossil_sites$Dated == "Yes"],  
       col = "blue", pch = 19, cex = 0.4)  

points(fossil_sites$Longitude[fossil_sites$Dated == "No"],  
       fossil_sites$Latitude[fossil_sites$Dated == "No"],  
       col = "red", pch = 19, cex = 0.4)  

# Add legend with xpd enabled
legend("bottomright",  
       legend = c("Dated", "Not dated"), 
       col = c("blue", "red"), 
       pch = 19, 
       cex = 0.5,
       bty = "o", 
       xpd = TRUE)  # Allow the legend to extend outside the plot

# Close the PNG device
dev.off()



# 3) Plot modern pollen records Interactive map----

## 3.1) Interactive map----

# Create a color palette based on the "Dated" column
color_palette <- colorFactor(palette = "green", domain = modern_sites$Dated)

# Create leaflet map 
modern_sites_map <- leaflet(modern_sites) %>%
  addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
  addCircleMarkers(
    lng = ~Longitude, lat = ~Latitude,  # Coordinates
    color = ~color_palette(Dated),  # Color based on "Dated"
    radius = 2, fillOpacity = 0.8,
    popup = ~paste( "<b>Name:</b>", Site_name_machine_readable, "<br>",
                    "<b>Dated:</b>", Dated, "<br>",
                    "<b>Link:</b>", `Link to database`, "<br>")  # Show name of site, date info and link
  ) %>%
  addLegend(
    position = "bottomright", 
    pal = color_palette, values = ~Dated, 
    title = "Dated", opacity = 1
  )

# Save as an HTML file
saveWidget(modern_sites_map,normalizePath("outputs/maps/modern_sites_interactive_map.html"), selfcontained = TRUE)


## 3.2) Static map (ggplot)----

modern_sites_plot <- ggplot() +
  geom_raster(data = elevation_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colors = terrain.colors(100)) +  # Terrain color scale
  geom_sf(data = biomes_crop, aes(geometry = geometry), fill = NA, color = "black", lwd = 0.5) +
  geom_point(data = modern_sites, mapping = aes(x = Longitude, y = Latitude, color = as.factor(Dated)), size = 2) +
  theme_minimal() +
  labs(fill = "Elevation (m)", color = "Dated") +
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(fossil_sites$Dated)), "Set1"))


ggsave(normalizePath("outputs/maps/modern_sites_ggplot.png"), modern_sites_plot, width = 17, height = 10, dpi = 300)

modern_sites_plot

## 3.3) Static map (base R)----

png(normalizePath("outputs/maps/modern_sites_plot.png"),  
    width= 4, 
    height= 3.5,   
    units= "cm",    
    res= 1200,    
    pointsize = 4)

# Plot the raster
plot(hs, col = gray(0:100 / 100), legend = FALSE, axes = TRUE)
# overlay with elevation
plot(elevation_crop, col = terrain.colors(25), alpha = 0.5, legend = FALSE,axes = FALSE, add = TRUE)

# Overlay the shapefile
#plot(sf::st_geometry(biomes), add = TRUE, col = "transparent", border = "black")

# Add the points
points(modern_sites$Longitude,  
       modern_sites$Latitude,  
       col = "black", pch = 19, cex = 0.4)  

dev.off()

# 4) Phytogeographical map -----

png(normalizePath("outputs/maps/phytogeographical_regions_map.png"), width = 1600, height = 1800, res = 300)

layout(matrix(1:2, nrow = 1, ncol = 2, byrow = TRUE), 
       widths = c(0.7, 1),  #  column widths
       heights = c(2, 2, 2))  # Adjust heights to fit all plots

### Plot 1: Phytogeographic Regions ###

# Define colors
n <- length(unique(merged_phytogeographic_regions$Region_Name))
colors_regions <- colorRampPalette(brewer.pal(12, "Set3"))(n)

plot(elevation_crop, col = terrain.colors(25), alpha = 0, legend = FALSE, axes = TRUE)
plot(st_geometry(merged_phytogeographic_regions), 
     col = colors_regions[as.numeric(merged_phytogeographic_regions$Region_Name)], 
     , border = "black", main = "", add = TRUE)


### Plot 2: Legend Phytogeographic Regions ###
par(mar = c(0, 0, 0, 0))  # Remove margins
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
legend("left", 
       legend = levels(merged_phytogeographic_regions$Region_Name),
       fill = colors_regions, 
       border = "black", 
       cex = 1, 
       title = NULL, 
       bty = "n",  
       xpd = TRUE,
       ncol=2)
mtext("Phytogeographic regions                   ", side = 3, line = -1, cex = 0.6, col = "black")

dev.off()

# 5) Combined map (fossil + modern) ----
## 5.1.) Interactive ----
# Eliminate na rows
sites <- sites |>
  filter(!is.na(Dated))

# Create a color palette based on the "Dated" column
color_palette <- colorFactor(palette =  c("green", "red","blue"), domain = sites$Dated)

# Create leaflet map 

sites_map <- leaflet(sites) %>%
  addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
  addCircleMarkers(
    lng = ~Longitude, lat = ~Latitude,
    color = ~color_palette(Dated),  # Color based on "Dated"
    radius = 2, fillOpacity = 0.8,
    popup = ~paste( "<b>Name:</b>", Site_name_machine_readable, "<br>",
                    "<b>Dated:</b>", Dated, "<br>",
                    "<b>Link:</b>", `Link to database`, "<br>")  # Show name of site, date info and link
  ) %>%
  addLegend(
    position = "bottomright", 
    pal = color_palette, values = ~Dated, 
    title = "Dated", opacity = 1
  )

# Save as an HTML file
saveWidget(sites_map,normalizePath("outputs/maps/full_sites_interactive_map.html"), selfcontained = TRUE)

## 5.2) Static ----

##### 5.2.1) Pollen records + Regions + Biomes -----

# Define output file
png(normalizePath("outputs/maps/combined_maps.png"),  
    width = 25.5,  
    height = 15,  
    units = "cm",  
    res = 3000,  # High resolution
    pointsize = 13)  # Adjust text size for better readability


# Define the layout matrix
layout_matrix <- matrix(c(
  1, 2, 3,  # First row: 3 columns
  4, 5, 5, # Second row: 2 columns
  6, 7, 7  # Third row: 2 columns
), nrow = 3, byrow = TRUE)


layout(layout_matrix)  # Apply layout here

# Reduce margins to decrease space between plots
par(mar = c(3, 3, 2, 2), oma = c(0, 0, 0, 0))

### Plot 1: Fossil dated Records ###
plot(hs, col = gray(0:100 / 100), legend = FALSE, axes = TRUE)
plot(elevation_crop, col = terrain.colors(25), alpha = 0.5, legend = FALSE, axes = FALSE, add = TRUE)

points(sites$Longitude[sites$Dated == "Yes"],  
       sites$Latitude[sites$Dated == "Yes"],  
       col = "black",   # Outline color
       bg = "blue",    # Fill color
       pch = 21, cex = 1)  

mtext("(a)", side = 3, line = 1, at = -15, cex = 0.8)


### Plot 2: Fossil not dated Records ###
plot(hs, col = gray(0:100 / 100), legend = FALSE, axes = TRUE)
plot(elevation_crop, col = terrain.colors(25), alpha = 0.5, legend = FALSE, axes = FALSE, add = TRUE)
points(sites$Longitude[sites$Dated == "No"],  
       sites$Latitude[sites$Dated == "No"],  
       col = "black", bg = "red", pch = 21, cex = 1)  

mtext("(b)", side = 3, line = 1, at = -15, cex = 0.8)

### Plot 3: Modern Records ###
plot(hs, col = gray(0:100 / 100), legend = FALSE, axes = TRUE)
plot(elevation_crop, col = terrain.colors(25), alpha = 0.5, legend = TRUE, axes = FALSE, add = TRUE)

points(sites$Longitude[sites$Dated == "Modern"],  
       sites$Latitude[sites$Dated == "Modern"],  
       col = "black", bg = "green", pch = 21, cex = 1) 

mtext("(c)", side = 3, line = 1, at = -15, cex = 0.8)

### Plot 4: Phytogeographic Regions ###

# Define colors
n <- length(unique(merged_phytogeographic_regions$Region_Name))
colors_regions <- colorRampPalette(brewer.pal(12, "Set3"))(n)

plot(elevation_crop, col = terrain.colors(25), alpha = 0, legend = FALSE, axes = TRUE)
plot(st_geometry(merged_phytogeographic_regions), 
     col = colors_regions[as.numeric(merged_phytogeographic_regions$Region_Name)], 
     , border = "black", main = "", add = TRUE)

mtext("(d)", side = 3, line = 1, at = -15, cex = 0.8)

### Plot 5: Legend Phytogeographic Regions ###
par(mar = c(0, 0, 0, 0))  # Remove margins
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
legend("left", 
       legend = levels(merged_phytogeographic_regions$Region_Name),
       fill = colors_regions, 
       border = "black", 
       cex = 1, 
       title = NULL, 
       bty = "n",  
       xpd = TRUE,
       ncol=2)
mtext("Phytogeographic regions                                                                                         ", side = 3, line = -2, cex = 0.6, col = "black")


### Plot 6: Biomes ###
n <- length(unique(biomes$BIOME))
my_colors <- brewer.pal(min(n, 12), "Set3")  
biomes$col <- my_colors[as.integer(factor(biomes$BIOME))]

plot(elevation_crop, col = adjustcolor(terrain.colors(100), alpha.f = 0.5), legend = FALSE)  
plot(st_geometry(biomes), col = biomes$col, add = TRUE, border = "black")

mtext("(e)", side = 3, line = -1, at = -15, cex = 0.8)

### Plot 6: Legend biomes ###
par(mar = c(0, 0, 0, 0))  # Remove margins
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
legend("left", 
       legend = unique(biomes$BIOME_definition),  
       fill = my_colors,  
       border = "black", 
       cex = 1,  
       title = "Biomes", 
       bty = "n", 
       xpd = TRUE)

dev.off()



##### 5.2.2) Pollen records + Biomes -----
# Define output file
png(normalizePath("outputs/maps/combined_maps_2.png"),  
    width = 15,  
    height = 10,  
    units = "cm",  
    res = 3000,  # High resolution
    pointsize = 10)  

# Define a layout with 2 rows and 2 columns
layout(matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE), 
       widths = c(1),  
       heights = c(2, 2))  

# Reduce margins to decrease space between plots
par(mar = c(3, 3, 2, 2), oma = c(0, 0, 0, 0))

### Plot 1: Fossil Sites ###
plot(hs, col = gray(0:100 / 100), legend = FALSE, axes = TRUE)
plot(elevation_crop, col = terrain.colors(25), alpha = 0.5, legend = FALSE, axes = FALSE, add = TRUE)
points(fossil_sites$Longitude[fossil_sites$Dated == "Yes"],  
       fossil_sites$Latitude[fossil_sites$Dated == "Yes"],  
       col = "blue", pch = 19, cex = 0.5)  
points(fossil_sites$Longitude[fossil_sites$Dated == "No"],  
       fossil_sites$Latitude[fossil_sites$Dated == "No"],  
       col = "red", pch = 19, cex = 0.5)  
mtext("(a)", side = 3, line = -1, at = -15, cex = 1)  

### Plot 2: Modern Sites ###
plot(hs, col = gray(0:100 / 100), legend = FALSE, axes = TRUE)
plot(elevation_crop, col = terrain.colors(25), alpha = 0.5, legend = FALSE, axes = FALSE, add = TRUE)
points(modern_sites$Longitude,  
       modern_sites$Latitude,  
       col = "black", pch = 19, cex = 0.5)
mtext("(b)", side = 3, line = -1, at = -15, cex = 1)

### Plot 3: Biomes ###
n <- length(unique(biomes$BIOME))
my_colors <- brewer.pal(min(n, 12), "Set3")  
biomes$col <- my_colors[as.integer(factor(biomes$BIOME))]

plot(elevation_crop, col = adjustcolor(terrain.colors(100), alpha.f = 0.5), legend = FALSE)  
plot(st_geometry(biomes), col = biomes$col, add = TRUE, border = "black")

mtext("(c)", side = 3, line = -1, at = -15, cex = 1)

### Plot 4: Legends ###
par(mar = c(0, 0, 0, 0))  # Remove margins
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
legend("top", 
       legend = unique(biomes$BIOME_definition),  
       fill = my_colors,  
       border = "black", 
       cex = 0.8,  # Slightly larger text
       title = "Biomes", 
       bty = "n", 
       xpd = TRUE)

# Second legend (Site Categories)
legend("bottom", 
       legend = c("Dated fossil pollen", "Undated fossil pollen", "Modern pollen"), 
       fill = c("blue", "red", "black"), 
       border = "black", 
       cex = 0.8, 
       title = "Pollen records", 
       bty = "n", 
       xpd = TRUE)

# Close the PNG device
dev.off()

