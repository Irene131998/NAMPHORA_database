## Script for data visualisation. Creation of maps of all the fossil and modern pollen records compiled in this database
  
# 0. Load libraries and functions----

source("scripts/functions.R")

libraries <-c("sf", "terra", "ggplot2", "dplyr", "readr", "leaflet", "htmlwidgets", "RColorBrewer")


# Install missing packages
invisible(lapply(libraries, install_if_missing))

# Load the libraries
lapply(libraries, require, character.only = TRUE)

# 1) Read data----

# Phytogeographic_regions
Phytogeographic_regions <- sf::st_read(normalizePath("data/raw_data/mapping_data/EcoregionsWWF_2017/wwf_terr_ecos.shp"))
Phytogeographic_regions <- sf::st_make_valid(Phytogeographic_regions)

Phytogeographic_regions_crop <- sf::st_crop(Phytogeographic_regions, ext(c(-19,70,0,50)))
Phytogeographic_regions_crop$ECO_NAME <- as.factor(Phytogeographic_regions_crop$ECO_NAME)
Phytogeographic_regions_crop$BIOME <- as.factor(Phytogeographic_regions_crop$BIOME)

biome_definitions <- read.csv(normalizePath("data/raw_data/mapping_data/EcoregionsWWF_2017/Biome_definitions.csv"))
colnames(biome_definitions) <- c("BIOME","BIOME_definition")

# Merge the biome definitions with the shapefile 
Phytogeographic_regions_crop <- merge(Phytogeographic_regions_crop, biome_definitions, by = "BIOME", all.x = TRUE)

plot(sf::st_geometry(Phytogeographic_regions_crop), col = Phytogeographic_regions_crop$BIOME, border = "black")

# Filter out rows where BIOME is 98 and BIOME_definition is NA or empty
Phytogeographic_regions_crop <- Phytogeographic_regions_crop[!(Phytogeographic_regions_crop$BIOME == 98 &  (is.na(Phytogeographic_regions_crop$BIOME_definition) | Phytogeographic_regions_crop$BIOME_definition == "")), ]


# Sites
sites <- read_csv(normalizePath("metadata/pollen_data/database.csv"))
sites$Latitude <- as.numeric(sites$Latitude)

## Fossil sites
fossil_sites <- sites |> filter(Pollen=="Fossil")  |>  select(Site_name_machine_readable,Longitude, Latitude,Dated, `Link to database`)
fossil_sites <- na.omit(fossil_sites)

## Modern sites
modern_sites <- sites |> filter(Pollen=="Modern") |>  select(Site_name_machine_readable,Longitude, Latitude,Dated,`Link to database`)

# Elevation
elevation <- rast(normalizePath("data/raw_data/mapping_data/elevation.tiff"))
elevation_crop <- crop(elevation,ext(c(-19,70,0,50)))

## Calculate hillshade for further plotting
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
  geom_sf(data = Phytogeographic_regions_crop, aes(geometry = geometry), fill = NA, color = "black", lwd = 0.5) +
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
  geom_sf(data = Phytogeographic_regions_crop, aes(geometry = geometry), fill = NA, color = "black", lwd = 0.5) +
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
#plot(sf::st_geometry(Phytogeographic_regions), add = TRUE, col = "transparent", border = "black")

# Add the points
points(modern_sites$Longitude,  
       modern_sites$Latitude,  
       col = "black", pch = 19, cex = 0.4)  

dev.off()

# 4) Phytogeographical map -----

png(normalizePath("outputs/maps/phytogeographical_regions_map.png"), width = 1600, height = 1800, res = 300)

# Number of unique BIOME categories
n <- length(unique(Phytogeographic_regions_crop$BIOME))

# Generate a color palette with enough colors for the categories
my_colors <- brewer.pal(n, "Set3")  

# Assign the colors to each BIOME category correctly
# Ensure that the levels of the factor align with the colors
Phytogeographic_regions_crop$col <- my_colors[as.factor(Phytogeographic_regions_crop$BIOME)]

# Plot the data with the assigned colors
plot(elevation_crop, col = adjustcolor(terrain.colors(100), alpha.f = 0.5), legend = FALSE)  # Base raster layer

plot(sf::st_geometry(Phytogeographic_regions_crop), 
     col = Phytogeographic_regions_crop$col, 
     add = TRUE,
     border = "black")
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
saveWidget(sites_map,normalizePath("outputs/maps/full_sites_interactive_map.html"), selfcontained = TRUE)

## 5.2) Static ----

# Define output file
png(normalizePath("outputs/maps/combined_maps.png"),  
    width = 15,  
    height = 10,  
    units = "cm",  
    res = 3000,  # High resolution
    pointsize = 10)  # Adjust text size for better readability

# Define a layout with 2 rows and 2 columns
layout(matrix(1:4, nrow = 2, byrow = TRUE), 
       widths = c(2, 2),      
       heights = c(2, 2))  # Increase first row, shrink legend row

# Reduce margins to decrease space between plots
par(mar = c(2, 2, 2, 2),  # Smaller plot margins
    oma = c(0, 0, 0, 0))  # Remove outer margins

### Plot 1: Fossil Sites ###
plot(hs, col = gray(0:100 / 100), legend = FALSE, axes = TRUE)
plot(elevation_crop, col = terrain.colors(25), alpha = 0.5, legend = FALSE, axes = FALSE, add = TRUE)
points(fossil_sites$Longitude[fossil_sites$Dated == "Yes"],  
       fossil_sites$Latitude[fossil_sites$Dated == "Yes"],  
       col = "blue", pch = 19, cex = 0.5)  
points(fossil_sites$Longitude[fossil_sites$Dated == "No"],  
       fossil_sites$Latitude[fossil_sites$Dated == "No"],  
       col = "red", pch = 19, cex = 0.5)  
mtext("(a)", side = 3, line = -1, at = -15, cex = 1)  # Adjust label closer

### Plot 2: Modern Sites ###
plot(hs, col = gray(0:100 / 100), legend = FALSE, axes = TRUE)
plot(elevation_crop, col = terrain.colors(25), alpha = 0.5, legend = FALSE, axes = FALSE, add = TRUE)
points(modern_sites$Longitude,  
       modern_sites$Latitude,  
       col = "black", pch = 19, cex = 0.5)
mtext("(b)", side = 3, line = -1, at = -15, cex = 1)

### Plot 3: Phytogeographic Regions ###
n <- length(unique(Phytogeographic_regions_crop$BIOME))
my_colors <- brewer.pal(n, "Set3")  
Phytogeographic_regions_crop$col <- my_colors[as.factor(Phytogeographic_regions_crop$BIOME)]

plot(elevation_crop, col = adjustcolor(terrain.colors(100), alpha.f = 0.5), legend = FALSE)  
plot(sf::st_geometry(Phytogeographic_regions_crop), 
     col = Phytogeographic_regions_crop$col, 
     add = TRUE,
     border = "black")
mtext("(c)", side = 3, line = -1, at = -15, cex = 1)

### Plot 4: Legends ###
par(mar = c(0, 0, 0, 0))  # Remove margins
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
legend("top", 
       legend = unique(Phytogeographic_regions_crop$BIOME_definition),  
       fill = my_colors,  
       border = "black", 
       cex = 0.8,  # Slightly larger text
       title = "Biomes", 
       bty = "n", 
       xpd = TRUE)

# Second legend (Site Categories)
legend("bottom", 
       legend = c("Dated", "Not dated", "Modern"), 
       fill = c("blue", "red", "black"), 
       border = "black", 
       cex = 0.8, 
       title = "Site categories", 
       bty = "n", 
       xpd = TRUE)

# Close the PNG device
dev.off()

