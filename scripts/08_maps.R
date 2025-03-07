## Script for data visualisation. Creation of maps of all the fossil and modern pollen records compiled in this database
  
# 0. Load libraries and functions----

source("functions.R")

libraries <-c("sf", "terra", "ggplot2", "dplyr", "readr", "leaflet", "htmlwidgets", "RColorBrewer")


# Install missing packages
invisible(lapply(libraries, install_if_missing))

# Load the libraries
lapply(libraries, require, character.only = TRUE)

# 1) Read data----

# Phytogeographic_regions
Phytogeographic_regions <- sf::st_read(normalizePath("../data/raw_data/mapping_data/EcoregionsWWF_2017/ecoregionsWWF_2017.shp"))
Phytogeographic_regions_crop <- sf::st_crop(Phytogeographic_regions, ext(c(-19,70,0,40)))
Phytogeographic_regions_crop$ECO_NAME <- as.factor(Phytogeographic_regions_crop$ECO_NAME)

# Sites
sites <- read_csv(normalizePath("../metadata/pollen_data/database.csv"))
sites$Latitude <- as.numeric(sites$Latitude)

## Fossil sites
fossil_sites <- sites |> filter(Pollen=="Fossil")  |>  select(Site_name_machine_readable,Longitude, Latitude,Dated, `Link to database`)
fossil_sites <- na.omit(fossil_sites)

## Modern sites
modern_sites <- sites |> filter(Pollen=="Modern") |>  select(Site_name_machine_readable,Longitude, Latitude,Dated,`Link to database`)

# Elevation
elevation <- rast(normalizePath("../data/raw_data/mapping_data/elevation.tiff"))
elevation_crop <- crop(elevation,ext(c(-19,70,0,50)))

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
saveWidget(fossil_sites_map,normalizePath("../outputs/maps/fossil_sites_interactive_map.html"), selfcontained = TRUE)


## 2.2) Static map (ggplot)----

fossil_sites_plot <- ggplot() +
  geom_raster(data = elevation_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colors = terrain.colors(100)) +  # Terrain color scale
  geom_sf(data = Phytogeographic_regions_crop, aes(geometry = geometry), fill = NA, color = "black", lwd = 0.5) +
  geom_point(data = fossil_sites, mapping = aes(x = Longitude, y = Latitude, color = as.factor(Dated)), size = 2) +
  theme_minimal() +
  labs(fill = "Elevation (m)", color = "Dated") +
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(fossil_sites$Dated)), "Set1"))


ggsave(normalizePath("../outputs/maps/fossil_sites_ggplot.png"), fossil_sites_plot, width = 17, height = 10, dpi = 300)

fossil_sites_plot


## 2.3) Static map (base R)----

png(normalizePath("../outputs/maps/fossil_sites_plot.png"), width = 1600, height = 1000, res = 150)

# Plot the raster
plot(elevation_crop, main = "Fossil Pollen Records", col = terrain.colors(100))  

# Overlay the shapefile
plot(sf::st_geometry(Phytogeographic_regions), add = TRUE, col = "transparent", border = "black")

# Add the points
points(fossil_sites$Longitude[fossil_sites$Dated == "Yes"],  
       fossil_sites$Latitude[fossil_sites$Dated == "Yes"],  
       col = "blue", pch = 19, cex = 0.9)  

points(fossil_sites$Longitude[fossil_sites$Dated == "No"],  
       fossil_sites$Latitude[fossil_sites$Dated == "No"],  
       col = "red", pch = 19, cex = 0.9)  

# Improved legend placement
legend("bottomright", 
       legend = c("Dated", "Not dated"), 
       col = c("blue", "red"), 
       pch = 19, 
       cex = 1) 

dev.off()


# 3) Plot modern pollen records Interactive map----

## 3.1) Interactive map----

# Create a color palette based on the "Dated" column
color_palette <- colorFactor(palette = "red", domain = modern_sites$Dated)

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
saveWidget(modern_sites_map,normalizePath("../outputs/maps/modern_sites_interactive_map.html"), selfcontained = TRUE)


## 3.2) Static map (ggplot)----

modern_sites_plot <- ggplot() +
  geom_raster(data = elevation_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colors = terrain.colors(100)) +  # Terrain color scale
  geom_sf(data = Phytogeographic_regions_crop, aes(geometry = geometry), fill = NA, color = "black", lwd = 0.5) +
  geom_point(data = modern_sites, mapping = aes(x = Longitude, y = Latitude, color = as.factor(Dated)), size = 2) +
  theme_minimal() +
  labs(fill = "Elevation (m)", color = "Dated") +
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(fossil_sites$Dated)), "Set1"))


ggsave(normalizePath("../outputs/maps/modern_sites_ggplot.png"), modern_sites_plot, width = 17, height = 10, dpi = 300)

modern_sites_plot

## 3.3) Static map (base R)----

png(normalizePath("../outputs/maps/modern_sites_plot.png"), width = 1600, height = 1000, res = 150)

# Plot the raster
plot(elevation_crop, main = "Modern Pollen Records", col = terrain.colors(100))  

# Overlay the shapefile
plot(sf::st_geometry(Phytogeographic_regions), add = TRUE, col = "transparent", border = "black")

# Add the points
points(modern_sites$Longitude,  
       modern_sites$Latitude,  
       col = "red", pch = 19, cex = 0.9)  

# Improved legend placement
legend("bottomright", 
       legend = "Modern sites", 
       col = "red", 
       pch = 19, 
       cex = 1) 

dev.off()

# 4) Interactive combined map (fossil + modern) ----
# Eliminate na rows
sites <- sites |>
  filter(!is.na(Dated))

# Create a color palette based on the "Dated" column
color_palette <- colorFactor(palette =  c("red", "blue","green"), domain = sites$Dated)

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
saveWidget(sites_map,normalizePath("../outputs/maps/full_sites_interactive_map.html"), selfcontained = TRUE)
