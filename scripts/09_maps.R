## Script for data visualisation. Creation of maps of all the fossil and modern pollen records compiled in this database

#--------------------------------------------------------#
# 0. Load libraries and functions----
#--------------------------------------------------------#

source("scripts/functions.R")

libraries <-c("sf", "terra", "ggplot2", "dplyr", "readr", "leaflet", "htmlwidgets", "RColorBrewer","ggnewscale","patchwork","maptiles","ggspatial","rnaturalearth")


# Install missing packages
invisible(lapply(libraries, install_if_missing))

# Load the libraries
lapply(libraries, require, character.only = TRUE)

#--------------------------------------------------------#
# 1) Read data----
#--------------------------------------------------------#

## Regions (modified from Olson 2001)----
regions <- sf::st_read(normalizePath("data/raw_data/mapping_data/Regions_WWF_2017/Ecoregions_Europe_Arabian_Peninsula.shp"))
regions <- sf::st_make_valid(regions)
regions <- sf::st_crop(regions, ext(c(-19,62,-1, 55))) # crop regions
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
phytogeographic_regions_White$PHYTOCHO_1 <- as.factor(phytogeographic_regions_White$PHYTOCHO_1)


## Modified regions names to summarised regions:

# Mediterranean
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("VII. Mediterranean Region", "Mediterranean", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("XVIII. Mediterranean/Sahara Regional Transition Zone", "Mediterranean", phytogeographic_regions_White$PHYTOCHO_1)

# Sahara
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("XVII. Sahara Regional TransitionZone", "Sahara", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("XVII. Sahara Regional Transition Zone", "Sahara", phytogeographic_regions_White$PHYTOCHO_1)


# Sahel
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("XVI. Sahel Regional Transition Zone", "Sahel", phytogeographic_regions_White$PHYTOCHO_1)

# Eastern Tropical Africa
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("VIII. Afromontane Region", "Eastern Tropical Africa", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("VIII and IX. Afromontane and Afroalpine Regions", "Eastern Tropical Africa", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("XIII. Zanzibar-Inhambane Regional Mosaic", "Eastern Tropical Africa", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("Zanzibar-Inhambane Regional Mosaic", "Eastern Tropical Africa", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("IV. Somalia-Masai Region", "Eastern Tropical Africa", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("XV. Somalia-Masai Region", "Eastern Tropical Africa", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1[
  phytogeographic_regions_White$VEG == "16" &  
    is.na(phytogeographic_regions_White$PHYTOCHO_1)
] <- "Eastern Tropical Africa"

# Sudanian
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("III. Sudanian Region", "Sudanian", phytogeographic_regions_White$PHYTOCHO_1)

# Guineo-Congolian
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("I. Guineo-Congolian Region", "Guineo-Congolian", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("XI. Guinea-Congolia/Sudania Regional Transition Zone", "Guineo-Congolian", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("X. Guinea-Congolia/Zambezia Regional Transition Zone", "Guineo-Congolian", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("X. Guinca-Congolia/Zambczia Regional Transition Zone", "Guineo-Congolian", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1[
  phytogeographic_regions_White$VEG_2 == "A" & # veg A is Guineo-Congolian
    phytogeographic_regions_White$PHYTOCHO_1 == "XIX. East Malagasy Region"
] <- "Guineo-Congolian"


# Madagascar
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("XX. West Malagasy Region", "Madagascar", phytogeographic_regions_White$PHYTOCHO_1) 

phytogeographic_regions_White$PHYTOCHO_1 <- gsub("XIX. East Malagasy Region", "Madagascar", phytogeographic_regions_White$PHYTOCHO_1) 

phytogeographic_regions_White$PHYTOCHO_1[
  phytogeographic_regions_White$VEG == "11B" &  
    is.na(phytogeographic_regions_White$PHYTOCHO_1)
] <- "Madagascar"



# Zambezian
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("II. Zarnbezian Region", "Zambezian", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("II. Zambezian Region", "Zambezian", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("Il. Zambezian Region", "Zambezian", phytogeographic_regions_White$PHYTOCHO_1)


phytogeographic_regions_White$PHYTOCHO_1[
  phytogeographic_regions_White$VEG == "75" &  
    is.na(phytogeographic_regions_White$PHYTOCHO_1)
] <- "Zambezian"

phytogeographic_regions_White$PHYTOCHO_1[
  phytogeographic_regions_White$VEG == "76" &  
    is.na(phytogeographic_regions_White$PHYTOCHO_1)
] <- "Zambezian"


# South Africa
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("VI. Karoo-Namib Region", "South Africa", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("XV. Tongaland-Pondoland Regional Mosaic", "South Africa", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("XIV. Kalahari-Highveld Transition Zone", "South Africa", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("XIV. Kalahari-Highvcld Regional Transition Zone", "South Africa", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("XIV. Kalahari-Highveld Regional Transition Zone", "South Africa", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("V. Cape Region", "South Africa", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("VI. Karoo-Namib Region", "South Africa", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("XV. Tongaland-Pondoland Rcgional Mosaic", "South Africa", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1[
  phytogeographic_regions_White$VEG == "74" &  # veg 74 = South Africa
    is.na(phytogeographic_regions_White$PHYTOCHO_1)
] <- "South Africa"

phytogeographic_regions_White$PHYTOCHO_1 <- gsub("II. Zambezian Region (as enclaves)", "South Africa", phytogeographic_regions_White$PHYTOCHO_1)
phytogeographic_regions_White$PHYTOCHO_1 <- gsub("Zambezian \\(as enclaves\\)", "South Africa",
                                                 phytogeographic_regions_White$PHYTOCHO_1)

phytogeographic_regions_White$PHYTOCHO_1[
  phytogeographic_regions_White$PHYTOCHO_2 == "XIV. Kalahari-Highveld Regional Transition Zone" & 
    phytogeographic_regions_White$PHYTOCHO_1 == "II. Zambezian Region"
] <- "Guineo-Congolian"

##  Combine White and Olson's regions into one shapefile

# If the CRS are different, transform one to match the other
if (!identical(st_crs(regions), st_crs(phytogeographic_regions_White))) {
  # Transform phytogeographic_regions_White CRS to match regions CRS
  phytogeographic_regions_White <- st_transform(phytogeographic_regions_White, crs = st_crs(regions))
}

# Ensure both shapefiles have matching columns
colnames(phytogeographic_regions_White)[colnames(phytogeographic_regions_White) == "PHYTOCHO_1"] <- "Region_Name"
colnames(regions)[colnames(regions) == "Reg_names"] <- "Region_Name"

phytogeographic_regions_White <- phytogeographic_regions_White |> select(Region_Name,geometry)
regions <- regions |>  select(Region_Name,geometry)

#  Merge both shapefiles
merged_phytogeographic_regions <- rbind(regions, phytogeographic_regions_White)

# Save merged shapefile
# sf::st_write(merged_phytogeographic_regions,"data/processed_data/mapping_data/merged_phytogeographic_regions.shp")

##  Sites----
sites <- read_csv(normalizePath("metadata/pollen_data/database.csv"))
sites$Latitude <- as.numeric(sites$Latitude)
sites <- sites |> rename(Pollen = "Record type")

### Fossil sites
fossil_sites <- sites |> filter(Pollen=="Fossil")  |>  select(Site_name_machine_readable,Longitude, Latitude,Dated, `Link to database`)
fossil_sites <- na.omit(fossil_sites)

### Modern sites
modern_sites <- sites |> filter(Pollen=="Modern") |>  select(Site_name_machine_readable,Longitude, Latitude,Dated,`Link to database`)

## Elevation----
elevation <- rast(normalizePath("data/raw_data/mapping_data/elevation.tiff"))
elevation_crop_1 <- crop(elevation,ext(c(-20,60,-1,46))) # for study area maps
elevation_crop <- crop(elevation,ext(c(-26,61.5,-37, 46))) # for phyto map

### Calculate hillshade for further plotting
slopes <- terrain(elevation_crop, "slope", unit = "radians")
aspect <- terrain(elevation_crop, "aspect", unit = "radians")
hs <- shade(slopes, aspect) # base shade for elevation plotting

# Convert the raster to a data frame for plotting with ggplot2
elevation_df <- as.data.frame(elevation_crop, xy = TRUE)
colnames(elevation_df) <- c("x", "y", "value")

#--------------------------------------------------------#
# 2) Phytogeographical map -----
#--------------------------------------------------------#

# Merge subpolygons into one per region
merged_regions <- merged_phytogeographic_regions |> 
  group_by(Region_Name) |>    # group by the region name
  summarise(geometry = st_union(geometry))  # merge all polygons in each group

# Merge areas within the Sahara region into Saharan polygon

## Filter Sahara regions
sahara <- merged_regions %>%
  filter(Region_Name == "Sahara") 

## Merge all polygons into one (fills internal gaps)
sahara_filled <- st_union(sahara)

sahara_filled_sf <- st_sf(
  Region_Name = "Sahara",
  geometry = sahara_filled
)

sahara_filled_sf <- sahara_filled_sf %>%
  st_make_valid() %>%       # Fix invalid geometry
  st_buffer(0)              # Merge tiny gaps/holes

## Extract polygons as a list
polys <- st_cast(sahara_filled_sf$geometry, "POLYGON")

## Compute areas
areas <- st_area(polys)

## Keep only the largest polygon
largest_poly <- polys[which.max(areas)]

## Wrap back into sf as a single feature
sahara_cleaned <- st_sf(
  Region_Name = "Sahara",
  geometry = st_sfc(largest_poly)
)

## Add cleaned Saharan polygon to the other regions
merged_regions <- bind_rows(merged_regions, sahara_cleaned)


# Drop NA levels
merged_regions <- merged_regions %>%
  filter(!is.na(Region_Name))

# Also, reset factor levels to only existing regions
merged_regions$Region_Name <- factor(merged_regions$Region_Name)


# Get world countries
countries <- ne_countries(scale = "medium", returnclass = "sf")

# Define main colors
palette_main <- c(
  "Arabian Peninsula" = "darkorange1",
  "Sahara" = "lightgoldenrod1",
  "Sudanian" = "chartreuse2",
  "Sahel" = "goldenrod1",
  "Mediterranean Europe" = "olivedrab4",
  "Mediterranean" = "olivedrab3")


# Get all regions
all_regions <- unique(merged_regions$Region_Name)

# Already colored
colored_regions <- names(palette_main)

# Generate colorful palette for remaining regions
remaining_regions <- all_regions[!all_regions %in% colored_regions]

# Remaining regions
palette_remaining <- setNames(
  RColorBrewer::brewer.pal(n = length(remaining_regions), name = "Set3"),
  remaining_regions
)

# Final palette
palette_final <- c(palette_main, palette_remaining)

# Filter palette_final to only include regions actually in merged_regions
palette_final <- palette_final[names(palette_final) %in% merged_regions$Region_Name]

# Re-create color vector
col_vec <- palette_final[as.character(merged_regions$Region_Name)]

# Create legend labels with references
legend_labels <- ifelse(
  names(palette_final) %in% phytogeographic_regions_White$Region_Name,
  paste0(names(palette_final), " (White, 1983)"),
  paste0(names(palette_final), " (Olson et al., 2001)")
)

# Plot
png(
  normalizePath("outputs/maps/phytogeographical_regions_map.png"),
  width = 13,
  height = 8,
  units = "cm",
  res = 300,
  pointsize = 10
)

layout(matrix(1:2, nrow = 1, ncol = 2, byrow = TRUE), 
       widths = c(1.1, 0.6),  #  column widths
       heights = c(3, 2))  # Adjust heights to fit all plots


plot(elevation_crop, col = terrain.colors(25), alpha = 0, legend = FALSE, axes = TRUE)

plot(st_geometry(merged_regions),
     col = col_vec,
     border = NA,
     add = TRUE)

plot(st_geometry(countries),
     border = "black",
     lwd = 0.7,
     add = TRUE)

par(mar = c(0, 0, 0, 0))  # Remove margins
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
legend("left", 
       legend = legend_labels,
       fill = palette_final,
       border = "black", 
       cex = 0.6, 
       title = "Phytogeographic regions", 
       bty = "n",  
       xpd = TRUE)


dev.off()
#--------------------------------------------------------#
# 3) Study regions map ----
#--------------------------------------------------------#

merged_phytogeographic_regions$Region_Name <- as.character(merged_phytogeographic_regions$Region_Name)


# Replace "Mediterranean Europe"
merged_phytogeographic_regions$Region_Name[
  merged_phytogeographic_regions$Region_Name == "Mediterranean Europe"
] <- "Mediterranean"


merged_phytogeographic_regions$Region_Name <- as.factor(merged_phytogeographic_regions$Region_Name)

# Select only the regions to plot
regions_to_plot <- merged_phytogeographic_regions %>%
  filter(Region_Name %in% c("Mediterranean", "Sahara","Arabian Peninsula","Sahel ","Sudanian"))
regions_to_plot$Region_Name <- droplevels(regions_to_plot$Region_Name)

# Combine polygons by region
regions_combined <- regions_to_plot %>%
  group_by(Region_Name) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

plot(regions_combined$geometry, col = "white", border = "black")

# Filter Sahara regions
sahara <- regions_combined %>%
  filter(Region_Name == "Sahara") 

# Merge all polygons into one (fills internal gaps)
sahara_filled <- st_union(sahara)

sahara_filled_sf <- st_sf(
  Region_Name = "Sahara",
  geometry = sahara_filled
)

sahara_filled_sf <- sahara_filled_sf %>%
  st_make_valid() %>%       # Fix invalid geometry
  st_buffer(0)              # Merge tiny gaps/holes

# Extract polygons as a list
polys <- st_cast(sahara_filled_sf$geometry, "POLYGON")

# Compute areas
areas <- st_area(polys)

# Keep only the largest polygon
largest_poly <- polys[which.max(areas)]

# Wrap back into sf as a single feature
sahara_cleaned <- st_sf(
  Region_Name = "Sahara",
  geometry = st_sfc(largest_poly)
)

# Check 
plot(sahara_cleaned$geometry, col = "yellow", border = "black")

# Add cleand Saharan polygon to the other regions
regions_combined <- regions_combined %>%
  filter(Region_Name != "Sahara")

regions_combined <- bind_rows(regions_combined, sahara_cleaned)

plot(regions_combined$geometry, col = "yellow", border = "black")

# Convert regions to a raster mask
regions_vect <- vect(regions_combined)
regions_raster <- rasterize(regions_vect, hs, field = 1)

# Mask hs to only regions
hs_masked <- mask(hs, regions_raster)

plot(hs_masked)

# Define output file
png(normalizePath("outputs/maps/study_regions.png"),  
    width = 13,  
    height = 7,  
    units = "cm",  
    res = 3000,  # High resolution
    pointsize = 10)  

# Define a layout with 2 rows and 2 columns
layout(matrix(1:2, nrow = 1, ncol = 2, byrow = TRUE), 
       widths = c(2, 1),  # Increase the width of the first plot (larger space)
       heights = c(1))

# Define colours

regions_combined$Region_Name <- factor( #  Ensure Region_Name is a factor
  regions_combined$Region_Name,
  levels = c("Arabian Peninsula", "Mediterranean", "Sahara Regional Transition", "Sahel Regional Transition", "Sudanian Region")
)
regions_combined <- regions_combined[order(regions_combined$Region_Name), ]

color_palette <- leaflet::colorFactor(
  palette = c("darkorange1", "olivedrab", "lightgoldenrod1", "goldenrod1", "chartreuse2"),
  domain = regions_combined$Region_Name,
  ordered = TRUE
)

# Create colors for legend
legend_labels <- levels(regions_combined$Region_Name)
legend_colors <- color_palette(legend_labels)

# Plot 1: map
plot(elevation_crop_1, col = "lightgray", legend = FALSE, axes = TRUE)
plot(hs_masked, col = gray(0:100 / 100), legend = FALSE, axes = FALSE,add = TRUE)
plot(
  st_geometry(regions_combined),
  col = adjustcolor(legend_colors, alpha.f = 0.6), 
  border = "black",
  lwd = 0.5,       # thinner borders (default is 1)
  main = "",
  add = TRUE
)


# Plot 1: Legend Phytogeographic Regions
par(mar = c(0, 0, 0, 0))  # Remove margins
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
legend("left", 
       legend = levels(regions_combined$Region_Name),
       fill = legend_colors, 
       border = "black", 
       cex = 0.8, 
       title = "Study regions", 
       bty = "n",  
       xpd = TRUE)

dev.off()

#--------------------------------------------------------#
# 4) Study area map ----
#--------------------------------------------------------#
# Get extent
range(na.omit(sites$Latitude))
range(na.omit(sites$Longitude))

xmin <- as.numeric(-21.0262)
xmax <- as.numeric(60.8325)
ymin <- as.numeric(7.516667)
ymax <- as.numeric(43)


bbox_sf <- st_as_sfc(st_bbox(
  c(xmin = -21.0262, xmax = 60.8325, ymin = 7.516667, ymax = 43),
  crs = 4326  # <- WGS84 (lat/long)
))

# Download satellite map
sat_map <- get_tiles(bbox_sf, provider = "Esri.WorldImagery", zoom = 5)

map_study_area <- ggplot() +
  layer_spatial(sat_map) +        
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) + # north arrow
  coord_sf(crs = st_crs(4326),
           xlim = c(xmin, xmax),
           ylim = c(ymin, ymax),
           expand = FALSE) +  # flat map
  theme_minimal(base_size = 12) +   # increases all text a bit
  labs(
    title = "",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    axis.title = element_text(size = 12),               # axis titles bigger
    axis.text = element_text(size = 10),                # axis tick labels
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

map_study_area

# Save plot
ggsave(
  "outputs/maps/study_area.png",
  plot = map_study_area,
  width = 14, height = 7, dpi = 600, units = "cm"
)


#--------------------------------------------------------#
# 5) Fossil and modern sites ----
#--------------------------------------------------------#

## 5.1) Interactive ----
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

# Combine site data
fossil_dated <-  sites |> filter(Pollen=="Fossil",Dated == "Yes")  |>  select(Site_name_machine_readable,Longitude, Latitude, `Biogeographic area`)

fossil_undated <-  sites |> filter(Pollen=="Fossil",Dated == "No")  |>  select(Site_name_machine_readable,Longitude, Latitude, `Biogeographic area`,Dated)

modern <-  sites |> filter(Pollen=="Modern")  |>  select(Site_name_machine_readable,Longitude, Latitude, `Biogeographic area`)

# Define palette
palette <- c("darkorange1", "olivedrab", "lightgoldenrod1", "goldenrod1", "chartreuse2")

# Ensure the column name is correct
fossil_dated$`Biogeographic area` <- factor(fossil_dated$`Biogeographic area`)
fossil_undated$`Biogeographic area` <- factor(fossil_undated$`Biogeographic area`)
modern$`Biogeographic area` <- factor(modern$`Biogeographic area`)


# Download satellite map
bbox_sf <- st_as_sfc(st_bbox(
  c(xmin = -22, xmax = 62, ymin = 0, ymax = 46),
  crs = 4326  # <- wider area
))
sat_map <- get_tiles(bbox_sf, provider = "Esri.WorldImagery", zoom = 6)



# Plot
p_fossil_dated <- ggplot() +
  layer_spatial(sat_map) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) +
  geom_point(
    data = fossil_dated,
    aes(x = Longitude, y = Latitude, fill = `Biogeographic area`),
    shape = 21, color = "black", size = 3.1, stroke = 0.5
  ) +
  scale_fill_manual(values = palette, name = "Biogeographic area") +
  coord_sf(
    xlim = c(-22, 62),
    ylim = c(2, 45),
    expand = FALSE
  ) +
  labs(
    title = "(a)",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16)
  )


p_fossil_undated <-ggplot() +
  layer_spatial(sat_map) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) +
  geom_point(
    data = fossil_undated,
    aes(x = Longitude, y = Latitude, fill = `Biogeographic area`),
    shape = 21, color = "black",size = 3.1, stroke = 0.5
  ) +
  scale_fill_manual(values = palette, name = "Biogeographic area") +
  coord_sf(
    xlim = c(-22, 62),
    ylim = c(2, 45),
    expand = FALSE
  ) +
  labs(
    title = "(b)",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16)
  )

p_modern <- ggplot() +
  layer_spatial(sat_map) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) +
  geom_point(
    data = modern,
    aes(x = Longitude, y = Latitude, fill = `Biogeographic area`),
    shape = 21, color = "black", size = 3.1, stroke = 0.5
  ) +
  scale_fill_manual(values = palette, name = "Biogeographic area") +
  coord_sf(
    xlim = c(-22, 62),
    ylim = c(2, 45),
    expand = FALSE
  ) +
  labs(
    title = "(c)",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 16) +
   theme(
    legend.position = "right",
    legend.title = element_text(size = 16, face = "bold"),   # bigger legend title
    legend.text  = element_text(size = 18),                  # bigger legend labels
    legend.key.size = unit(0.8, "cm"),                       # bigger symbol boxes
    legend.spacing.y = unit(0.3, "cm"),                      # vertical space between items
    plot.title = element_text(face = "bold", hjust = 0)
  )

# Combine plots
combined_plot <- (
  p_fossil_dated / 
  p_fossil_undated /  # top row
  p_modern                              # bottom row
) +
  plot_layout(heights = c(1, 1, 1), guides = "collect")

# Save combined plot
ggsave(
  "outputs/maps/site_maps.png",
  plot = combined_plot,
  width = 14, height = 20, dpi = 600
)


#--------------------------------------------------------#

## 5.3) Static v2----

# 1. Unify data into a single df
sites_clean <- sites |> 
  mutate(
    Record_type = case_when(
      Pollen == "Modern" ~ "Modern",
      Pollen == "Fossil" & Dated == "Yes" ~ "Fossil (dated)",
      Pollen == "Fossil" & Dated == "No" ~ "Fossil (undated)"
    )
  ) |> 
  filter(!is.na(Longitude), !is.na(Latitude))

sites_clean <- sites_clean |>  filter(!is.na(Record_type))

sites_clean$Record_type <- factor(
  sites_clean$Record_type,
  levels = c("Modern", "Fossil (dated)", "Fossil (undated)")
)

# 2. Define point symbols
shape_values <- c(
  "Modern" = 21,           # circle
  "Fossil (dated)" = 24,   # triangle
  "Fossil (undated)" = 22  # square
)

fill_values <- c(
  "Modern" = "#56B4E9",
  "Fossil (dated)" = "#009E73",
  "Fossil (undated)" = "#F0E442"
)


# 3. Create map with all records
p_overview <- plot_sector(
  sites_clean,
  xmin = -22, xmax = 62,
  ymin = 2, ymax = 45,
  zoom_level = 5,
  title_label = "(a) All records"
) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) 

ggsave(
  "outputs/maps/all_records.png",
  p_overview,
  width = 14,
  height = 10,
  dpi = 600
)


# 4. Create A.Peninsula
p_arabian <- plot_sector(
  sites_clean |> filter(`Biogeographic area` == "Arabian Peninsula"),
  xmin = 32, xmax = 62,
  ymin = 12, ymax = 38,
  zoom_level = 5,
  title_label = "(b) Arabian Peninsula"
) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) #+ 
 # theme(legend.position = "right")

ggsave(
  "outputs/maps/arabian_records.png",
  p_arabian,
  width = 10,
  height = 6,
  dpi = 600
)

# 5. Create Mediterranean
p_medit <- plot_sector(
  sites_clean |> filter(`Biogeographic area` == "Mediterranean"),
  xmin = -21, xmax = 45,
  ymin = 27, ymax = 45,
  zoom_level = 5,
  title_label = "(c) Mediterranean"
)+
  annotation_north_arrow(
    location = "tr", which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) 

ggsave(
  "outputs/maps/mediterranean_records.png",
  p_medit,
  width = 9,
  height = 10,
  dpi = 600
)

# 6. Create Saharan
p_saharan <- plot_sector(
  sites_clean |> filter(`Biogeographic area` == "Sahara"),
  xmin = -22, xmax = 41,
  ymin = 13, ymax = 33,
  zoom_level = 6,
  title_label = "(d) Sahara"
)+
  annotation_north_arrow(
    location = "tr", which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) 

ggsave(
  "outputs/maps/saharan_records.png",
  p_saharan,
  width = 9,
  height = 10,
  dpi = 600
)

# 7. Create Sahel
p_sahel <- plot_sector(
  sites_clean |> filter(`Biogeographic area` == "Sahel"),
  xmin = -22, xmax = 41,
  ymin = 8, ymax = 22,
  zoom_level = 6,
  title_label = "(e) Sahel"
)+
  annotation_north_arrow(
    location = "tr", which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) 

ggsave(
  "outputs/maps/sahel_records.png",
  p_sahel,
  width = 9,
  height = 10,
  dpi = 600
)

# 8. Create Sudanian
p_sudan <- plot_sector(
  sites_clean |> filter(`Biogeographic area` == "Sudanian"),
  xmin = -22, xmax = 52,
  ymin = -1, ymax = 18,
  zoom_level = 5,
  title_label = "(f) Sudanian"
)+
  annotation_north_arrow(
    location = "tr", which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) 

ggsave(
  "outputs/maps/sudan_records.png",
  p_sudan,
  width = 9,
  height = 10,
  dpi = 600
)


