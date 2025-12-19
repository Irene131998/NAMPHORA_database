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

phytogeographic_regions_White$PHYTOCHO_1 <- gsub("Sahara Regional Transition Zone", "Sahara Regional Transition", phytogeographic_regions_White$PHYTOCHO_1)

phytogeographic_regions_White$PHYTOCHO_1 <- gsub("Sahel Regional Transition Zone", "Sahel Regional Transition", phytogeographic_regions_White$PHYTOCHO_1)

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

# Save merged shapefile
#st_write(merged_phytogeographic_regions, "data/procesed_data/mapping_data/merged_phytogeographic_regions.shp")

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
elevation_crop_1 <- crop(elevation,ext(c(-20,60,7,44))) # for study area map
elevation_crop <- crop(elevation,ext(c(-26,61.5,5,44)))

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
  filter(Region_Name == "Sahara Regional Transition") 

## Merge all polygons into one (fills internal gaps)
sahara_filled <- st_union(sahara)

sahara_filled_sf <- st_sf(
  Region_Name = "Sahara Regional Transition",
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
  Region_Name = "Sahara Regional Transition",
  geometry = st_sfc(largest_poly)
)

## Add cleand Saharan polygon to the other regions
merged_regions <- merged_regions %>%
  filter(Region_Name != "Sahara Regional Transition")

merged_regions <- bind_rows(merged_regions, sahara_cleaned)

## Make sure Region_Name is a factor
merged_regions$Region_Name <- factor(merged_regions$Region_Name)

# Get world countries
countries <- ne_countries(scale = "medium", returnclass = "sf")

# Plot
png(
  normalizePath("outputs/maps/phytogeographical_regions_map.png"),
  width = 14,       # width in cm
  height = 8,       # height in cm
  units = "cm",
  res = 300,        # resolution in dpi
  pointsize = 12    # adjust text size
)

layout(matrix(1:2, nrow = 1, ncol = 2, byrow = TRUE), 
       widths = c(1.1, 0.6),  #  column widths
       heights = c(3, 2))  # Adjust heights to fit all plots

### Plot 1: Phytogeographic Regions ###

# Define colors
palette_main <- c(
  "Arabian Peninsula" = "darkorange1",
  "Sahara Regional Transition" = "lightgoldenrod1",
  "Sudanian Region" = "chartreuse2",
  "Sahel Regional Transition" = "goldenrod1"
)

# Mediterranean sub-regions
palette_medit <- c(
  "Mediterranean Europe" = "olivedrab3",
  "Mediterranean Africa" = "olivedrab2",
  "Mediterranean/Sahara Transition" = "olivedrab4"
)

# Get all regions
all_regions <- unique(merged_regions$Region_Name)

# Combine the ones already colored
colored_regions <- c(names(palette_main), names(palette_medit))

# Remaining regions get shades of grey
palette_grey <- setNames(
  gray.colors(length(all_regions[!all_regions %in% colored_regions]), start = 0.8, end = 0.4),
  all_regions[!all_regions %in% colored_regions]
)

# Final palette
palette_final <- c(palette_main, palette_medit, palette_grey)
col_vec <- palette_final[as.character(merged_regions$Region_Name)]

plot(elevation_crop, col = terrain.colors(25), alpha = 0, legend = FALSE, axes = TRUE)
plot(st_geometry(merged_regions),
     col = col_vec,  
     border = NA,
     main = "",
     add = TRUE)

# Draw country borders on top
plot(st_geometry(countries), 
     border = "black", 
     lwd = 0.7,       # line width of country borders
     add = TRUE)

### Plot 2: Legend Phytogeographic Regions ###
par(mar = c(0, 0, 0, 0))  # Remove margins
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
legend("left", 
       legend = names(palette_final),
       fill = palette_final,
       border = "black", 
       cex = 0.5, 
       title = "Phytogeographic regions", 
       bty = "n",  
       xpd = TRUE)

dev.off()

#--------------------------------------------------------#
# 3) Biomes map----
#--------------------------------------------------------#

png(normalizePath("outputs/maps/biomes_map.png"),  
    width = 35,  
    height = 12,  
    units = "cm",  
    res = 2700,  # High resolution
    pointsize = 15)  # Adjust text size for better readability

layout(matrix(1:2, nrow = 1, ncol = 2, byrow = TRUE), 
       widths = c(1.1, 0.6),  #  column widths
       heights = c(2.2, 2))  # Adjust heights to fit all plots

### Plot 1: Biomes mao
n <- length(unique(biomes$BIOME))
my_colors <- brewer.pal(min(n, 12), "Paired")  
biomes$col <- my_colors[as.integer(factor(biomes$BIOME))]

plot(elevation_crop, col = adjustcolor(terrain.colors(100), alpha.f = 0.5), legend = FALSE)  
plot(st_geometry(biomes), col = biomes$col, add = TRUE, border = "black")


### Plot 2: Legend biomes ###
par(mar = c(0, 0, 0, 0))  # Remove margins
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
legend("left", 
       legend = unique(biomes$BIOME_definition),  
       fill = my_colors,  
       border = "black", 
       cex = 0.8,  
       title = "Biomes", 
       bty = "n", 
       xpd = TRUE)
dev.off()


#--------------------------------------------------------#
# 4) Combined map (biomes + phytogeographic Regions) ----
#--------------------------------------------------------#


# Define the layout matrix
layout_matrix <- matrix(c(
  1, 2,
  3, 4), nrow = 2, byrow = TRUE)

layout(layout_matrix)  # Apply layout here

# Reduce margins to decrease space between plots
par(mar = c(3, 3, 2, 2), oma = c(0, 0, 0, 0))


### Plot 1: Phytogeographic Regions ###

# Define colors
n <- length(unique(merged_phytogeographic_regions$Region_Name))
colors_regions <- colorRampPalette(brewer.pal(12, "Paired"))(n)

plot(elevation_crop, col = terrain.colors(25), alpha = 0, legend = FALSE, axes = TRUE)
plot(st_geometry(merged_phytogeographic_regions), 
     col = colors_regions[as.numeric(merged_phytogeographic_regions$Region_Name)], 
     , border = "black", main = "", add = TRUE)

mtext("(a)", side = 3, line = 1, at = -15, cex = 0.8)

### Plot 2: Legend Phytogeographic Regions ###
par(mar = c(0, 0, 0, 0))  # Remove margins
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
legend("left", 
       legend = levels(merged_phytogeographic_regions$Region_Name),
       fill = colors_regions, 
       border = "black", 
       cex = 0.7, 
       title = NULL, 
       bty = "n",  
       xpd = TRUE)
mtext("Phytogeographic regions                                                                   ", side = 3, line = -1, cex = 0.6, col = "black")



### Plot 3: Biomes ###
n <- length(unique(biomes$BIOME))
my_colors <- brewer.pal(min(n, 12), "Paired")  # Ensure no errors for >12 colors
biomes$col <- my_colors[as.integer(factor(biomes$BIOME))]

plot(elevation_crop, col = adjustcolor(terrain.colors(100), alpha.f = 0.5), legend = FALSE)  
plot(st_geometry(biomes), col = biomes$col, add = TRUE, border = "black")

mtext("(b)", side = 3, line = -1, at = -15, cex = 0.8)

### Plot 4: Legend biomes ###
par(mar = c(0, 0, 0, 0))  # Remove margins
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
legend("left", 
       legend = unique(biomes$BIOME_definition),  
       fill = my_colors,  
       border = "black", 
       cex = 0.7,  
       title = "Biomes", 
       bty = "n", 
       xpd = TRUE)

dev.off()

#--------------------------------------------------------#
# 5) Study regions map ----
#--------------------------------------------------------#

merged_phytogeographic_regions$Region_Name <- as.character(merged_phytogeographic_regions$Region_Name)


# Replace "Mediterranean/Sahara Transition"
merged_phytogeographic_regions$Region_Name[
  merged_phytogeographic_regions$Region_Name == "Mediterranean/Sahara Transition"
] <- "Mediterranean"

# Replace "Mediterranean Europe"
merged_phytogeographic_regions$Region_Name[
  merged_phytogeographic_regions$Region_Name == "Mediterranean Europe"
] <- "Mediterranean"

# Replace "Mediterranean Africa"
merged_phytogeographic_regions$Region_Name[
  merged_phytogeographic_regions$Region_Name == "Mediterranean Africa"
] <- "Mediterranean"

merged_phytogeographic_regions$Region_Name <- as.factor(merged_phytogeographic_regions$Region_Name)

# Select only the regions to plot
regions_to_plot <- merged_phytogeographic_regions %>%
  filter(Region_Name %in% c("Mediterranean", "Sahara Regional Transition","Arabian Peninsula","Sahel Regional Transition","Sudanian Region"))
regions_to_plot$Region_Name <- droplevels(regions_to_plot$Region_Name)

# Combine polygons by region
regions_combined <- regions_to_plot %>%
  group_by(Region_Name) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

plot(regions_combined$geometry, col = "white", border = "black")

# Filter Sahara regions
sahara <- regions_combined %>%
  filter(Region_Name == "Sahara Regional Transition") 

# Merge all polygons into one (fills internal gaps)
sahara_filled <- st_union(sahara)

sahara_filled_sf <- st_sf(
  Region_Name = "Sahara Regional Transition",
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
  Region_Name = "Sahara Regional Transition",
  geometry = st_sfc(largest_poly)
)

# Check 
plot(sahara_cleaned$geometry, col = "yellow", border = "black")

# Add cleand Saharan polygon to the other regions
regions_combined <- regions_combined %>%
  filter(Region_Name != "Sahara Regional Transition")

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
# 6) Study area map ----
#--------------------------------------------------------#
# Get extent
range(na.omit(sites$Latitude))
range(na.omit(sites$Longitude))

xmin <- as.numeric(-21.0262)
xmax <- as.numeric(60.8325)
ymin <- as.numeric(7.516667)
ymax <- as.numeric(40.963610)


bbox_sf <- st_as_sfc(st_bbox(
  c(xmin = -21.0262, xmax = 60.8325, ymin = 7.516667, ymax = 40.963610),
  crs = 4326  # <- WGS84 (lat/long)
))

# Download satellite map
sat_map <- get_tiles(bbox_sf, provider = "Esri.WorldImagery", zoom = 4)

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
    axis.text = element_text(size = 10),                               # axis tick labels
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
# 7) Fossil and modern sites ----
#--------------------------------------------------------#

## 7.1.) Interactive ----
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

## 7.2) Static ----

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
  c(xmin = -22, xmax = 62, ymin = 7, ymax = 42),
  crs = 4326  # <- wider area
))
sat_map <- get_tiles(bbox_sf, provider = "Esri.WorldImagery", zoom = 5)


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
    shape = 21, color = "black", size = 3, stroke = 0.8
  ) +
  scale_fill_manual(values = palette, name = "Biogeographic area") +
  coord_sf(
    xlim = c(-22, 62),
    ylim = c(7, 42),
    expand = FALSE
  ) +
  labs(
    title = "(a)",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
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
    shape = 21, color = "black", size = 3, stroke = 0.8
  ) +
  scale_fill_manual(values = palette, name = "Biogeographic area") +
  coord_sf(
    xlim = c(-22, 62),
    ylim = c(7, 42),
    expand = FALSE
  ) +
  labs(
    title = "(b)",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
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
    shape = 21, color = "black", size = 3, stroke = 0.8
  ) +
  scale_fill_manual(values = palette, name = "Biogeographic area") +
  coord_sf(
    xlim = c(-22, 62),
    ylim = c(7, 42),
    expand = FALSE
  ) +
  labs(
    title = "(c)",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 12) +
   theme(
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),   # bigger legend title
    legend.text  = element_text(size = 12),                  # bigger legend labels
    legend.key.size = unit(0.8, "cm"),                       # bigger symbol boxes
    legend.spacing.y = unit(0.3, "cm"),                      # vertical space between items
    plot.title = element_text(face = "bold", hjust = 0.25)
  )

# Combine plots
combined_plot <- (
  (p_fossil_dated + p_fossil_undated) /  # top row
    p_modern                              # bottom row
) +
  plot_layout(heights = c(1, 1), guides = "collect", widths = c(4, 1)) 

# Save combined plot
ggsave(
  "outputs/maps/site_maps.png",
  plot = combined_plot,
  width = 14, height = 7, dpi = 600
)


#--------------------------------------------------------#