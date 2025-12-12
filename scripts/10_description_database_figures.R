## Script for data visualisation. Creation of graphs that describe the database

#--------------------------------------------------------#
# 0) Load libraries and functions----
#--------------------------------------------------------#

source("scripts/functions.R")

libraries <- c("dplyr","readr","tidyr","ggplot2","stringr","tibble")

# Install missing packages
#invisible(lapply(libraries, install_if_missing))

# Load the libraries
lapply(libraries, require, character.only = TRUE)

#--------------------------------------------------------#
# 1) Read data----
#--------------------------------------------------------#

sites <- read_csv(normalizePath("metadata/pollen_data/database.csv"))

sites <- sites |> rename(Pollen = "Record type")

taxonomy <- read_csv(normalizePath("data/processed_data/taxonomy/harmonised_taxonomy_list.csv"))
phyto_aff <- read_csv(normalizePath("data/processed_data/taxonomy/phytogeographic_affinity_list_columns.csv"))
pft <- read_csv(normalizePath("data/processed_data/plant_functional_types/total_pfts.csv"))


# Colour palette (colour-blind friendly).
palette_1 <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999",  "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5",  "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F", "#1F78B4", "#33A02C", "#FB9A99", "#E31A1C",  "#FDBF6F", "#CAB2D6", "#6A3D9A", "#FF7F00")

palette_2 <- c(
  "#E31A1C", "#56B4E9", "#F0E442", "#E69F00", "#009E73", "#D55E00", "#CC79A7", "#999999",
  "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5",
  "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F", "#1F78B4", "#33A02C", "#FB9A99", "#0072B2",
  "#FDBF6F", "#CAB2D6", "#6A3D9A", "#FF7F00")


palette_3 <- c("darkorange1", "olivedrab", "lightgoldenrod1", "goldenrod1", "chartreuse2") # FOR REGIONS

#--------------------------------------------------------#
# 2) Number sites per bigeographic region----
#--------------------------------------------------------#

# Total sites
sites_biogeo <- sites |> select(Site_name_machine_readable,'Biogeographic area')

# Count number of total sites per biogeographic region
site_counts <- sites_biogeo |> 
  count(`Biogeographic area`)
names(site_counts)[2] <- "Total"

# Modern sites
modern_sites_biogeo <- sites |> filter(Pollen=="Modern") |> select(Site_name_machine_readable,'Biogeographic area')

# Count number of total sites per biogeographic region
modern_site_counts <- modern_sites_biogeo |> 
  count(`Biogeographic area`)
names(modern_site_counts)[2] <- "Modern"

# Dated sites
site_dated_filtered <- sites |> filter(Pollen=="Fossil")|> filter(Dated == "Yes")

sites_dated_biogeo <- site_dated_filtered |>  select(Site_name_machine_readable,'Biogeographic area')

# Count number of sites per biogeographic region
site_dated_counts <- sites_dated_biogeo |> 
  count(`Biogeographic area`)
names(site_dated_counts)[2] <- "Dated fossil"

# Not dated sites
site_not_dated_filtered <- sites |> filter(Pollen=="Fossil")|> filter(Dated == "No")
sites_not_dated_biogeo <- site_not_dated_filtered |>  select(Site_name_machine_readable,'Biogeographic area')

# Count number of sites per biogeographic region
site_not_dated_counts <- sites_not_dated_biogeo |> 
  count(`Biogeographic area`)
names(site_not_dated_counts)[2] <- "Not dated fossil"

# Merge all counts into one dataframe
sites_combined <- site_counts |> 
  left_join(modern_site_counts, by = "Biogeographic area") |> 
  left_join(site_dated_counts, by = "Biogeographic area") |> 
  left_join(site_not_dated_counts, by = "Biogeographic area")

# Reshape data to long format for ggplot
sites_long <- sites_combined |> 
  pivot_longer(cols = c("Total", "Modern","Dated fossil", "Not dated fossil"), 
               names_to = "Record Type", 
               values_to = "Count")


# Define the order
order <- c("Total", "Modern","Dated fossil", "Not dated fossil")

# Convert `Period` into a factor with the specified order
sites_long$`Record Type` <- factor(sites_long$`Record Type`, levels = order)

# Specify the order of Record Type
pollen_order <- c("Total", "Modern", "Dated fossil", "Not dated fossil")

# Convert `Record Type` into a factor
sites_long$`Record Type` <- factor(
  sites_long$`Record Type`, 
  levels = pollen_order
)

# Create the grouped bar plot
combined_barplot <- ggplot(sites_long, aes(x = `Biogeographic area`, y = Count, fill = `Record Type`)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +  # Dodge bars side by side
  theme_minimal() +
  labs(x = "Biogeographic region", 
       y = "Number of sites", 
       fill = "Record type") +  # Ensure legend title is explicitly set
  scale_fill_manual(values = palette_1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12))

# Save the plot
ggsave(normalizePath("outputs/graphs/combined_sites_biogeo_barplot.png"), 
       combined_barplot, 
       width = 14,
       height = 10,
       dpi = 600,
       units = "cm")

combined_barplot

#--------------------------------------------------------#
# 3) Number sites (fossil and modern) per database----
#--------------------------------------------------------#

# Total sites
sites_database <- sites |>  select(Site_name_machine_readable,Database, Pollen,Dated)

# Count number of total sites per database
total_counts <- sites_database |> 
  count(Database)
names(total_counts)[2] <- "Total"

# Modern sites
sites_database_modern <- sites_database |> filter(Pollen=="Modern")

# Count number of total sites per database
modern_counts <- sites_database_modern |> 
  count(Database)
names(modern_counts)[2] <- "Modern"

# Dated sites
site_dated_filtered <- sites_database |> filter(Pollen=="Fossil")|> filter(Dated == "Yes")

# Count number of sites per database
site_dated_counts <- site_dated_filtered |> 
  count(Database)
names(site_dated_counts)[2] <- "Dated fossil"

# Not dated sites
site_not_dated_filtered <- sites_database |> filter(Pollen=="Fossil")|> filter(Dated == "No")

# Count number of sites per database
site_not_dated_counts <- site_not_dated_filtered |> 
  count(Database)
names(site_not_dated_counts)[2] <- "Not dated fossil"

# Merge all counts into one dataframe
sites_combined_database <- total_counts |> 
  left_join(modern_counts, by = "Database") |> 
  left_join(site_dated_counts, by = "Database") |> 
  left_join(site_not_dated_counts, by = "Database")

sites_combined_database <- sites_combined_database |> mutate(Database = ifelse(Database == "Received from authors", "From authors", Database))


# Reshape data to long format for ggplot
sites_long_database <- sites_combined_database |> 
  pivot_longer(cols = c("Total", "Modern","Dated fossil", "Not dated fossil"), 
               names_to = "Record Type", 
               values_to = "Count")

sites_long_database <- sites_long_database |> na.omit()

# Define the order
order <- c("APD", "Neotoma","APD/Neotoma", "From authors")

# Convert `Period` into a factor with the specified order
sites_long_database$Database <- factor(sites_long_database$Database, levels = order)

# Specify the order of Record Type
pollen_order <- c("Total", "Modern", "Dated fossil", "Not dated fossil")

# Convert `Record Type` into a factor
sites_long_database$`Record Type` <- factor(
  sites_long_database$`Record Type`, 
  levels = pollen_order
)

# Create the grouped bar plot
database_barplot <- ggplot(sites_long_database, aes(x = Database, y = Count, fill = `Record Type`)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +  # Dodge bars side by side
  theme_minimal() +
  labs(x = "", 
       y = "Number of sites", 
       fill = "Record type") +  
scale_fill_manual(values = palette_1) +
theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 14),  
      legend.text = element_text(size = 12))

# Save the plot
ggsave(normalizePath("outputs/graphs/database_barplot.png"), 
       database_barplot, 
       width = 14,
       height = 10,
       dpi = 600,
       units = "cm")
database_barplot


#--------------------------------------------------------#
# 4) Number of dated records per time interval----
#--------------------------------------------------------#

sites_temporal <- sites |>  select(Site_name_machine_readable,"Biogeographic area", "Minimum mean cal BP", "Maximum mean cal BP")
sites_temporal$`Maximum mean cal BP`  <- sites_temporal$`Maximum mean cal BP` |>  as.numeric()
sites_temporal$`Minimum mean cal BP`  <- sites_temporal$`Minimum mean cal BP` |>  as.numeric()

# Define bin width (1000-year intervals)
bin_width <- 1000

# Create bins based on both `Minimum mean cal BP` and `Maximum mean cal BP`
dated_records_binned <- sites_temporal |>
  mutate(
    year_bin = cut(
      `Minimum mean cal BP`, 
      breaks = seq(0, max(`Maximum mean cal BP`, na.rm = TRUE) + bin_width, by = bin_width),
      labels = FALSE,
      include.lowest = TRUE
    )
  ) |>
  group_by(year_bin, `Biogeographic area`) |> # Group by both bins and Biogeographic area
  summarise(n = n(), .groups = "drop")  # Count occurrences

# Convert 'year_bin' back to numeric for plotting
dated_records_binned$year_bin <- as.numeric(as.character(dated_records_binned$year_bin)) * bin_width

# Remove NA values
dated_records_binned <- na.omit(dated_records_binned)

# Omit dates older than 20,000 years BP (because 4 pollen records have dates older than 20,000 BP)
dated_records_binned <- filter(dated_records_binned, year_bin <= 20000)

# Create the bar plot with color per Biogeographic area

dated_records_temporal_distribution <- ggplot(dated_records_binned, 
                                              aes(x = year_bin, y = n, fill = `Biogeographic area`)) +# AHP shaded region
                    geom_rect(aes(xmin = 14800, xmax = 5500, 
                                  ymin = 0, ymax = 100),
                              fill = "lightgrey", alpha = 0.05) +  
                    
                    # Bars 
                    geom_bar(stat = "identity", color = "black", position = "stack", alpha = 0.9) +
                    
                    # Annotate AHP on top 
                    annotate("text", x = (5500 + 14800) / 2, y = 105, label = "AHP", color = "black", size = 5, fontface = "bold", hjust = 0.5)  + 
                  
                    # Scales and theme 
                    scale_x_reverse(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
                    scale_fill_manual(values = palette_3) +
                    theme_minimal() +
                    labs(x = "Years BP", y = "Number of sites", fill = "Biogeographic region") +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  
                          axis.text.y = element_text(size = 12),  
                          axis.title.x = element_text(size = 12), 
                          axis.title.y = element_text(size = 12),
                          plot.title = element_text(size = 14),
                          legend.title = element_text(size = 8), 
                          legend.text = element_text(size = 8))


ggsave("outputs/graphs/dated_records_temporal_distribution.png",
       dated_records_temporal_distribution,
       width = 14,
       height = 10,
       dpi = 600,
       units = "cm")

dated_records_temporal_distribution

#--------------------------------------------------------#
# 5) Latitudinal distribution of records according to their archive type----
#--------------------------------------------------------#

sites_archive_type <- sites |> select(Site_name_machine_readable,Latitude,"Archive type")

# round latitude so it does not have decimals
sites_archive_type$Latitude <- as.numeric(sites_archive_type$Latitude)
sites_archive_type$Latitude <- round(sites_archive_type$Latitude,0)

names(sites_archive_type)[3] <- "Archive_type"

# Count the number of sites per latitude and archive type
sites_archive_type_count <- sites_archive_type |>
  group_by(Latitude, Archive_type) |>
  summarise(num_sites = n(), .groups = "drop")

sites_archive_type_count <- na.omit(sites_archive_type_count)

# Create the bar plot with different colors for each archive type
barplot_latitude_archive_type <- ggplot(sites_archive_type_count, aes(x = num_sites, y = factor(Latitude), fill = Archive_type)) +
  geom_col() +  # Creates bars for each latitude
  labs(
    x = "Number of sites",
    y = "Latitude (degrees)",
    fill = "Archive Type",
  ) +
  theme_minimal() +
  scale_fill_manual(values = palette_1) +
    theme(
    legend.position = "top", 
    legend.text = element_text(size = 10),  
    legend.title = element_text(size = 16, face = "bold"),  
    axis.text.x = element_text(size = 10), 
    axis.text.y = element_text(size = 10), 
    axis.title.x = element_text(size = 16, face = "bold"),  
    axis.title.y = element_text(size = 16, face = "bold"),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)  
  )

# Save
ggsave(normalizePath("outputs/graphs/barplot_archive_type.png"), 
       barplot_latitude_archive_type, 
       width = 14,
       height = 10,
       dpi = 600,
       units = "cm")

# Create a boxplot

# Filter archive types recorded in only one latitude and site:
sites_archive_type_count_filtered <- sites_archive_type_count |>   group_by(Archive_type) |> 
  filter(n_distinct(Latitude) > 1)  |> 
  ungroup()

sites_archive_type_count_filtered <- sites_archive_type_count_filtered |> filter(!Archive_type=="Unknown")

# Plot

boxplot_latitude_archive_type_num_sites <- ggplot(
  sites_archive_type_count_filtered, 
  aes(x = Archive_type, y = Latitude, fill = Archive_type)
) +
  geom_boxplot(
    width = 0.5,
    outlier.shape = NA   # hide boxplot outliers since we’ll plot all points
  ) +
  geom_jitter(
    aes(color = Archive_type),
    width = 0.15,   # horizontal jitter
    size = 2,
    alpha = 0.7,
    show.legend = FALSE
  ) +
  labs(x = "", y = "Latitude (degrees)") +
  coord_cartesian(ylim = c(-10, 60)) +
  theme_minimal(base_size = 20) +
  scale_fill_manual(values = palette_1) +
  scale_color_manual(values = palette_1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
     axis.text.y = element_text(size = 12),
     axis.title.y = element_text(size = 12),
     plot.title = element_text(size = 14, face = "bold"),
     legend.position = "none",
    plot.margin = margin(t = 2, r = 2, b = 2, l = 1))


# Save
ggsave(normalizePath("outputs/graphs/boxplot_latitude_archive_type_num_sites.png"), 
       boxplot_latitude_archive_type_num_sites, 
       width = 14,
       height = 10,
       dpi = 600,
       units = "cm")

#--------------------------------------------------------#
# 6) Altitudinal distribution of records according to their archive type----
#--------------------------------------------------------#

sites_altitude_type <- sites |> select(Site_name_machine_readable,Altitude,"Biogeographic area","Archive type")

sites_altitude_type <- sites_altitude_type |> rename(Archive_type = "Archive type")

sites_altitude_type$Altitude <- as.numeric(sites_altitude_type$Altitude)

# Round down to nearest centennial
sites_altitude_type$Altitude <- floor(sites_altitude_type$Altitude / 100) * 100

names(sites_altitude_type)[3] <- "Biogeographic_area"

# Exclude marine cores
sites_altitude_type <- sites_altitude_type |> filter(Archive_type !="Marine core")

# Count the number of sites per altitude and biogeographic area
sites_altitude_archive_count <- sites_altitude_type |>
  group_by(Altitude, Archive_type) |>
  summarise(num_sites = n(), .groups = "drop")

sites_altitude_archive_count <- na.omit(sites_altitude_archive_count)

# Create the bar plot with different colors for each biogeographic area
barplot_altitude_archive <- ggplot(sites_altitude_archive_count, aes(x = num_sites, y = factor(Altitude), fill = Archive_type)) +
  geom_col() +  # Creates bars for each altitude
  labs(
    x = "Number of sites",
    y = "Altitude (meters)",
    fill = "Archive type",
  ) +
  theme_minimal() +
  scale_fill_manual(values = palette_1) +
  theme(
    legend.position = "top", 
    legend.text = element_text(size = 10),  
    legend.title = element_text(size = 16, face = "bold"),  
    axis.text.x = element_text(size = 10), 
    axis.text.y = element_text(size = 10), 
    axis.title.x = element_text(size = 16, face = "bold"),  
    axis.title.y = element_text(size = 16, face = "bold"),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)  
  )
# Save
ggsave(normalizePath("outputs/graphs/barplot_altitude_archive.png"), 
       barplot_altitude_archive, 
       width = 14,
       height = 10,
       dpi = 600,
       units = "cm")

#--------------------------------------------------------#
# 7) Altitudinal distribution of records according to the biogeographic area----
#--------------------------------------------------------#

sites_altitude_type <- sites |> select(Site_name_machine_readable,Altitude,"Biogeographic area","Archive type")

sites_altitude_type <- sites_altitude_type |> rename(Archive_type = "Archive type")

sites_altitude_type$Altitude <- as.numeric(sites_altitude_type$Altitude)

# Round down to nearest centennial
sites_altitude_type$Altitude <- floor(sites_altitude_type$Altitude / 100) * 100

names(sites_altitude_type)[3] <- "Biogeographic_area"

# Number of marine records per biogeographic area
n_records_marine <- sites_altitude_type |> filter(Archive_type =="Marine core")
n_records_marine <- n_records_marine |> group_by(Biogeographic_area) |>
  summarise(num_sites = n(), .groups = "drop")

# Exclude marine cores
sites_altitude_type <- sites_altitude_type |> filter(!Altitude<0)

# Count the number of sites per altitude and biogeographic area
sites_altitude_type_count <- sites_altitude_type |>
  group_by(Altitude, Biogeographic_area) |>
  summarise(num_sites = n(), .groups = "drop")


sites_altitude_type_count <- na.omit(sites_altitude_type_count)

# Join the counts of marine records to the biogeographic areas
biogeo_labels <- n_records_marine |>
  mutate(label = paste0(Biogeographic_area, "\n","(N marine records = ", " ", num_sites, ")")) |>
  select(Biogeographic_area, label)

# If some Biogeographic_area have 0 marine sites, include them with (0)
all_areas <- unique(sites_altitude_type_count$Biogeographic_area)
biogeo_labels <- full_join(
  tibble(Biogeographic_area = all_areas),
  biogeo_labels,
  by = "Biogeographic_area"
) |>
  mutate(label = ifelse(is.na(label),
                        paste0(Biogeographic_area, " (0)"),
                        label))

# Boxplot

boxplot_altitude_biogeography_num_sites <- ggplot(
  sites_altitude_type_count, 
  aes(x = Biogeographic_area, y = Altitude, fill = Biogeographic_area)
) +
  geom_boxplot(
    width = 0.5,
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(color = Biogeographic_area),
    width = 0.15,
    size = 2,
    alpha = 0.7,
    show.legend = FALSE
  ) +
  labs(
    x = "",
    y = "Altitude (meters)"
  ) +
  theme_minimal(base_size = 16) +
  scale_y_continuous() +
  scale_fill_manual(values = palette_3) +
  scale_color_manual(values = palette_3) +
  scale_x_discrete(labels = setNames(biogeo_labels$label, biogeo_labels$Biogeographic_area)) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 0,        # keep labels horizontal
      hjust = 0.5,      # center horizontally
      vjust = 1,        # optional: tweak vertical position
      size = 16
    ),
    plot.margin = margin(10, 10, 10, 10)
  )

# Save
ggsave(normalizePath("outputs/graphs/boxplot_altitude_biogeography_num_sites.png"), 
       boxplot_altitude_biogeography_num_sites, 
       width = 14,
       height = 10,
       dpi = 600,
       units = "cm")


#--------------------------------------------------------#
# 8) Number of taxa per plant functional trait ----
#--------------------------------------------------------#

# Select only the desired trait columns
selected_columns <- c(
  "pollen_type",
  "mean_log10_whole_plant_height_m",
  "mean_log10_seed_mass_mg",
  "mean_log10_leaf_area_mm2",
  "mean_log10_longest_whole_plant_longevity_years",
  "mean_log10_leaf_dry_mass_g",
  "mean_log10_leaf_life_span_months",
  "mean_log10_leaf_nitrogen_content_per_leaf_dry_mass_mg_g_1",
  "mean_log10_leaf_dry_mass_per_area_g_mm_2",
  "whole_plant_growth_form_diversity",
  "whole_plant_vegetative_phenology",
  "flower_pollination_syndrome",
  "whole_plant_dispersal_syndrome",
  "whole_plant_sexual_system",
  "leaf_type",
  "growth_form_literature"
)

traits <- pft |>select(all_of(selected_columns))

# Count how many pollen types have non-missing values per trait
pfts_counts <- traits |>
  mutate(across(-pollen_type, as.character)) |>  # Convert all columns except pollen_type to character

  pivot_longer(cols = -pollen_type, names_to = "trait", values_to = "value") |> # Reshape to long format

  filter(!is.na(value) & value != "") |> # exclude missing/empty
  group_by(trait) |>
  summarise(count = n_distinct(pollen_type), .groups = "drop") |>
  arrange(desc(count))


# Change names
trait_names <- c(
  "whole_plant_growth_form_diversity" = "Growth forms\n(BIEN)",
  "mean_log10_seed_mass_mg" = "Seed mass\n(mg)",
  "leaf_type" = "Leaf type",
  "mean_log10_whole_plant_height_m" = "Plant height\n(m)",
  "whole_plant_vegetative_phenology" = "Vegetative\nphenology",
  "mean_log10_leaf_area_mm2" = "Leaf area\n(mm2)",
  "mean_log10_leaf_nitrogen_content_per_leaf_dry_mass_mg_g_1" = 
    "Leaf nitrogen content/\nleaf dry mass (mg/g)",
  "whole_plant_sexual_system" = "Sexual system",
  "flower_pollination_syndrome" = "Flower pollination\nsyndrome",
  "whole_plant_dispersal_syndrome" = "Dispersal\nsyndrome",
  "mean_log10_leaf_dry_mass_g" = "Leaf dry mass\n(g)",
  "mean_log10_leaf_dry_mass_per_area_g_mm_2" = "Leaf dry mass/\narea (g/mm2)",
  "mean_log10_longest_whole_plant_longevity_years" = 
    "Longest plant longevity\n(years)",
  "mean_log10_leaf_life_span_months" = "Leaf life span\n(months)",
  "growth_form_literature" = "Growth forms\n(NAMPHORA)"
)


# Apply the new names
pfts_counts$trait <- trait_names[pfts_counts$trait]

# Order trait factor levels by descending count
pfts_counts$trait <- factor(pfts_counts$trait, 
                            levels = pfts_counts$trait[order(-pfts_counts$count)])

# Bar plot
pft_barplot <- ggplot(pfts_counts, aes(x = trait, y = count, fill = trait)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(x = "", y = "Number of harmonised pollen types", fill = "Taxonomic Level") +
  scale_fill_manual(values = palette_1) +
  theme(
    axis.text.x = element_text(angle = 50, hjust = 1, size = 15),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "none"
  )

# Step 8: Save the plot
ggsave(
  filename = normalizePath("outputs/graphs/pft_barplot.png"),
  plot = pft_barplot,
  width = 14,
  height = 10,
  dpi = 600,
  units = "cm")

pft_barplot
