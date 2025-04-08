## Script for data visualisation. Creation of graphs that describe the database

# 0. Load libraries and functions----

source("scripts/functions.R")

libraries <- c("dplyr","readr","tidyr","ggplot2","stringr","tibble")

# Install missing packages
invisible(lapply(libraries, install_if_missing))

# Load the libraries
lapply(libraries, require, character.only = TRUE)


# 1) Read data----

sites <- read_csv(normalizePath("metadata/pollen_data/database.csv"))
taxonomy <- read_csv(normalizePath("data/processed_data/taxonomy/harmonised_taxonomy_list.csv"))
phyto_aff <- read_csv(normalizePath("data/processed_data/taxonomy/phytogeographic_affinity.csv"))
pft <- read_csv(normalizePath("data/processed_data/plant_functional_types/total_pfts.csv"))

# 2) Number sites per bigeographic region----

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
names(site_dated_counts)[2] <- "Dated"

# Not dated sites
site_not_dated_filtered <- sites |> filter(Pollen=="Fossil")|> filter(Dated == "No")
sites_not_dated_biogeo <- site_not_dated_filtered |>  select(Site_name_machine_readable,'Biogeographic area')

# Count number of sites per biogeographic region
site_not_dated_counts <- sites_not_dated_biogeo |> 
  count(`Biogeographic area`)
names(site_not_dated_counts)[2] <- "Not dated"

# Merge all counts into one dataframe
sites_combined <- site_counts |> 
  left_join(modern_site_counts, by = "Biogeographic area") |> 
  left_join(site_dated_counts, by = "Biogeographic area") |> 
  left_join(site_not_dated_counts, by = "Biogeographic area")

# Reshape data to long format for ggplot
sites_long <- sites_combined |> 
  pivot_longer(cols = c("Total", "Modern","Dated", "Not dated"), 
               names_to = "Site Type", 
               values_to = "Count")


# Define the order
order <- c("Total", "Modern","Dated", "Not dated")

# Convert `Period` into a factor with the specified order
sites_long$`Site Type` <- factor(sites_long$`Site Type`, levels = order)


# Create the grouped bar plot
combined_barplot <- ggplot(sites_long, aes(x = `Biogeographic area`, y = Count, fill = `Site Type`)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +  # Dodge bars side by side
  theme_minimal() +
  labs(x = "Biogeographic region", 
       y = "Number of sites", 
       fill = "Site type") +  # Ensure legend title is explicitly set
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 16),  
        legend.text = element_text(size = 14))
# Save the plot
ggsave(normalizePath("outputs/graphs/combined_sites_biogeo_barplot.png"), 
       combined_barplot, 
       width = 8.5,   
       height = 6,  
       dpi = 300,   # High resolution (300 DPI is standard for publication)
       units = "in" 
)

combined_barplot


# 3) Number sites (fossil and modern) per database----

# Total sites
sites_database <- sites |>  select(Site_name_machine_readable,Database, Pollen)

# Count number of total sites per database
total_counts <- sites_database |> 
  count(Database)
names(total_counts)[2] <- "Total"

# Dated sites
sites_database_fossil <- sites_database |> filter(Pollen=="Fossil")

# Count number of sites per biogeographic region
fossil_counts <- sites_database_fossil |> 
  count(Database)
names(fossil_counts)[2] <- "Fossil"

# Not dated sites
sites_database_modern <- sites_database |> filter(Pollen=="Modern")

# Count number of sites per biogeographic region
modern_counts <- sites_database_modern |> 
  count(Database)
names(modern_counts)[2] <- "Modern"

# Merge all counts into one dataframe
sites_combined_database <- total_counts |> 
  left_join(fossil_counts, by = "Database") |> 
  left_join(modern_counts, by = "Database")

sites_combined_database <- sites_combined_database |> mutate(Database = ifelse(Database == "Received from authors", "From authors", Database))


# Reshape data to long format for ggplot
sites_long_database <- sites_combined_database |> 
  pivot_longer(cols = c("Total", "Fossil", "Modern"), 
               names_to = "Pollen Type", 
               values_to = "Count")

sites_long_database <- sites_long_database |> na.omit()

# Define the order
order <- c("APD", "Neotoma","APD/Neotoma", "From authors")

# Convert `Period` into a factor with the specified order
sites_long_database$Database <- factor(sites_long_database$Database, levels = order)


# Create the grouped bar plot
database_barplot <- ggplot(sites_long_database, aes(x = Database, y = Count, fill = `Pollen Type`)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +  # Dodge bars side by side
  theme_minimal() +
  labs(x = "", 
       y = "Number of sites", 
       fill = "Pollen type") +  
  scale_fill_manual(values = c("#56B4E9", "#A3B96C", "#E69F00")) +
theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 16),  
        legend.text = element_text(size = 14))

# Save the plot
ggsave(normalizePath("outputs/graphs/database_barplot.png"), 
       database_barplot, 
       width = 8,   
       height = 6,  
       dpi = 300,   # High resolution (300 DPI is standard for publication)
       units = "in" 
)
database_barplot



# 4) Number of dated records per time interval----

sites_temporal <- sites |>  select(Site_name_machine_readable,"Biogeographic area", "Minimum mean cal BP", "Maximum mean cal BP")
sites_temporal$`Maximum mean cal BP`  <- sites_temporal$`Maximum mean cal BP` |>  as.numeric()
sites_temporal$`Minimum mean cal BP`  <- sites_temporal$`Minimum mean cal BP` |>  as.numeric()

# Define bin width (1000-year intervals)
bin_width <- 1000

# Create bins based on both `Minimum mean cal BP` and `Maximum mean cal BP`
dated_records_binned <- sites_temporal %>%
  mutate(
    year_bin = cut(
      `Minimum mean cal BP`, 
      breaks = seq(0, max(`Maximum mean cal BP`, na.rm = TRUE) + bin_width, by = bin_width),
      labels = FALSE,
      include.lowest = TRUE
    )
  ) %>%
  group_by(year_bin, `Biogeographic area`) %>%  # Group by both bins and Biogeographic area
  summarise(n = n(), .groups = "drop")  # Count occurrences

# Convert 'year_bin' back to numeric for plotting
dated_records_binned$year_bin <- as.numeric(as.character(dated_records_binned$year_bin)) * bin_width

# Remove NA values
dated_records_binned <- na.omit(dated_records_binned)

# Omit dates older than 20,000 years BP
dated_records_binned <- filter(dated_records_binned, year_bin <= 20000)

# Create the bar plot with color per Biogeographic area
dated_records_temporal_distribution <- ggplot(dated_records_binned, 
                                              aes(x = year_bin, y = n, fill = `Biogeographic area`)) +
  geom_bar(stat = "identity", color = "black", position = "stack") +  # Stack bars per Biogeographic area
  theme_minimal() +
  labs(x = "Years BP", y = "Number of sites", fill = "Biogeographic region") +
  scale_x_reverse() +  # Reverse x-axis for chronological representation
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 14),
        legend.title = element_text(size = 8), 
        legend.text = element_text(size = 8)) + 
  geom_segment(aes(x = 5500, xend = 14800, y = 30, yend = 30), color = "black", size = 1) +  # Horizontal line from x = 5500 to x = 14800 at y = 60
  # Small vertical lines at both ends
  geom_segment(aes(x = 5500, xend = 5500, y = 28, yend = 32), color = "black", size = 1) +  # Left end vertical
  geom_segment(aes(x = 14800, xend = 14800, y = 28, yend = 32), color = "black", size = 1) +  # Right end vertical
  annotate("text", x = (5500 + 14800) / 2, y = 35, label = "AHP", color = "black", size = 5, fontface = "bold", hjust = 0.5)  # Add text "AHP" at the center of the line


ggsave(normalizePath("outputs/graphs/dated_records_temporal_distribution.png"), 
       dated_records_temporal_distribution, 
       width = 6,   
       height = 4,  
       dpi = 300,   # High resolution (300 DPI is standard for publication)
       units = "in" 
)

dated_records_temporal_distribution


# 5) Number of sites against year of publications by fossil and modern pollen----

sites_publication <- sites |>  select(Site_name_machine_readable,`Reference 1`,Pollen)

# Extract the first 4-digit number (year) from the `Reference 1` column
sites_publication$year <- stringr::str_extract(sites_publication$`Reference 1`, "\\d{4}")

# Ensure the 'year' column is numeric
sites_publication$year <- as.numeric(sites_publication$year)

# Group years into decades by rounding down
sites_publication$decade <- floor(sites_publication$year / 10) * 10

sites_publication <- sites_publication |>  select(Site_name_machine_readable,decade,Pollen)

# Count number of sites per decade
sites_publication_fossil <- sites_publication |> filter(Pollen=="Fossil") |> 
  count(decade)
sites_publication_fossil <- na.omit(sites_publication_fossil)
sites_publication_fossil <- sites_publication_fossil %>% dplyr::filter(decade > 1900)
names(sites_publication_fossil)[2] <- "Fossil"

sites_publication_modern <- sites_publication |> filter(Pollen=="Modern") |> 
  count(decade)
sites_publication_modern <- na.omit(sites_publication_modern)
names(sites_publication_modern)[2] <- "Modern"

# Merge all counts into one dataframe
sites_publication_total <-  full_join(sites_publication_fossil,sites_publication_modern)

# Reshape data to long format for ggplot
sites_publication_total_long <- sites_publication_total |> 
  pivot_longer(cols = c("Fossil", "Modern"), 
               names_to = "Pollen Type", 
               values_to = "Count")


sites_publication_total_long[is.na(sites_publication_total_long)] <- 0

# Create bar plot
sites_publication_barplot <- ggplot(sites_publication_total_long, aes(x = decade, y = Count, fill = `Pollen Type`)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +  # Dodge bars side by side
  theme_minimal() +
  labs(x = "Publication decade", 
       y = "Number of sites", 
       fill = "Pollen type") +  # Ensure legend title is explicitly set
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 18),  # Increase Site Type label size
        legend.text = element_text(size = 16))


ggsave(normalizePath("docs/supplementary_info/graphs/sites_publication_barplot.png"), 
       sites_publication_barplot, 
       width = 8,   
       height = 6,  
       dpi = 300,   # High resolution (300 DPI is standard for publication)
       units = "in" 
)

sites_publication_barplot


# 6) Latitudinal distribution of records according to their archive type----

sites_archive_type <- sites |> select(Site_name_machine_readable,Latitude,"Archive type")

# round latitude so it does not have decimals
sites_archive_type$Latitude <- as.numeric(sites_archive_type$Latitude)
sites_archive_type$Latitude <- round(sites_archive_type$Latitude,0)

names(sites_archive_type)[3] <- "Archive_type"

# Count the number of sites per latitude and archive type
sites_archive_type_count <- sites_archive_type %>%
  group_by(Latitude, Archive_type) %>%
  summarise(num_sites = n(), .groups = "drop")

sites_archive_type_count <- na.omit(sites_archive_type_count)

# Create the bar plot with different colors for each archive type
barplot_archive_type <- ggplot(sites_archive_type_count, aes(x = num_sites, y = factor(Latitude), fill = Archive_type)) +
  geom_col() +  # Creates bars for each latitude
  labs(
    x = "Number of sites",
    y = "Latitude (degrees)",
    fill = "Archive Type",
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "darkblue", "darkorange", "black", "lightgreen", 
                               "gold", "purple", "turquoise", "blue", "violet", 
                               "yellow", "grey", "tomato", "darkgreen", 
                               "darkcyan", "chocolate3", "red", "pink", "steelblue", "chartreuse3","darkred","blueviolet","coral2","azure1","aquamarine","brown3","chartreuse1","burlywood2")) + # 28
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

ggsave(normalizePath("outputs/graphs/barplot_archive_type.png"), 
       barplot_archive_type, 
       width = 10,   
       height = 8.5,  
       dpi = 300,   # High resolution (300 DPI is standard for publication)
       units = "in" 
)
barplot_archive_type


# 7) Altitudinal distribution of records according to the biogeographic area----

sites_altitude_type <- sites |> select(Site_name_machine_readable,Altitude,"Biogeographic area","Archive type")

sites_altitude_type <- sites_altitude_type |> rename(Archive_type = "Archive type")

sites_altitude_type$Altitude <- as.numeric(sites_altitude_type$Altitude)

# Round down to nearest centennial
sites_altitude_type$Altitude <- floor(sites_altitude_type$Altitude / 100) * 100

names(sites_altitude_type)[3] <- "Biogeographic_area"

# Exclude marine cores
sites_altitude_type <- sites_altitude_type |> filter(Archive_type !="Marine core")

# Count the number of sites per altitude and biogeographic area
sites_altitude_type_count <- sites_altitude_type %>%
  group_by(Altitude, Biogeographic_area) %>%
  summarise(num_sites = n(), .groups = "drop")

sites_altitude_type_count <- na.omit(sites_altitude_type_count)

# Create the bar plot with different colors for each biogeographic area
barplot_altitude_biogeography <- ggplot(sites_altitude_type_count, aes(x = num_sites, y = factor(Altitude), fill = Biogeographic_area)) +
  geom_col() +  # Creates bars for each altitude
  labs(
    x = "Number of sites",
    y = "Altitude (meters)",
    fill = "Biogeographic area",
  ) +
  theme_minimal() +
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

ggsave(normalizePath("outputs/graphs/barplot_altitude_biogeography.png"), 
       barplot_altitude_biogeography, 
       width = 10,   
       height = 8,  
       dpi = 300,   # High resolution (300 DPI is standard for publication)
       units = "in" 
)
barplot_altitude_biogeography

# 8) Number of harmonised pollen taxa per phytogeographic affinity (proportion)----

# Reshape data into long format
phyto_aff_long <- phyto_aff %>%
  pivot_longer(cols = -Pollen_type_SM_morphological, # Keep Pollen_type_SM_morphological
               names_to = "phytogeographic_Affinity", # Affinities become a new column
               values_to = "Presence") %>%  # Presence/absence info
  filter(Presence == "x")  # Keep only rows where the pollen type is associated with the affinity

# Count the occurrences of each phytogeographic affinity
affinity_counts <- phyto_aff_long %>%
  count(phytogeographic_Affinity) %>%
  arrange(desc(n))  # Sort in descending order

# Calculate proportions 
affinity_percentages <- affinity_counts %>%
mutate(percentage = n / sum(n) * 100)

affinity_percentages$phytogeographic_Affinity <- gsub("_", " ", affinity_percentages$phytogeographic_Affinity) # removes the underscore in the names

affinity_percentages <- affinity_percentages |> arrange(desc(percentage))
order <- affinity_percentages$phytogeographic_Affinity

# Ensure the phytogeographic_Affinity is a factor with the desired order
affinity_percentages$phytogeographic_Affinity <- factor(affinity_percentages$phytogeographic_Affinity, levels = order)


phyto_aff_barplot <- ggplot(affinity_percentages, aes(x = phytogeographic_Affinity, y = percentage)) +
  geom_bar(stat = "identity", fill = "grey") + # fill all the bars in grey
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5, 
            size = 6) +
  theme_minimal() +
  labs(x = "",
       y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 18),
        legend.position = "none")  # removes the legend


# Save the plot
ggsave(normalizePath("outputs/graphs/phyto_aff_proportions.png"), 
       phyto_aff_barplot, 
       width = 15,   
       height = 8,  
       dpi = 300,   # High resolution (300 DPI is standard for publication)
       units = "in" 
)
phyto_aff_barplot

# 9) Number of taxa (divided by family, genus and species) per plant functional trait ----

pft <- pft |> select(!c(Pollen_type_SM_morphological,Family,Genus,url_source,project_pi,project_pi_contact,Try_reference,Try_dataset,leaf_dry_mass_g))
                     
# Species (select those with two words)
species <- pft %>% 
  filter(str_detect(taxa, "^\\w+\\s+\\w+$"))
species_counts <- species |> select(-taxa) |> summarise(across(everything(), n_distinct, na.rm = TRUE))
species_counts <- t(species_counts) |> as.data.frame()
names(species_counts)[1] <- "species"


# Family (select taxa that ends with eae)
family <- pft %>% 
  filter(str_detect(taxa, "eae"))
family_counts <- family |> select(-taxa) |> summarise(across(everything(), n_distinct, na.rm = TRUE))
family_counts <- t(family_counts) |> as.data.frame()
names(family_counts)[1] <- "family"

# Genera
genera <- pft %>%
  filter(!taxa %in% species_counts$taxa & !taxa %in% family_counts$taxa)
genera_counts <-genera |> select(-taxa)|> summarise(across(everything(), n_distinct, na.rm = TRUE))
genera_counts <- t(genera_counts) |> as.data.frame()
names(genera_counts)[1] <- "genus"

# Merge all counts into one dataframe
pfts_counts_combined <- cbind(family_counts,genera_counts,species_counts)
pfts_counts_combined <- pfts_counts_combined %>%
  tibble::rownames_to_column(var = "row_name")  # Store row names

# Reshape data to long format for ggplot
pfts_counts_long <- pfts_counts_combined %>%
  pivot_longer(cols = -row_name, names_to = "category", values_to = "count")

# Define the order
pfts_counts_long <- pfts_counts_long |> arrange(desc(count))
order <- unique(pfts_counts_long$row_name)

# Modify the row names: Replace underscores with spaces and add parentheses around the last word
modified_order <- sub("_", " ", order)  # Replace underscores with spaces
modified_order <- c(
  "Seed mass (mg)",                                    # modified
  "Whole plant height (m)",                             # modified
  "Leaf area (mm2)",                                    # modified
  "Leaf nitrogen content per leaf dry mass (mg/g)",
  "Leaf dry mass per area (g/mm2)",
  "Plant flowering begin (month)",                      # modified
  "Longest whole plant longevity (years)",              # modified
  "Leaf life span (months)",                            # modified
  "Plant flowering begin date",
  "Whole plant growth form diversity",
  "Leaf type",
  "Whole plant dispersal syndrome",
  "Whole plant sexual system",
  "Whole plant vegetative phenology",
  "Flower pollination syndrome"
)


# Ensure the row_name is a factor with the desired order
pfts_counts_long$row_name <- factor(pfts_counts_long$row_name, levels = order)

# Apply modified names to the factor levels
levels(pfts_counts_long$row_name) <- modified_order

# Create the bar plot
pft_barplot <- ggplot(pfts_counts_long, aes(x = row_name, y = count, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(x = "",
       y = "Count",
       fill = "Taxonomic Level") +
  scale_fill_manual(values = c("family" = "blue", "genus" = "green", "species" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 18),  
        legend.text = element_text(size = 18),
        legend.position = "bottom")


# Save the plot
ggsave(normalizePath("outputs/graphs/pft_barplot.png"), 
       pft_barplot, 
       width = 18,   
       height = 10,  
       dpi = 300,   # High resolution (300 DPI is standard for publication)
       units = "in" 
)

pft_barplot
