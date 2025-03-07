## Script for data visualisation. Creation of graphs that describe the database

# 0. Load libraries and functions----

source("functions.R")

libraries <- c("dplyr","readr","tidyr","ggplot2","stringr")

# Install missing packages
invisible(lapply(libraries, install_if_missing))

# Load the libraries
lapply(libraries, require, character.only = TRUE)


# 1) Read sites----

sites <- read_csv(normalizePath("../metadata/pollen_data/database.csv"))

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


# Define the order of periods (modify according to your data)
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
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 18),  # Increase Site Type label size
        legend.text = element_text(size = 16))
# Save the plot
ggsave(normalizePath("../outputs/graphs/combined_sites_biogeo_barplot.png"), combined_barplot, width = 17, height = 10, dpi = 300)

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

# Reshape data to long format for ggplot
sites_long_database <- sites_combined_database |> 
  pivot_longer(cols = c("Total", "Fossil", "Modern"), 
               names_to = "Pollen Type", 
               values_to = "Count")

sites_long_database <- sites_long_database |> na.omit()
# Create the grouped bar plot
database_barplot <- ggplot(sites_long_database, aes(x = Database, y = Count, fill = `Pollen Type`)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +  # Dodge bars side by side
  theme_minimal() +
  labs(x = "Database", 
       y = "Number of sites", 
       fill = "Pollen type") +  # Ensure legend title is explicitly set
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 18),  # Increase Site Type label size
        legend.text = element_text(size = 16))
# Save the plot
ggsave(normalizePath("../outputs/graphs/database_barplot.png"), database_barplot, width = 17, height = 10, dpi = 300)

database_barplot

# 4) Number sites per time period----

sites_filtered <- sites |> filter(Dated == "Yes")

sites_period <- sites_filtered |>  select(Site_name_machine_readable,Period)

# Count number of sites per dated/non-dated
sites_period_counts <- sites_period |> 
  count(Period)
sites_period_counts <- na.omit(sites_period_counts)

# Define the order of periods (modify according to your data)
period_order <- c("Late Holocene (Meghalayan)", "Middle Holocene (Northgrippian)", "Early Holocene (Greenlandian)", "Holocene (Early, Middle and Late)", "Holocene, Pre-Holocene","Pre-Holocene")

# Convert `Period` into a factor with the specified order
sites_period_counts$Period <- factor(sites_period_counts$Period, levels = period_order)

# Create bar plot
sites_period_barplot <- ggplot(sites_period_counts, aes(x = `Period`, y = n, fill = `Period`)) +
  geom_bar(stat = "identity", color = "black") +
  theme_minimal() +
  labs(x = "Period", y = "Number of sites") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increase font size of x-axis labels
        axis.text.y = element_text(size = 14),  # Increase font size of y-axis labels
        axis.title.x = element_text(size = 16),  # Increase font size of x-axis title
        axis.title.y = element_text(size = 16),  # Increase font size of y-axis title
        plot.title = element_text(size = 18), legend.position = "none")  # Increase plot title font size

ggsave(normalizePath("../outputs/graphs/sites_period_barplot.png"), sites_period_barplot, width = 17, height = 10, dpi = 300)

sites_period_barplot


# 5) Number of dated records per time interval----


#| warning: false
sites_temporal <- sites_filtered |>  select(Site_name_machine_readable, "Minimum mean cal BP", "Maximum mean cal BP")
sites_temporal$`Maximum mean cal BP`  <- sites_temporal$`Maximum mean cal BP` |>  as.numeric()
sites_temporal$`Minimum mean cal BP`  <- sites_temporal$`Minimum mean cal BP` |>  as.numeric()

# Define bin width (1000-year intervals)
bin_width <- 1000

# Create bins based on both `Minimum mean cal BP` and `Maximum mean cal BP`
dated_records_binned <- sites_temporal %>%
  mutate(
    # Create bins based on the minimum and maximum cal BP range
    year_bin = cut(
      `Minimum mean cal BP`, 
      breaks = seq(0, max(`Maximum mean cal BP`, na.rm = TRUE) + bin_width, by = bin_width),
      labels = FALSE,  # Do not specify labels here, let `cut()` handle it
      include.lowest = TRUE
    )
  ) %>%
  count(year_bin)  # Count the number of records per bin

# Convert 'year_bin' back to a meaningful numeric label
dated_records_binned$year_bin <- as.numeric(as.character(dated_records_binned$year_bin)) * bin_width

dated_records_binned <- na.omit(dated_records_binned)

# Omit dates older than 32.000 years BP
dated_records_binned <- dated_records_binned[-c(23:30),]

# Create the bar plot
dated_records_temporal_distribution<- ggplot(dated_records_binned, aes(x = year_bin, y = n)) +
  geom_bar(stat = "identity", fill = "black", color = "black") +
  theme_minimal() +
  labs(x = "Years BP", y = "Number of sites") +
  scale_x_reverse() +  # Reverse x-axis to show recent years on the right
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increase font size of x-axis labels
        axis.text.y = element_text(size = 14),  # Increase font size of y-axis labels
        axis.title.x = element_text(size = 16),  # Increase font size of x-axis title
        axis.title.y = element_text(size = 16),  # Increase font size of y-axis title
        plot.title = element_text(size = 18), legend.position = "none") + # Increase plot title font size
  geom_vline(xintercept = c(5000, 14800), linetype = "dashed", color = "red", size = 1)  # Add vertical lines for the AHP interval

ggsave(normalizePath("../outputs/graphs/dated_records_temporal_distribution.png"), dated_records_temporal_distribution, width = 17, height = 10, dpi = 300)

dated_records_temporal_distribution


# 6) Number of sites against year of publications by fossil and modern pollen----


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


ggsave(normalizePath("../outputs/graphs/sites_publication_barplot.png"), sites_publication_barplot, width = 17, height = 10, dpi = 300)

sites_publication_barplot


# 7) Latitudinal distribution of records according to their archive type----

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
    y = "Latitude",
    fill = "Archive Type",
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",  # Move legend to top
    legend.text = element_text(size = 14),  # Increase legend text size
    legend.title = element_text(size = 16, face = "bold"),  # Increase and bold legend title
    axis.text.x = element_text(size = 14),  # Increase x-axis labels
    axis.text.y = element_text(size = 14),  # Increase y-axis labels
    axis.title.x = element_text(size = 16, face = "bold"),  # Increase and bold x-axis title
    axis.title.y = element_text(size = 16, face = "bold"),  # Increase and bold y-axis title
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)  # Center and enlarge plot title
  )

ggsave(normalizePath("../outputs/graphs/barplot_archive_type.png"), barplot_archive_type, width = 17, height = 10, dpi = 300)

barplot_archive_type


# 8) Altitudinal distribution of records according to the biogeographic area----

sites_altitude_type <- sites |> select(Site_name_machine_readable,Altitude,"Biogeographic area")

sites_altitude_type$Altitude <- as.numeric(sites_altitude_type$Altitude)

# Round down to nearest centennial
sites_altitude_type$Altitude <- floor(sites_altitude_type$Altitude / 100) * 100

names(sites_altitude_type)[3] <- "Biogeographic_area"

# Eliminate Atlantic ocean
sites_altitude_type <- sites_altitude_type |> filter(Biogeographic_area !="Atlantic ocean")

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
    y = "Altitude",
    fill = "Biogeographic area",
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",  # Move legend to top
    legend.text = element_text(size = 14),  # Increase legend text size
    legend.title = element_text(size = 16, face = "bold"),  # Increase and bold legend title
    axis.text.x = element_text(size = 14),  # Increase x-axis labels
    axis.text.y = element_text(size = 14),  # Increase y-axis labels
    axis.title.x = element_text(size = 16, face = "bold"),  # Increase and bold x-axis title
    axis.title.y = element_text(size = 16, face = "bold"),  # Increase and bold y-axis title
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)  # Center and enlarge plot title
  )

ggsave(normalizePath("../outputs/graphs/barplot_altitude_biogeography.png"), barplot_altitude_biogeography, width = 17, height = 10, dpi = 300)

barplot_altitude_biogeography

