## Script showing how to use the database for a specific taxon and for a site. 

# Load libraries and functions

source("scripts/functions.R")

libraries <-c("dplyr","readr","tidyr","ggplot2","stringr","tibble","sf", "terra", "ggplot2", "dplyr", "readr", "RColorBrewer","ggnewscale","patchwork","maptiles","ggspatial","rnaturalearth","scales","FD","compositions")

# Install missing packages
#invisible(lapply(libraries, install_if_missing))

# Load the libraries
lapply(libraries, require, character.only = TRUE)

#--------------------------------------------------------#
# 1) EXAMPLE A----
#--------------------------------------------------------#

## 1.1. Get pollen records from a specific taxon ----

# File list with all pollen records files in csv
file_list <- list.files(path = here::here("data/processed_data/pollen_data/fossil/harmonised_percentages"), pattern = "*.csv", full.names = TRUE)

# Read the Database file containing latitude and longitude for all records
database <- suppressWarnings(readr::read_csv(here::here("metadata/pollen_data/database.csv")))

# Get records for Quercus evergreen pollen type
taxa = "Quercus evergreen"
  
# Get all the files in which the pollen types is found, and extract the depths in which the pollen type occurs, with the corresponding date and percentages
  
# Empty data frame to store results
compiled_data <- data.frame(
  Site = character(),
  Years_BP_median = numeric(),
  Percentage = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each file
for (file in file_list) {
  # Read the CSV
  data <- suppressWarnings(
    readr::read_csv(file, locale = locale(encoding = "UTF-8"))
  )
  
  # Find columns for the target taxon and the median age column
  taxa_columns <- grep(taxa, names(data), ignore.case = TRUE, value = TRUE)
  median_columns <- grep("median", names(data), ignore.case = TRUE, value = TRUE)
  
  # Extract data if both taxa and median columns exist
  if (length(taxa_columns) > 0 & length(median_columns) > 0) {
    median_column <- median_columns[1]
    
    # Filter rows where the median column has numeric values
    filtered_data <- data[!is.na(as.numeric(data[[median_column]])), c(taxa_columns, median_column)]
    
    if (nrow(filtered_data) > 0) {
      # Create a temporary data frame with Site, Years_BP_median, and Percentage
      temp_df <- data.frame(
        Site = basename(file),
        Years_BP_median = filtered_data[[median_column]],
        Percentage = filtered_data[, taxa_columns, drop = FALSE],
        stringsAsFactors = FALSE
      )
      
      # Append to compiled_data
      compiled_data <- rbind(compiled_data, temp_df)
      
      print(paste("Processed file:", file))
    }
  } else {
    print(paste("No", taxa, "or 'median' columns found in:", file))
  }
}


# Incorporate columns latitude and longitude from the database.csv metadata file
  
# Rename columns if necessary
compiled_data <- compiled_data %>%
    dplyr::rename(Site_name_machine_readable =Site ) 
  
  
# Remove ".csv" suffix from the 'Site_name_machine_readable' column
compiled_data$Site_name_machine_readable <- str_remove(compiled_data$Site_name_machine_readable, "\\.csv$")
  
# Select the columns that we need from the database
database <- database %>%
    dplyr::select(Site_name_machine_readable, Latitude, Longitude,`Biogeographic area`)
  
# Remove duplicate rows based on the 'Site_name_machine_readable' column
database <- database %>%
    dplyr::distinct(Site_name_machine_readable, .keep_all = TRUE)
  
# Merge latitude and longitude information with compiled_data
final_data <- compiled_data %>%
    dplyr::left_join(database, by = "Site_name_machine_readable")
  
# Create new column "Years_BP_category": Column specifying the interval to which the pollen date corresponds. 
dated_fossil_records <- final_data %>%
    dplyr::mutate(Years_BP_category = round(Years_BP_median / 1000) * 1000)
  

## 1.2. Map dated fossil pollen records from the selected taxon for the Sahara region----

# Ensure the column name is correct
dated_fossil_records <- dated_fossil_records |> rename(Percentage = "Quercus.evergreen")

# Download satellite map
bbox_sf <- st_as_sfc(st_bbox(
    c(xmin = -22, xmax = 40, ymin = 18, ymax = 35),
    crs = 4326  # <- wider area
  ))

sat_map <- get_tiles(bbox_sf, provider = "Esri.WorldImagery", zoom = 7)
  
dated_fossil_records_sahara <- dated_fossil_records |> filter(`Biogeographic area` == "Sahara")

# Plot
p_fossil_dated_Sahara <- ggplot() +
    layer_spatial(sat_map) +
    annotation_north_arrow(
      location = "tr", which_north = "true",
      style = north_arrow_fancy_orienteering()
    ) +
    geom_point(
      data = dated_fossil_records_sahara,
      aes(x = Longitude, y = Latitude),
      shape = 21, color = "black", size = 2, stroke = 0.4,  fill = "lightgoldenrod1"
    ) +
    coord_sf(
      xlim = c(-22, 40),
      ylim = c(18, 34),
      expand = FALSE
    ) +
    labs(
      title = "",
      x = "Longitude", y = "Latitude"
    ) +
    theme_minimal(base_size =  8) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 8))
  


ggsave(
  filename = normalizePath("outputs/maps/taxa_example_A_pollen_records_map.png"),
  plot = p_fossil_dated_Sahara,
  width = 12,
  height = 12,
  dpi = 600,
  units = "cm")



# Select only the records from the Holocene
dated_fossil_records_sahara <- dated_fossil_records_sahara |> filter(!Years_BP_category > 15000)

dated_fossil_records_sahara <- dated_fossil_records_sahara |> filter(!Percentage == 0.000)


# Plot smoothed loess trend with CI  
percentage_smooth_plot <- ggplot(dated_fossil_records_sahara, aes(x = Years_BP_median, y = Percentage)) +
  
  # Smoothed trend with CI ribbon
  geom_smooth(color = "lightgoldenrod1", fill = "lightgoldenrod1", method = "loess",
              span = 0.3, alpha = 0.2, size = 1.2) +
  
  # Original points
  geom_point(color = "black", fill = "lightgoldenrod1", shape = 21, size = 1.5,
             stroke = 0.4, alpha = 0.6) +
  
  # Reverse x-axis for Years BP
  scale_x_reverse(labels = scales::label_number(big.mark = ",")) +
  
  labs(
    x = "Years BP",
    y = "Pollen percentage",
    title = ""
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


ggsave(
  filename = normalizePath("outputs/graphs/percentag_over_time_smooth_plot.png"),
  plot = percentage_smooth_plot,
  width = 10,
  height = 6,
  dpi = 600,
  units = "cm")


## 1.3. Get plant functional traits from the selected taxon----

# Load data
species_traits <- read.csv(normalizePath("data/processed_data/plant_functional_types/total_pfts.csv"))

# Eliminate unecessary columns
species_traits <- species_traits |> select(!c(17:141))

# Select only the traits from the selected taxon (i.e. Quercus evergreen)

species_traits_quercus <- species_traits |> filter(pollen_type == "Quercus evergreen")

cat_traits <- species_traits_quercus |> select(c("whole_plant_vegetative_phenology","flower_pollination_syndrome","whole_plant_dispersal_syndrome","whole_plant_sexual_system","leaf_type","growth_form_literature"))


# Prepare numeric traits
numeric_cols <- names(species_traits_quercus)[
  sapply(species_traits_quercus, is.numeric) &
    !str_detect(names(species_traits_quercus), "Try_reference|BIEN_url_source")
]

num_traits <- species_traits_quercus %>%
  select(all_of(numeric_cols)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Trait",
    values_to = "Value"
  ) %>%
  mutate(
    # Reverse log10 for variables that were stored in log10
    Value_transformed = if_else(str_detect(Trait, "mean_log10"), 10^Value, Value),
    # Add nice labels / units
    Trait_label = case_when(
      str_detect(Trait, "mean_log10_whole_plant_height_m") ~ "Height (m)",
      str_detect(Trait, "mean_log10_seed_mass_mg") ~ "Seed mass (mg)",
      str_detect(Trait, "mean_log10_leaf_area_mm2") ~ "Leaf area (mm²)",
      str_detect(Trait, "mean_log10_longest_whole_plant_longevity_years") ~ "Longevity (yrs)",
      str_detect(Trait, "mean_log10_leaf_dry_mass_g") ~ "Leaf dry mass (g)",
      str_detect(Trait, "mean_log10_leaf_life_span_months") ~ "Leaf lifespan (months)",
      str_detect(Trait, "mean_log10_leaf_nitrogen_content_per_leaf_dry_mass_mg_g_1") ~ "Leaf N (mg/g)",
      str_detect(Trait, "mean_log10_leaf_dry_mass_per_area_g_mm_2") ~ "Leaf dry mass per area (g/mm²)",
      TRUE ~ Trait  # keep original for any others
    )
  )

num_traits <- na.omit(num_traits)

# Plot continuous traits
num_traits_plot <- ggplot(num_traits, aes(x = Trait_label, y = Value)) +
  geom_segment(aes(x = Trait_label, xend = Trait_label, y = -1, yend = Value),
               color = "grey70") +
  geom_point(size = 2, color = "darkgreen") +
  geom_text(aes(label = round(Value, 2)),  # add values on top
            vjust = 2, size = 3.5, fontface = "bold") +
  theme_minimal(base_size = 20) +
  ylab("Log10 transformed values") +
  xlab("") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  coord_flip()  # optional: flip for horizontal lollipop


ggsave(
  filename = normalizePath("outputs/graphs/taxa_example_num_traits_plot.png"),
  plot = num_traits_plot,
  width = 14,
  height = 10,
  dpi = 600,
  units = "cm")


#--------------------------------------------------------#
# 2) EXAMPLE B----
#--------------------------------------------------------#

# 2.1. Load Lake Yoa record ----
df <- readr::read_csv(file_list[grepl("Yoa", file_list)], locale = readr::locale(encoding = "latin1"))

# Eliminate unnecessary columns
df <- df |> dplyr::select(!matches(c("Sample","sum")))

# Rename recal median BP column to cal_median_BP
if (any(grepl("recal_median_BP", names(df)))) {
  df <- df |> rename(cal_median_BP = matches("recal_median_BP"))
}

# Eliminate columns with all NA
df <- df |> select(where(~ !all(is.na(.))))

# Add column sample_id
df_filtered <- df |> 
  dplyr::mutate(sample_id = df$depth) |>
  select(last_col(), everything())

df_filtered$sample_id <- as.character(df_filtered$sample_id) # sample_id must be categorical

# Eliminate rows with no age
df_filtered <- df_filtered |> filter(!is.na(cal_median_BP))

# Transform df so taxa are in columns (long format)
taxa_cols <- names(df_filtered)[
  !names(df_filtered) %in% c(
    "sample_id", "depth", "cal_median_BP", "total_grains", "sum", "site", "age_NA","age"
  )
]

df_transformed <- df_filtered |> 
  pivot_longer(cols = taxa_cols,
               names_to = "pollen_type",
               values_to = "abundance")

# Eliminate sample, age and depth columns so there are only taxa in the df
df_transformed <- df_transformed |> select(-matches(c("depth","total_grains","site","BP","age","volume")))
df_transformed <- df_transformed |> filter(abundance > 0) # filter out taxa/samples with 0 abundance

# 2.2. Add traits ----
df_transformed_traits <- df_transformed |> left_join(species_traits)

# Get the names of character columns with numbers, excluding pollen_type and sample_id
cols_to_convert <- df_transformed_traits |>
  select(-c(pollen_type,sample_id)) |>
  select(where(is.character)) |>
  select(where(~ any(grepl("\\d", ., perl = TRUE)))) |>
  names()

# Convert continuous variables to numeric in case they are not
df_transformed_traits <- df_transformed_traits |>
  mutate(across(all_of(cols_to_convert), ~ as.numeric(.)))

# Convert character variables to factor
df_transformed_traits <- df_transformed_traits |>
  mutate(across(
    where(~ is.character(.)),
    as.factor
  ))

# Eliminate taxa (rows) with no trait data in all traits
df_transformed_traits <- df_transformed_traits |>
  filter(!if_all(
    .cols = -c(sample_id, pollen_type, abundance),
    .fns = ~ is.na(.) | . == ""
  ))

# Get traits and abundance for each spp as separate dfs for estimating CWMs
traits <- df_transformed_traits |> select(!c(sample_id,abundance)) |> distinct(pollen_type, .keep_all = TRUE) # species in rows and traits in cols

abundance <- df_transformed_traits |> select(c(sample_id,abundance,pollen_type)) 
abundance <- abundance |> pivot_wider(names_from = pollen_type,
                                      values_from = c(abundance))

# Convert NULL and NA values to 0
abundance <- abundance|>
  mutate(across(where(is.list), ~ as.character(.)))|>
  mutate(across(where(is.character), ~ as.numeric(replace(., . == "NULL", "0")))) 

abundance[is.na(abundance)] <- 0

abundance_df <- as.data.frame(abundance) # convert to df and then convert to matrix

# Convert to matrix for functcomp function
traits_df <- as.data.frame(traits) # convert to df to set spp names as rownames and then convert to matrix
rownames(traits_df) <- traits_df$pollen_type
traits_df$pollen_type <- NULL # eliminate pollen_type col

# Add NA to empty cells in traits_df
traits_df <- traits_df|>
  mutate(across(where(is.factor), ~ {
    x <- as.character(.)
    x <- na_if(x, "")
    factor(x)
  }))

traits_df <- traits_df |> select(where(~ !all(is.na(.))))   # Eliminate traits with all NA

# Modify original df to eliminate taxa with no traits
df_filtered <- df_filtered  |> 
  select(c(sample_id,depth,cal_median_BP,all_of(rownames(traits_df))))

# Convert abundance_df to matrix 
rownames(abundance_df) <- abundance_df$sample_id
abundance_df$sample_id <- NULL
abundance_matrix <- as.matrix(abundance_df)

# Replace NA values in all character columns with empty strings
traits_df <- traits_df|>
  mutate(across(where(is.factor), ~ {
    x <- as.character(.)
    x[is.na(x)] <- ""
    factor(x)
  }))


# Combine in a list to supply to functcomp
list <- list(trait=traits_df,
             abun=abundance_matrix)


# 2.3. Calculate CWM for each trait ----
df_CWM <- FD::functcomp(list$trait, list$abun,CWM.type = "all") # For categorical variables the abundance of each individual class (level of the factor) is returned, varying between 0 (low abundance) and 1 (high abundance) for a trait.


# Log-ratio transformation of categorical traits (as they are in proportions)
categorical_traits <- c("whole_plant_dispersal_syndrome", 
                        "whole_plant_vegetative_phenology",
                        "whole_plant_growth_form_diversity",
                        "whole_plant_sexual_system",
                        "flower_pollination_syndrome",
                        "leaf_type",
                        "growth_form_literature")

pattern <- paste(categorical_traits, collapse = "|")
df_selected <- df_CWM[, grepl(pattern, names(df_CWM))]

# Get groups for each trait
groups <- list(
  dispersal = grep("^whole_plant_dispersal_syndrome_", names(df_selected), value = TRUE),
  phenology = grep("^whole_plant_vegetative_phenology_", names(df_selected), value = TRUE),
  growth_form_BIEN = grep("^whole_plant_growth_form_diversity_", names(df_selected), value = TRUE),
  sexual_system = grep("^whole_plant_sexual_system_", names(df_selected), value = TRUE),
  pollination = grep("^flower_pollination_syndrome_", names(df_selected), value = TRUE),
  leaf_type = grep("^leaf_type_", names(df_selected), value = TRUE),
  growth_form_literature = grep("^growth_form_literature_", names(df_selected), value = TRUE))

# Because we have the proportions of each category for each categorical trait, and they are not independent (all categories sum to 1 in each categorical trait), we perform log ratio transformation (centered log ratio transformation) to categorical variables
clr_transformed_list <- lapply(groups, function(cols) {
  
  comp_data <- df_selected[, cols, drop = FALSE]
  
  # Replace 0s with small value
  comp_data[comp_data == 0] <- 1e-6
  
  # Normalise rows
  comp_data <- comp_data / rowSums(comp_data)
  
  # CLR transformation
  clr(comp_data)
  
})

# Combine in a df
clr_df <- do.call(cbind, clr_transformed_list)
clr_df <- as.data.frame(clr_df)

# Combine with the other traits
continuous_traits <- df_CWM[, setdiff(names(df_CWM), names(df_selected))]

df_CWM_final <- cbind(continuous_traits,clr_df)

# Add sample_id
df_CWM_final$sample_id <- rownames(df_CWM_final)

df_CWM_final <- df_CWM_final |>
  select(sample_id, everything()) # add sample_id column to the beginning of df

# Add age BP
cal_median_BP <- df_filtered |> select(sample_id,cal_median_BP)

# Ensure sample_id is in both and character
df_CWM_final$sample_id <- as.character(df_CWM_final$sample_id)
cal_median_BP$sample_id <- as.character(cal_median_BP$sample_id)

# Join by sample_id
df_CWM_final <- left_join(df_CWM_final, cal_median_BP, by = "sample_id")

# Locate age column after sample_id
df_CWM_final <- df_CWM_final |>
  relocate(cal_median_BP, .after = sample_id)

# 2.4. Model temporal trends of some traits ----

# Fit and plot GAMs to capture temporal trends

# Rename selected continuous traits
df_CWM_final <- df_CWM_final |> 
  rename("Plant height (m)" = mean_log10_whole_plant_height_m,
         "leaf dry mass/area (g/mm²)" = mean_log10_leaf_dry_mass_per_area_g_mm_2)

cont_traits <-  c("Plant height (m)",
                  "Leaf dry mass/area (g/mm²)")

for(trait in cont_traits) {
  plot_df <- df_CWM_final %>%
    select(cal_median_BP, all_of(trait))
  
  p <- ggplot(plot_df, aes(x = cal_median_BP, y = .data[[trait]])) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "gam", formula = y ~ s(x), color = "darkgreen", se = TRUE) +
    scale_x_reverse(labels = comma) +  # BP time goes backward
    labs(x = "Years BP", y = trait,
         title = paste("Temporal trend of", trait)) +
    theme_minimal() + 
    theme(axis.text = element_text(size = 12))
  
  
  saved_trait <- gsub("/", "_", trait) 
  
  ggsave(
    filename = paste0("outputs/graphs/record_example_plot_", saved_trait, ".png"),  # file name
    plot = p,
    width = 7, height = 5, dpi = 600
  )
  
}


# For growth forms, combine the trends in a single plot

# Select variables to model
cat_traits <- c("growth_form_literature_Herbs",    
                "growth_form_literature_Shrubs",
                "growth_form_literature_Trees")

# Combine raw data for Herbs, Shrubs, Trees
raw_df <- df_CWM_final %>%
  select(cal_median_BP, all_of(cat_traits)) %>%
  pivot_longer(-cal_median_BP, names_to = "trait", values_to = "value")

# Rename traits
raw_df <- raw_df |> mutate(
  trait = case_when(
    str_detect(trait, "growth_form_literature_Herbs") ~ "Herbs",
    str_detect(trait, "growth_form_literature_Trees") ~ "Trees",
    str_detect(trait, "growth_form_literature_Shrubs") ~ "Shrubs"))

# Plot points + GAM smoothing for all growth forms together
cat_plot <- ggplot(raw_df, aes(x = cal_median_BP, y = value, color = trait)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, size = 1,
              aes(fill = trait),) +
  scale_x_reverse(labels = comma) +
  labs(x = "Years BP",
       y = "Value",
       title = "Temporal trends of growth forms") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "right",
        axis.text = element_text(size = 12))

ggsave(
  filename = paste0("outputs/graphs/record_example_plot_growth_forms.png"),  # file name
  plot = cat_plot,
  width = 7, height = 6, dpi = 600
)


# 2.5. Map Lake Yoa ---- 

# Find coordinates for Lake Yoa in the database metadata

lake_yoa_coordinates <- database |> filter(Site_name_machine_readable == "Lake_Yoa_Chad")
lake_yoa_coordinates <- lake_yoa_coordinates |> select(Latitude,Longitude)

# Download satellite map
bbox_sf <- st_as_sfc(st_bbox(
  c(xmin = -2, xmax = 32, ymin = 4, ymax = 33),
  crs = 4326  # <- wider area
))

sat_map <- get_tiles(bbox_sf, provider = "Esri.WorldImagery", zoom = 7)

# Get world countries
countries <- ne_countries(scale = "medium", returnclass = "sf")

# Crop to satelline map extent
bbox <- st_bbox(c(xmin = -2, xmax = 32, ymin = 4, ymax = 33), crs = st_crs(countries))
countries_cropped <- st_crop(countries, bbox)

# Plot
lake_yoa_map <- ggplot() +
  # Satellite map
  layer_spatial(sat_map) +

  # Country borders
  geom_sf(data = countries, fill = NA, color = "black", size = 0.7) +
  
  # Lake points
  geom_point(data = lake_yoa_coordinates,
             aes(x = Longitude, y = Latitude),
             shape = 21, color = "black", size = 4, stroke = 0.4,  fill = "red") +
  
  # North arrow
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  
  coord_sf(
    xlim = c(-2, 32),
    ylim = c(4, 33),
    expand = FALSE
  ) +
  labs(
    title = "",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size =  12) +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11)    )



ggsave(
  filename = normalizePath("outputs/maps/record_example_B_map.png"),
  plot = lake_yoa_map,
  width = 12,
  height = 12,
  dpi = 600,
  units = "cm")


df_transformed_traits_matrix <-  df_transformed_traits[,-c(5:10)]
df_transformed_traits_matrix <- df_transformed_traits_matrix[,c(1:5,12)]
                                                             df_transformed_traits_matrix <- df_transformed_traits_matrix |>rename(plant_height=mean_log10_whole_plant_height_m,LMA=mean_log10_leaf_dry_mass_per_area_g_mm_2,growth_forms=growth_form_literature)                                           
                                                             