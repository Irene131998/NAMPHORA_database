## Script for downloading plant functional traits from BIEN & TRY databases

#--------------------------------------------------------#
# 0) Load libraries and functions----
#--------------------------------------------------------#

source("scripts/functions.R")

libraries <- c("readxl", "readr", "dplyr", "tidyr", "tibble", "stringr", "tools", "here", "purrr","BIEN","devtools")

# Install missing packages
invisible(lapply(libraries, install_if_missing))

# Load the libraries
lapply(libraries, require, character.only = TRUE)

#--------------------------------------------------------#
# 1) Read habit list and species assigned to pollen types ----
#--------------------------------------------------------#

# Read csv of species belonging to pollen types
species_pollen_types <- readr::read_csv((normalizePath("data/processed_data/taxonomy/species_pollen_types_list.csv")),locale = locale(encoding = "latin1"))
species_pollen_types <- species_pollen_types |> rename(pollen_type = Pollen_type_harmonised)

# Get vector of species
species <- species_pollen_types$species

# Read habit categories
habit_list <- readr::read_csv((normalizePath("data/processed_data/taxonomy/habit_list.csv")),locale = locale(encoding = "latin1"))

habit_list <- habit_list |> select("Pollen_type_harmonised","Habit_summarised (for percentage calculation)")
habit_list <- habit_list |> rename(pollen_type = Pollen_type_harmonised)
habit_list <- habit_list |> rename(growth_form_literature= "Habit_summarised (for percentage calculation)")

#--------------------------------------------------------#
# 2) Retrieve the traits----
#--------------------------------------------------------#

### 2.1) Categorical variables----

# List of traits
categorical_traits <- c("whole plant dispersal syndrome", 
                        "whole plant vegetative phenology",
                        "whole plant growth form diversity",
                        "whole plant sexual system",
                        "flower pollination syndrome")

# Loop through each trait and get the corresponding trait for each species
species_categorical_variables <- lapply(categorical_traits, function(trait) {
  BIEN_trait_traitbyspecies(trait = trait, species = species)
})
names(species_categorical_variables) <- categorical_traits

#### 2.1.1) Citations----

temp_dir <- file.path(normalizePath(here::here("../docs/references/pfts_references/references_BIEN_database")))

# Create it if it doesn’t exist
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir, recursive = TRUE)
}

for (i in seq_along(species_categorical_variables)) {  # For every item of the list
  BIEN_metadata_citation(
    trait.dataframe = species_categorical_variables[[i]],  # Access each dataframe
    bibtex_file = file.path(temp_dir, paste0(names(species_categorical_variables)[i], " species.bib"))
  )
}

### 2.2) Continuous variables----

continuous_traits <- c("whole plant height")


# List of traits
continuous_traits <- c("whole plant height",
                       "seed mass",
                       "leaf area",
                       "longest whole plant longevity",
                       "leaf dry mass",
                       "leaf life span",
                       "leaf nitrogen content per leaf dry mass")

# Loop through each trait and get the corresponding trait for each Species
Species_continuous_variables <- lapply(continuous_traits, function(trait) {
  BIEN_trait_traitbyspecies(trait = trait, species = species)
})
names(Species_continuous_variables) <- continuous_traits

####  2.2.1) Log-transform & calculate mean for each trait for species----
Species_continuous_traits <- lapply(Species_continuous_variables, process_species)

#### 2.2.2) Citations----

for (i in seq_along(Species_continuous_traits)) {  # For every item of the list
  BIEN_metadata_citation(
    trait.dataframe = Species_continuous_traits[[i]],  # Access each dataframe
    bibtex_file = file.path(temp_dir, paste0(names(Species_continuous_traits)[i], " species.bib"))
  )
}

#--------------------------------------------------------#
# 3) Transform data to dataframes----
#--------------------------------------------------------#

### 3.1) Categorical traits----

# Change col names in all the dfs from each list so they have matching colnames and eliminate duplicates in each df
species_categorical_variables_clean <-purrr::map(species_categorical_variables, categorical_trait_columns)

# Extract dfs from the list and bind them
species_categorical_variables_df <- purrr::reduce(species_categorical_variables_clean, rbind, by = "taxa")

# Eliminate rows that has the word "taxa" across all columns
species_categorical_variables_df <- species_categorical_variables_df |> filter(!trait_value == "taxa")

# Eliminate those species that has the value "variable or conflicting reports" 
species_categorical_variables_df_cleaned <- species_categorical_variables_df |> 
filter(!trait_value == "variable or conflicting reports")

### 3.2) Continuous traits----

# Change col names in all the dfs from each list so they have matching colnames and eliminate duplicates in each df
Species_continuous_traits_clean <-purrr::map(Species_continuous_traits, continuous_trait_columns)

# Extract dfs from the list and bind them
Species_continuous_traits_df <- purrr::reduce(Species_continuous_traits_clean, rbind, by = "taxa")

# Eliminate rows that has the word "taxa" across all columns
Species_continuous_traits_df <- Species_continuous_traits_df |> filter(!mean_trait_value == "taxa")

#--------------------------------------------------------#
# 4) Bind all dataframes----
#--------------------------------------------------------#

# Modify continuous dfs
Species_continuous_traits_df <-continuous_traits_unit(Species_continuous_traits_df)

# Bind all dfs
pfts <- rbind(Species_continuous_traits_df, species_categorical_variables_df_cleaned)

# Transform the data from long to wide format (we want variables as columns)
pfts_wide <- pfts |>
  pivot_wider(names_from = trait_name, values_from = trait_value, 
              values_fn = list(trait_value = list), 
              names_repair = "unique")

pfts_wide <- pfts_wide |> rename(species = taxa)

# Replace NULL values with NA
pfts_wide <- pfts_wide |>
  mutate(across(where(is.list), ~ as.character(.x))) |>  # Convert list columns to character
  mutate(across(everything(), ~ na_if(.x, "NULL")))  # Replace "NULL" with NA

# Combine values of url_source, project_pi and project_pi_contact for every taxon
pfts_combined <- pfts_wide |>
  group_by(species) |>
  summarise(across(everything(), ~ paste(na.omit(.), collapse = "; "))) |> # 
  ungroup()

colnames(pfts_combined) <- gsub(" ", "_", colnames(pfts_combined))

pfts_combined <- pfts_combined |> rename(leaf_nitrogen_content_per_leaf_dry_mass_mg_g_1 = `leaf_nitrogen_content_per_leaf_dry_mass_mg.g-1`)

# Force continuous variables as numeric
pfts_combined <- pfts_combined |> mutate(across(c(whole_plant_height_m, seed_mass_mg, leaf_area_mm2, longest_whole_plant_longevity_years, leaf_dry_mass_g,leaf_life_span_months, leaf_nitrogen_content_per_leaf_dry_mass_mg_g_1), as.numeric))

# Calculate Leaf dry mass per area (LMA): leaf dry mass/leaf area
pfts_combined <- pfts_combined |> 
  mutate(leaf_dry_mass_per_area_g_mm_2 =leaf_dry_mass_g/leaf_area_mm2)

write.csv(pfts_combined, file = normalizePath("data/processed_data/plant_functional_types/pfts_combined_bien_part5.csv"), row.names = FALSE)

pfts_combined <-read_csv(normalizePath("data/processed_data/plant_functional_types/pfts_combined_bien_part5.csv"))


#--------------------------------------------------------#
# 5) Get authors list from BIEN traits for acknowledgements ----
#--------------------------------------------------------#

# Get project pi list to acknowledge each of data producers in the Supplementary information file
authors_list <- pfts_wide |> select(project_pi, project_pi_contact) |> distinct()
authors_list <- authors_list |> filter(!is.na(project_pi))
authors_list_trait <- pfts |> select(trait_name, project_pi, project_pi_contact) |> distinct()

# Save authors list
write.csv(authors_list, file = normalizePath("docs/supplementary_info/authors_list_pft_BIEN.csv"), row.names = FALSE)
write.csv(authors_list_trait, file = normalizePath("docs/supplementary_info/authors_list_per_trait_BIEN.csv"), row.names = FALSE)

#--------------------------------------------------------#
# 6) Combine pfts with pollen types (harmonised)----
#--------------------------------------------------------#

# Change names to lowercase and eliminate spaces at the end for correct joining
species_pollen_types <- species_pollen_types |> 
  mutate(species = str_squish(str_to_lower(species)))
species_pollen_types <- species_pollen_types |> select(!Taxa)

pfts_combined <- pfts_combined |> 
  mutate(species = str_squish(str_to_lower(species)))

# Do a full join
pollen_types_pfts <- full_join(species_pollen_types, pfts_combined, by = c("species"))

#--------------------------------------------------------#
# 7) Calculate mean traits for each pollen type -----
#--------------------------------------------------------#

# Identify numeric and categorical columns (trait columns)
exclude_cols <- c("pollen_type", "species", "url_source", "project_pi", "project_pi_contact")

numeric_vars <- names(pollen_types_pfts)[sapply(pollen_types_pfts, is.numeric)]
categorical_vars <- names(pollen_types_pfts)[sapply(pollen_types_pfts, is.character)]
categorical_vars <- categorical_vars[!(categorical_vars %in% exclude_cols)]

# Summarise numeric variables (means per pollen_type)
numeric_summary <- pollen_types_pfts |>
  group_by(pollen_type) |>
  summarise(across(all_of(numeric_vars), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"))

# Summarise categorical variables (combine unique values into one cell)
categorical_summary <- pollen_types_pfts |>
  select(pollen_type, all_of(categorical_vars)) |>
  pivot_longer(cols = -pollen_type, names_to = "trait", values_to = "category") |>
  filter(!is.na(category) & category != "") |>
  group_by(pollen_type, trait) |>
  summarise(categories_combined = paste(sort(unique(category)), collapse = ", "), .groups = "drop") |>
  pivot_wider(names_from = trait, values_from = categories_combined)

# Join both summaries into one table
pollen_types_pfts_summary <- numeric_summary |>
  left_join(categorical_summary, by = "pollen_type")

# Force continuous variables as numeric
pollen_types_pfts_summary <- pollen_types_pfts_summary |> mutate(across(c(mean_whole_plant_height_m, mean_seed_mass_mg, mean_leaf_area_mm2, mean_longest_whole_plant_longevity_years, mean_leaf_dry_mass_g,mean_leaf_life_span_months, mean_leaf_nitrogen_content_per_leaf_dry_mass_mg_g_1,mean_leaf_dry_mass_per_area_g_mm_2), as.numeric))

# Change NaN cells for NA
pollen_types_pfts_summary <- pollen_types_pfts_summary  |> 
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))

# summarise project_pi
pi_summary <- pollen_types_pfts |>
  select(pollen_type, project_pi) |>
  group_by(pollen_type) |>
  summarise(
    project_pi = paste(sort(unique(project_pi)), collapse = ", "),
    .groups = "drop"
  )

# create separate columns for each url_source
url_summary <- pollen_types_pfts |>
  select(pollen_type, url_source) |>
  filter(!is.na(url_source) & url_source != "") |>
  distinct() |>
  group_by(pollen_type) |>
  mutate(url_id = paste0("BIEN_url_source_", row_number())) |>
  pivot_wider(
    names_from = url_id,
    values_from = url_source
  )

# Include the url source and project PIs into final Bien pft df
pollen_types_pfts_summary <- pollen_types_pfts_summary |>
  left_join(pi_summary, by = "pollen_type") |>
  left_join(url_summary, by = "pollen_type")

pollen_types_pfts_summary <- pollen_types_pfts_summary |> rename(projects_PIs_BIEN =project_pi)

#--------------------------------------------------------#
# 8) Get variable from TRY database (leaf type)----
#--------------------------------------------------------#

# Read leaf type data, which has been retrieved directly from TRY website
leaf_type_TRY <- read.table(
  normalizePath("data/raw_data/plant_functional_types/TRY/leaf_type_TRY.txt"),
  sep = "\t",
  fill = TRUE,          # Fill missing values with NA
  header = TRUE,        
  stringsAsFactors = FALSE
)

leaf_type_TRY <- leaf_type_TRY |> select(AccSpeciesName,TraitName, OrigValueStr, Dataset,LastName,FirstName,Reference)

# Filter out those values that do not correspond to leaf type
leaf_type_TRY <- leaf_type_TRY |> filter(TraitName == "Leaf type")

# Homogenise trait values names
leaf_type_TRY <- leaf_type_TRY |>
  mutate(
    LeafType_clean = case_when(
      # broadleaf
      OrigValueStr %in% c("B","broad-leaved", "broadleaved", "Broadleaved", "broadleaf") ~ "broad-leaved",
      
      # needleleaf
      OrigValueStr %in% c("needle-leaved", "needle-leaf", "needle", "needles", "needleleaved", "N", "coniferous", "needle-leaved (including non-conifers with needle-like leaves, e.g. many arid-zone Hakea and Acacia)", "fine-leaved (needle, leptophypylluous, linear)") ~ "needle-leaved ",
      
      # scaleleaf
      OrigValueStr %in% c("scale-like", "scale-leaf", "scale-shaped") ~ "scale-leaved",
      
      # unknowns
      OrigValueStr %in% c("C", "yes", "no", "C3", "*", "n.d.","narrowleaved", "mikrophylle", "microphylle", "tiny-leaf", 
                          "Cylindrical", "Phyllodium") ~ "NA",
      
      TRUE ~ "NA"
    )
  )

# Eliminate OrigValueStr column
leaf_type_TRY <- leaf_type_TRY |> select(!OrigValueStr)

# Eliminate rows with NA in LeafType_clean column
leaf_type_TRY <- leaf_type_TRY |> filter(!is.na(LeafType_clean))

# Replace "unpub." values with NA
leaf_type_TRY <- leaf_type_TRY |>
  mutate(across(everything(), ~ na_if(.x, "unpub.")))

# Eliminate duplicates
leaf_type_TRY_no_duplicates <- leaf_type_TRY |>
  arrange(AccSpeciesName, desc(Reference)) |>  # Sort taxa and prioritise non-NA values in 'Reference'
  distinct(AccSpeciesName, .keep_all = TRUE)     

# Replace NA back to "unpub."
leaf_type_TRY_no_duplicates <- leaf_type_TRY_no_duplicates |>
  mutate_all(~ replace(., is.na(.), "unpub.")) |> rename(Try_dataset = Dataset) |> rename(Try_reference = Reference)

# Change names to lowercase and eliminate spaces at the end for correct joining
leaf_type_TRY_no_duplicates <- leaf_type_TRY_no_duplicates |> 
  mutate(AccSpeciesName = str_squish(str_to_lower(AccSpeciesName))) # 

# Change taxa column name for joining
leaf_type_TRY_no_duplicates <- leaf_type_TRY_no_duplicates |> rename(species = AccSpeciesName)

# Eliminate TraitName column
leaf_type_TRY_no_duplicates <- leaf_type_TRY_no_duplicates |> select(!TraitName)

# Join with species_pollen_types
Pollen_types_final_leaf_type <- left_join(species_pollen_types, leaf_type_TRY_no_duplicates, by = "species") 

# Summarise per pollen type and keep references

Pollen_types_final_leaf_type_final <- Pollen_types_final_leaf_type |>
  select(pollen_type, LeafType_clean, Try_reference) |>
  filter(!is.na(LeafType_clean) & LeafType_clean != "") |> 
  group_by(pollen_type) |>
  summarise(
    LeafType_clean = paste(sort(unique(LeafType_clean)), collapse = ", "),
    Try_reference = list(unique(Try_reference)),
    .groups = "drop"
  ) |>
  unnest_longer(Try_reference) |>
  group_by(pollen_type) |>
  mutate(ref_id = paste0("Try_reference_", row_number())) |> # different columns per reference for each pollen type
  pivot_wider(
    names_from = ref_id,
    values_from = Try_reference
  )


# Clean categories
Pollen_types_final_leaf_type_final <- Pollen_types_final_leaf_type_final |>
  mutate(
    leaf_type = LeafType_clean |>
      # split values by comma
      str_split(",\\s*") |>
      # remove "NA" and trim whitespace
      lapply(function(x) {
        x <- trimws(x)
        x <- x[x != "NA"]
        if (length(x) == 0) "NA" else paste(unique(x), collapse = ", ")
      }) |>
      unlist()
  )

Pollen_types_final_leaf_type_final <- Pollen_types_final_leaf_type_final |> select(!LeafType_clean)

# Reorder columns so leaf_type is next to pollen_type column
Pollen_types_final_leaf_type_final <- Pollen_types_final_leaf_type_final  |> 
  relocate(last_col(), .after = 1)

#--------------------------------------------------------#
# 9) Get authors list from TRY trait for acknowledgements ----
#--------------------------------------------------------#

# Get authors list to acknowledge each of data producers in the Supplementary information file
authors_list_try <- Pollen_types_final_leaf_type |> select(LastName, FirstName,Try_reference) |> distinct()
authors_list_try <- authors_list_try |> filter(!is.na(LastName))

# Save authors list
write.csv(authors_list_try, file = normalizePath("docs/supplementary_info/authors_list_pft_TRY.csv"), row.names = FALSE)

#--------------------------------------------------------#
# 10) Bind al pfts and save----
#--------------------------------------------------------#

# Combine Bien traits with TRY trait
all_pfts <- full_join(pollen_types_pfts_summary,Pollen_types_final_leaf_type_final)

# Reorder columns so all traits are next to each other before references
all_pfts <- all_pfts |>
  relocate(leaf_type, .before = projects_PIs_BIEN)

#--------------------------------------------------------#
# 11) Add habit categories from taxonomy list (categories from NAMPHORA) -----
#--------------------------------------------------------#

all_pfts <- full_join(all_pfts,habit_list)

# Reorder columns so all traits are next to each other before references
all_pfts <- all_pfts |>
  relocate(growth_form_literature, .before = projects_PIs_BIEN)

# Rename continuous traits to mean (log10())
all_pfts <- all_pfts |> 
  rename_with(
    ~ sub("^mean_", "mean_log10_", .x), # Finds "mean_" at the start, replaces it with "mean_log10_"
    starts_with("mean_")
  )

#--------------------------------------------------------#
# 12) Save final list of procesed PFTs -----
#--------------------------------------------------------#

# Save csv
write.csv(all_pfts, file = normalizePath("data/processed_data/plant_functional_types/total_pfts.csv"), row.names = FALSE)
