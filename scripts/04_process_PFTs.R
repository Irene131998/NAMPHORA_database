## Script for downloading plant functioanl traits from BIEN & TRY databases

# 0. Load libraries and functions----

source("scripts/functions.R")
libraries <- c("readxl", "readr", "dplyr", "tidyr", "tibble", "stringr", "tools", "here", "purrr","BIEN")

# Install missing packages
invisible(lapply(libraries, install_if_missing))

# Load the libraries
lapply(libraries, require, character.only = TRUE)


## 1) Read taxonomy and get vectors of taxa----

# Read taxonomy database
taxonomy_pollen_taxa <- readxl::read_xlsx(normalizePath("data/processed_data/taxonomy/harmonised_taxonomy_list.xlsx"))

Pollen_types <- taxonomy_pollen_taxa |> dplyr::select(Family,Genus,Species_pollen_type,Pollen_type_SM_morphological) |>  dplyr::distinct(Pollen_type_SM_morphological, .keep_all = TRUE) |> 
  dplyr::mutate(across(everything(), trimws))  # Trim whitespace in all columns

# Create vectors for each taxonomic level
families <- grepl("eae", Pollen_types$Species_pollen_type)
families <- Pollen_types[families, ] # select only family 
families <- families$Species_pollen_type

genera <- Pollen_types |> 
  dplyr::filter(grepl("^\\S+$", Species_pollen_type)) # select rows with one word
exclude <- grepl("eae", genera$Species_pollen_type) # exclude families
genera <- genera[!exclude, , drop = FALSE] # select only genera 
genera <- genera$Species_pollen_type

species <-  Pollen_types |> 
  dplyr::filter(grepl("^\\S+ \\S+$", Species_pollen_type))# select rows with two words
species <- species$Species_pollen_type

## Clean vectors
## Genera
# Find taxa with "/"
genera_with_slash <- grep("/", genera, value = TRUE) # find pollen types that have 2 genera names

# Split taxa with "/" and keep the first word
split_genera <- unlist(lapply(genera_with_slash, function(x) {
  parts <- strsplit(x, "/")[[1]] # splits names
}))

# Combine with original taxa (excluding ones with "/")
genera <- c(setdiff(genera, genera_with_slash), split_genera)
genera <- genera[genera != "NA"]

## Species
# Find taxa with "/"
species_with_slash <- grep("/", species, value = TRUE)

# Split taxa with "/" and keep the first word
split_species <- unlist(lapply(species_with_slash, function(x) {
  parts <- strsplit(x, "/")[[1]]
  first_word <- strsplit(parts[1], " ")[[1]][1]
  sapply(parts, function(part) {
    part_split <- strsplit(part, " ")[[1]]
    if (length(part_split) == 1) {
      return(paste(first_word, part_split[1]))
    } else {
      return(paste(first_word, part_split[2]))
    }
  })
}))


# Combine with original taxa (excluding ones with "/")
all_species <- c(setdiff(species, species_with_slash), split_species)

# Remove single word entries
species <- all_species[grepl("^\\S+ \\S+$", all_species)]

# Remove duplicates
species <- unique(all_species)

# Remove the element "CRETACEOUS FOSSIL"
species <- species[species != "CRETACEOUS FOSSIL"]


## 2) Retrieve the traits----

### 2.1) Categorical variables----

# List of traits
categorical_traits <- c("whole plant dispersal syndrome", 
                        "whole plant vegetative phenology",
                        "whole plant growth form diversity",
                        "whole plant sexual system",
                        "flower pollination syndrome")


# Loop through each trait and get the corresponding trait for each family
family_categorical_variables <- lapply(categorical_traits, function(trait) {
  BIEN_trait_traitbyfamily(trait = trait, family = families)
})
names(family_categorical_variables) <- categorical_traits# Assign results to variables dynamically

# Loop through each trait and get the corresponding trait for each genus
genus_categorical_variables <- lapply(categorical_traits, function(trait) {
  BIEN_trait_traitbygenus(trait = trait, genus = genera)
})
names(genus_categorical_variables) <- categorical_traits# Assign results to variables dynamically

# Loop through each trait and get the corresponding trait for each species
species_categorical_variables <- lapply(categorical_traits, function(trait) {
  BIEN_trait_traitbyspecies(trait = trait, species = species)
})
names(species_categorical_variables) <- categorical_traits# Assign results to variables dynamically


#### 2.1.1) Citations----


temp_dir <- file.path(normalizePath(here::here("docs/supplementary_info/references/references_BIEN_database")))
# family
for (i in seq_along(family_categorical_variables)) {  # For every item of the list
  BIEN_metadata_citation(
    trait.dataframe = family_categorical_variables[[i]],  # Access each dataframe
    bibtex_file = file.path(temp_dir, paste0(names(family_categorical_variables)[i], " family.bib"))
  )
}
# genus
for (i in seq_along(genus_categorical_variables)) {  # For every item of the list
  BIEN_metadata_citation(
    trait.dataframe = genus_categorical_variables[[i]],  # Access each dataframe
    bibtex_file = file.path(temp_dir, paste0(names(genus_categorical_variables)[i], " genus.bib"))
  )
}
# species

for (i in seq_along(species_categorical_variables)) {  # For every item of the list
  BIEN_metadata_citation(
    trait.dataframe = species_categorical_variables[[i]],  # Access each dataframe
    bibtex_file = file.path(temp_dir, paste0(names(species_categorical_variables)[i], " species.bib"))
  )
}


### 2.2) Continuous variables----


# List of traits
continuous_traits <- c("whole plant height",
                       "seed mass",
                       "leaf area",
                       "longest whole plant longevity",
                       "plant flowering begin",
                       "leaf dry mass",
                       "leaf life span",
                       "leaf nitrogen content per leaf dry mass")


# Family ----
# Loop through each trait and get the corresponding trait for each family
family_continuous_variables <- lapply(continuous_traits, function(trait) {
  BIEN_trait_traitbyfamily(trait = trait, family = families)
})
names(family_continuous_variables) <- continuous_traits# Assign results to variables dynamically

# Calculate mean for each trait for family
family_continuous_traits <- lapply(family_continuous_variables, process_family)

# Genus ----
# Loop through each trait and get the corresponding trait for each Genus
Genus_continuous_variables <- lapply(continuous_traits, function(trait) {
  BIEN_trait_traitbygenus(trait = trait, genus = genera)
})
names(Genus_continuous_variables) <- continuous_traits# Assign results to variables dynamically

# Calculate mean for each trait for Genus
Genus_continuous_traits <- lapply(Genus_continuous_variables, process_genus)

# Species ----
# Loop through each trait and get the corresponding trait for each Species
Species_continuous_variables <- lapply(continuous_traits, function(trait) {
  BIEN_trait_traitbyspecies(trait = trait, species = species)
})
names(Species_continuous_variables) <- continuous_traits# Assign results to variables dynamically

# Calculate mean for each trait for Species
Species_continuous_traits <- lapply(Species_continuous_variables, process_species)


#### 2.2.1) Citations----


temp_dir <- file.path(normalizePath(here::here("docs/supplementary_info/references/references_BIEN_database")))

# family
for (i in seq_along(family_continuous_traits)) {  # For every item of the list
  BIEN_metadata_citation(
    trait.dataframe = family_continuous_traits[[i]],  # Access each dataframe
    bibtex_file = file.path(temp_dir, paste0(names(family_continuous_traits)[i], " family.bib"))
  )
}
# genus
for (i in seq_along(Genus_continuous_traits)) {  # For every item of the list
  BIEN_metadata_citation(
    trait.dataframe = Genus_continuous_traits[[i]],  # Access each dataframe
    bibtex_file = file.path(temp_dir, paste0(names(Genus_continuous_traits)[i], " genus.bib"))
  )
}
# species

for (i in seq_along(species_continuous_traits)) {  # For every item of the list
  BIEN_metadata_citation(
    trait.dataframe = species_continuous_traits[[i]],  # Access each dataframe
    bibtex_file = file.path(temp_dir, paste0(names(species_continuous_traits)[i], " species.bib"))
  )
}


## 3) Transform data to dataframes----

### 3.1) Categorical traits----


# Change col names in all the dfs from each list so they have matching colnames and eliminate duplicates in each df
family_categorical_variables_clean <- purrr::map(family_categorical_variables,categorical_trait_columns)
genus_categorical_variables_clean <-  purrr::map(genus_categorical_variables, categorical_trait_columns)
species_categorical_variables_clean <-purrr::map(species_categorical_variables, categorical_trait_columns)

# Extract dfs from the list and bind them
family_categorical_variables_df<- purrr::reduce(family_categorical_variables_clean, rbind, by = "taxa")
genus_categorical_variables_df <- purrr::reduce(genus_categorical_variables_clean, rbind, by = "taxa")
species_categorical_variables_df <- purrr::reduce(species_categorical_variables_clean, rbind, by = "taxa")


### 3.2) Continuos traits----


# Change col names in all the dfs from each list so they have matching colnames and eliminate duplicates in each df
family_continuous_traits_clean <- purrr::map(family_continuous_traits,continuous_trait_columns)
Genus_continuous_traits_clean <-  purrr::map(Genus_continuous_traits, continuous_trait_columns)
Species_continuous_traits_clean <-purrr::map(Species_continuous_traits, continuous_trait_columns)

# Extract dfs from the list and bind them
family_continuous_traits_df<- purrr::reduce(family_continuous_traits_clean, rbind, by = "taxa")
Genus_continuous_traits_df <- purrr::reduce(Genus_continuous_traits_clean, rbind, by = "taxa")
Species_continuous_traits_df <- purrr::reduce(Species_continuous_traits_clean, rbind, by = "taxa")


## 4) Bind all dataframes----


# Modify continuous dfs
family_continuous_traits_df <-continuous_traits_unit(family_continuous_traits_df)
Genus_continuous_traits_df <-continuous_traits_unit(Genus_continuous_traits_df)
Species_continuous_traits_df <-continuous_traits_unit(Species_continuous_traits_df)

# Bind all dfs
pfts <- rbind(family_continuous_traits_df,Genus_continuous_traits_df,Species_continuous_traits_df,family_categorical_variables_df,genus_categorical_variables_df,species_categorical_variables_df)

# Transform the data from long to wide format (we want variables as columns)
pfts_wide <- pfts %>%
  pivot_wider(names_from = trait_name, values_from = trait_value, 
              values_fn = list(trait_value = list), 
              names_repair = "unique")

pfts_wide <- pfts_wide[,-c(7,17)]
colnames(pfts_wide)[1] <- c("taxa")

# Replace NULL values with NA
pfts_wide <- pfts_wide %>%
  mutate(across(where(is.list), ~ as.character(.x))) %>%  # Convert list columns to character
  mutate(across(everything(), ~ na_if(.x, "NULL")))  # Replace "NULL" with NA

# Combine values of url_source, project_pi and project_pi_contact for every taxon
pfts_combined <- pfts_wide %>%
  group_by(taxa) %>%
  summarise(across(everything(), ~ paste(na.omit(.), collapse = "; "))) %>% # 
  ungroup()

colnames(pfts_combined) <- gsub(" ", "_", colnames(pfts_combined))
colnames(pfts_combined)[13] <- "leaf_nitrogen_content_per_leaf_dry_mass_mg_g_1"

pfts_combined <- pfts_combined |> 
  mutate(across(c(whole_plant_height_m, seed_mass_mg, leaf_area_mm2, 
                  longest_whole_plant_longevity_years, plant_flowering_begin_month, 
                  plant_flowering_begin_date, leaf_dry_mass_g, 
                  leaf_life_span_months, leaf_nitrogen_content_per_leaf_dry_mass_mg_g_1), as.numeric))

# Calculate Leaf dry mass per area (LMA): leaf dry mass/leaf area
pfts_combined <- pfts_combined |> 
  mutate(Leaf_dry_mass_per_area_g_mm_2 =leaf_dry_mass_g/leaf_area_mm2)


## 5) Combine pfts with pollen types (harmonised)----

# As there are pollen types that can be two genera or species (e.g. Pinus nigra/sylvestris), we have to separate the taxa in order to combine them with their corresponding pfts, and then bind them together at the end.


names(Pollen_types)[3] <- "taxa" 

# Split taxa that have two names into separate rows
Pollen_types_split <- Pollen_types %>%
  separate(taxa, into = c("taxa1", "taxa2"), sep = "/")

# Rename species Myriophyllum verticillatum and Pinus sylvestris after their separation
Pollen_types_split$taxa2[Pollen_types_split$taxa2 == "verticillatum"] <- "Myriophyllum verticillatum"
Pollen_types_split$taxa2[Pollen_types_split$taxa2 == "sylvestris"] <- "Pinus sylvestris"

# Create a new dataframe with only the taxa (previously separated with /) from taxa2
taxa2_separated <- Pollen_types_split %>%
  filter(!is.na(taxa2)) |> select(-taxa1) |> rename(taxa = taxa2)

# Create a new dataframe with only the taxa (previously separated with /) from taxa1
taxa1_separated <- Pollen_types_split %>%
  filter(!is.na(taxa2)) |> select(-taxa2) |> rename(taxa = taxa1)

# Create a new dataframe with only the taxa that do not have / 
Pollen_types_no_split <- Pollen_types_split %>%
  filter(is.na(taxa2)) %>% select(-taxa2) %>%  rename(taxa = taxa1) # eliminate rows that have info in taxa2

# Bind all pollen types together
Pollen_types_final <- bind_rows(Pollen_types_no_split,taxa1_separated, taxa2_separated)

# Join pollen types with pfts
pollen_types_pfts <- full_join(Pollen_types_final, pfts_combined, by = c("taxa"))

# Combine information from pollen types that have two potential names
pollen_types_pfts_combined <- pollen_types_pfts %>%
  group_by(Pollen_type_SM_morphological, Family,Genus) %>%
  summarise(across(everything(), ~ paste(na.omit(.), collapse = "; "))) %>% # 
  ungroup()


## 6.) Get variable from TRY database (leaf type)----

# For retrieving the categorical variable leaf type from the TRY database (must be directly from the website), we match the final pollen types with the try species lists:


# Match pollen_types list with try species lists
Try_sp <- readr::read_csv(
  normalizePath("data/raw_data/plant_functional_types/TRY/TryAccSpecies.csv"))
names(Try_sp)[2] <- "taxa"

Try_sp_download <- semi_join(Try_sp, Pollen_types_final, by = "taxa") # keeps the rows from the Try_sp that matches those of Pollen_types. 
names(Try_sp_download)[2] <- "AccSpeciesName" # rename to original column name

readr::write_csv(Try_sp_download, file = normalizePath("data/raw_data/plant_functional_types/TRY/Try_list_download.csv"))



# Retrieve the downloaded variable from Try and match it to pollen types so we can include it to the Database as well


leaf_type_TRY <- readxl::read_xlsx(normalizePath("data/raw_data/plant_functional_types/TRY/leaf_type_TRY.xlsx"))

leaf_type_TRY <- leaf_type_TRY |> select(Dataset,AccSpeciesName,OrigValueStr,Reference) |>  rename(Leaf_type = OrigValueStr) |>  rename(taxa = AccSpeciesName)

# Replace "unpub." values with NA
leaf_type_TRY <- leaf_type_TRY %>%
  mutate(across(everything(), ~ na_if(.x, "unpub.")))

# Eliminate duplicates
leaf_type_TRY_no_duplicates <- leaf_type_TRY %>%
  arrange(taxa, desc(Reference)) %>%  # Sort taxa and prioritise non-NA values in 'Reference'
  distinct(taxa, .keep_all = TRUE)     

# Replace NA back to "unpub."
leaf_type_TRY_no_duplicates <- leaf_type_TRY_no_duplicates %>%
  mutate_all(~ replace(., is.na(.), "unpub.")) %>% rename(Try_dataset = Dataset) %>% rename(Try_reference = Reference)

# Join with pollen types final
Pollen_types_final_leaf_type <- full_join(Pollen_types_final, leaf_type_TRY_no_duplicates, by = "taxa") # keeps the rows from the Try_sp that matches those of Pollen_types. 

# Combine information from pollen types that have two potential names
Pollen_types_final_leaf_type <- Pollen_types_final_leaf_type %>%
  group_by(Pollen_type_SM_morphological, Family,Genus) %>%
  summarise(across(everything(), ~ paste(na.omit(.), collapse = "; "))) %>% # 
  ungroup()


## 7) Bind al pfts and save----

# Combine Bien traits with TRY trait
all_pfts <- full_join(pollen_types_pfts_combined,Pollen_types_final_leaf_type)

# Save csv
write.csv(all_pfts, file = normalizePath("data/processed_data/plant_functional_types/total_pfts.csv"), row.names = FALSE)


