## Script that retrieves species associated with each pollen type that are exclusively found within the study region

#--------------------------------------------------------#
# 0. Load libraries and functions----
#--------------------------------------------------------#

source("scripts/functions.R")

libraries <- c("readxl", "readr", "dplyr", "tidyr", "tibble", "stringr", "tools", "here", "purrr","BIEN","devtools","furrr","rgbif","CoordinateCleaner","data.table")

# Install missing packages
#invisible(lapply(libraries, install_if_missing))

# Load the libraries
lapply(libraries, require, character.only = TRUE)

#--------------------------------------------------------#
# 1. Read data and get vectors of taxa----
#--------------------------------------------------------#

# Read pollen types taxa csv
Taxa_pollen_types <- readr::read_csv((normalizePath("data/processed_data/taxonomy/Taxa_pollen_types_list.csv")),locale = locale(encoding = "latin1")) # pollen types and the taxa that produce the pollen grain (every pollen type with t. and undif. or typeA/typeB: get genera manually/ everything else, get species as it is the taxon)

# Remove leading and trailing spaces
Taxa_pollen_types$Taxa <- trimws(Taxa_pollen_types$Taxa)
Taxa_pollen_types$Taxa <- gsub("\\s+$", "", Taxa_pollen_types$Taxa)

#--------------------------------------------------------#
# 2. Define study area ----
#--------------------------------------------------------#

# Load raster
elevation <- terra::rast(normalizePath("data/raw_data/mapping_data/elevation.tiff"))

# Define study area and raster for resolution
study_area <- terra::ext(c(-20,63,2,45)) 
rasters <- terra::crop(elevation,study_area)
terra::plot(rasters)

# Define geometry polygon
geometry <- geom(terra::as.polygons(ext(rasters[[1]]), crs = crs(rasters[[1]])), wkt = TRUE)

#--------------------------------------------------------#
# 3. Get species belonging to pollen types that are family level ----
#--------------------------------------------------------#

# Get only Taxa at family level
families <- grepl("aceae", Taxa_pollen_types$Taxa)
families <- Taxa_pollen_types[families, ] # select only family 
families <- families$Taxa

## species
species_families <- list()

for (i in seq_along(families)) {
  taxon <- families[i]
  cat("Processing taxon", i, "of", length(families), ":", taxon, "\n")
  
  result <- tryCatch({
    get_species_from_family(taxon)
  }, error = function(e) {
    message("Error for taxon ", taxon, ": ", e$message)
    return(NULL)
  })
  
  if (!is.null(result)) {
    species_families[[taxon]] <- result
  }
}

# Combine the list into a single data frame with genus as a column
species_families_df <- dplyr::bind_rows(
  lapply(names(species_families), function(family) {
    data.frame(
      taxon = family,
      species = species_families[[family]],
      stringsAsFactors = FALSE
    )
  })
)
species_families_df <- species_families_df |> dplyr::filter(!is.na(species.canonicalName))

#--------------------------------------------------------#
# 4. Get species belonging to pollen types that are genus level ----
#--------------------------------------------------------#

# Get Taxa that are at genus level
genera <- Taxa_pollen_types |> 
  dplyr::filter(grepl("^\\S+$", Taxa)) # select rows with one word
exclude <- grepl("aceae", genera$Taxa) # exclude families
genera <- genera[!exclude, , drop = FALSE] # select only genera 
genera <- genera$Taxa

species_genera <- list()

for (i in seq_along(genera)) {
  taxon <- genera[i]
  cat("Processing taxon", i, "of", length(genera), ":", taxon, "\n")
  
  result <- tryCatch({
    get_species_from_genera(taxon)
  }, error = function(e) {
    message("Error for taxon ", taxon, ": ", e$message)
    return(NULL)
  })
  
  if (!is.null(result)) {
    species_genera[[taxon]] <- result
  }
}

# Combine the list into a single data frame with genus as a column
species_genera_df <- dplyr::bind_rows(
  lapply(names(species_genera), function(genus) {
    data.frame(
      taxon = genus,
      species = species_genera[[genus]],
      stringsAsFactors = FALSE
    )
  })
)

species_genera_df <- dplyr::bind_rows(
  lapply(names(species_genera), function(genus) {
    dplyr::mutate(species_genera[[genus]], taxon = genus)
  })
)


species_genera_df <- species_genera_df |> dplyr::filter(!is.na(canonicalName))


#--------------------------------------------------------#
# 5. Final species list to retrieve occurrences from GBIF----
#--------------------------------------------------------#

# Get Taxa that were already harmonised at species level in the database
species <-  Taxa_pollen_types |> 
  dplyr::filter(grepl("^\\S+ \\S+$", Taxa))# select rows with two words
species <- species$Taxa

# Get GBIF keys for the species

species_keys <- list()

for (i in seq_along(species)) {
  taxon <- species[i]
  cat("Processing taxon", i, "of", length(species), ":", taxon, "\n")
  
  result <- tryCatch({
    get_species_keys(taxon)
  }, error = function(e) {
    message("Error for taxon ", taxon, ": ", e$message)
    return(NULL)
  })
  
  if (!is.null(result)) {
    species_keys[[taxon]] <- result
  }
}

# Combine the list into a single data frame with genus as a column
species_df <- dplyr::bind_rows(
  lapply(names(species_keys), function(species) {
    data.frame(
      taxon = species,
      species.canonicalName = species,
      species.speciesKey= species_keys[[species]],
      stringsAsFactors = FALSE
    )
  })
)

species_df <- species_df |> dplyr::filter(!is.na(species.canonicalName))


# Rename colnames to match
species_genera_df <- species_genera_df |> rename(species.canonicalName=canonicalName)
species_genera_df <- species_genera_df |> rename(species.speciesKey=speciesKey)

# Bind with the species from families and genera
taxa_species_names <- rbind(species_df, species_families_df, species_genera_df) # 447485 species

write.csv(taxa_species_names, file = normalizePath("data/processed_data/taxonomy/taxa_species_names.csv"), row.names = FALSE)


#--------------------------------------------------------#
# 6. Get species list from GBIF -----
#--------------------------------------------------------#

# Read GBIF species list (directly downloaded from GBIF website due to unfeasibility of retrieving this data with the GBIF API in R)
gbif_species <- read_delim("data/raw_data/taxonomy/GBIF_species_list/0026936-250827131500795.csv", delim = "\t")# GBIF.org (04 September 2025) GBIF Occurrence Download https://doi.org/10.15468/dl.66vbhj

# Select essential columns
gbif_species <- gbif_species |> select(species,speciesKey)

# Eliminate NAs
gbif_species <- gbif_species |> filter(!is.na(species))

# Eliminate duplicates
gbif_species <- gbif_species |> distinct()
  

# Modify names in final_list for joining
taxa_species_names <- taxa_species_names  |> rename(Taxa = taxon,
                                        species = species.canonicalName,
                                        speciesKey = species.speciesKey)

# Select only the column species from both lists
taxa_species_names_check <- taxa_species_names |> select(species)
gbif_species_check <-gbif_species|> select(species)

# Change names to lowercase and eliminate spaces at the end for correct joining
taxa_species_names_check <- taxa_species_names_check |> 
  mutate(species = str_squish(str_to_lower(species))) # as interect is very strict, change names to lowercase and eliminate spaces at the end

gbif_species_check <- gbif_species_check |> 
  mutate(species = str_squish(str_to_lower(species)))

# Check which species from our list are in the study area according to GBIF
species_in_area <- intersect(taxa_species_names_check$species,
                             gbif_species_check$species)

species_in_area <- species_in_area |> as.data.frame()

species_in_area <- species_in_area |> rename(species = species_in_area)

# Eliminate species key
taxa_species_names <- taxa_species_names |> select(!speciesKey)

# Change names to lowercase and eliminate spaces at the end for correct joining
taxa_species_names <- taxa_species_names |> 
  mutate(species = str_squish(str_to_lower(species))) 

# Join with taxa_species_names to get Taxa name
filtered_species <- taxa_species_names %>%
  semi_join(species_in_area, by = "species")

# Capitalise first letter of genus name
filtered_species <- filtered_species  |>  mutate(species = str_replace(species,"^(\\w)", toupper))

# Join with taxa_pollen_types to get Pollen type name
species_list_pollen_types <- left_join(Taxa_pollen_types,filtered_species)

# Eliminate NA
species_list_pollen_types <- species_list_pollen_types |> filter(!is.na(species))

#--------------------------------------------------------#
# 7. Save final pollen type - species list ----
#--------------------------------------------------------#

write.csv(species_list_pollen_types, file = normalizePath("data/processed_data/taxonomy/species_pollen_types_list.csv"), row.names = FALSE)


