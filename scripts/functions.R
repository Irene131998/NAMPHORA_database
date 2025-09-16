# Install libraries ----
install_if_missing <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}

# Processing pollen data ----
# Check if any numeric values in the data frame have decimals

has_decimals <- function(df) {
  # Select numeric columns that are NOT 'depth' or contain 'median' or "BP" in the column name
  pollen_cols <- df|>
    select(where(is.numeric))|>
    select(!matches("depth|median|BP|sample")) #eliminate columns that are 'depth' or 'median' or "BP" 
  
  # Check if any values have decimals
  any(sapply(pollen_cols, function(col) any(col %% 1 != 0, na.rm = TRUE)))
}

# Species-pollen types list ----

get_species_from_family <- function(family_name) {
 
  # Get species under family
  sp <- rgbif::name_lookup(
    query=family_name,
    rank = "SPECIES",
    status = "ACCEPTED",
    limit = 10000
  )$data
  
  sp <- sp |> dplyr::filter(kingdom %in% c("Viridiplantae", "Plantae"))
  sp <- sp |> dplyr::filter(family == family_name)
  sp <- sp |> dplyr::filter(!grepl(family_name, species))
  
  if (is.null(sp) && nrow(sp)== 0) {
  stop("No species found under family: ", family_name)
  }
    
  species_list <- sp |> dplyr::distinct(canonicalName, .keep_all = TRUE) |> dplyr::select(canonicalName,speciesKey)
  
  return(species_list)
}

get_species_from_genera <- function(taxon_name) {

  # Get species under genus
  sp <- rgbif::name_lookup(
    query=taxon_name,
    rank = "SPECIES",
    status = "ACCEPTED",
    limit = 10000
  )$data
  
  sp <- sp |> dplyr::filter(kingdom %in% c("Viridiplantae", "Plantae"))
  sp <- sp |> dplyr::filter(genus == taxon_name)

  if (is.null(sp) && nrow(sp)== 0) {
    stop("No species found under family: ", taxon_name)
  }
  
  species_list <- sp |> dplyr::distinct(canonicalName, .keep_all = TRUE) |> dplyr::select(canonicalName,speciesKey)
  
  
  return(species_list)
}


get_species_keys <- function(taxon_name) {

  # Get GBIF taxon key
  key <- rgbif::name_backbone(name = taxon_name, kingdom='plants')$usageKey
  if (is.null(key)) stop("Taxon not found")

  return(key)
}
  
  
# PFTS ----

# Function for calculating the mean in each df of the list Species_continuous_traits
process_species<- function(df) {

  # Eliminate NAs
  df <- df |> filter(!trait_value == "*")
  df <- df |> filter(!is.na(trait_value))
  
  # Convert to numeric variable
  df$trait_value <- as.numeric(df$trait_value)
  
  # Eliminate 0 & negative values
  df <- df |> filter(trait_value > 0)

  # Log transform to reduce skewness and normalise distributions
  df <- df|>
    mutate(log_trait_value = log10(trait_value + 0.001))
  
  # Calculate mean
  df_mean <- df|>
    group_by(scrubbed_species_binomial)|>
    summarise(mean_trait_value = mean(log_trait_value, na.rm = TRUE),
              .groups = "drop")
  
  # Calculate SD
  df_sd<- df|>
    group_by(scrubbed_species_binomial)|>
    summarise(SD_trait_value = sd(log_trait_value, na.rm = TRUE),
              .groups = "drop")
  
  # Bind to original df
  df_mean_complete <- df_mean|>
    dplyr::left_join(df)
  
  df_mean_sd_complete <- df_sd|>
    dplyr::left_join(df_mean_complete)
  
  
  return(df_mean_sd_complete)
}


# Function to select and change colnames in each df of the list and to eliminate duplicates in each df
continuous_trait_columns <- function(df){ 
  colnames(df)[1]<- "taxa"
  df <- df |> select(taxa,mean_trait_value,trait_name,unit,url_source,project_pi,project_pi_contact)
  df <- df |>  distinct(taxa, .keep_all = TRUE)   # Eliminate duplicated taxa
  return(df)  # Ensure the function returns the modified data frame
}

categorical_trait_columns <- function(df){ 
  colnames(df)[1]<- "taxa"
  df <- df |> select(taxa,trait_name,trait_value,url_source,project_pi,project_pi_contact)
  df <- df |>  distinct(taxa, .keep_all = TRUE)   # Eliminate duplicated taxa
  return(df)  # Ensure the function returns the modified data frame
}

# Append values from column unit to trait_name

continuous_traits_unit <- function(df){
  # change name so it matches that of categorical dfs 
  colnames(df)[colnames(df) == "mean_trait_value"] <- "trait_value"
  
  # Create the new column name from 'trait_name' and 'unit' columns
    df <- df|>
    mutate(trait_name_unit = paste(trait_name, unit, sep = "_"))|>
    relocate(trait_name_unit, .after = trait_value)
  
  # Eliminate columns that we do not need
  df <- df|>
   select(-trait_name,-unit)
  
  # Rename new column so it matches that of categorical dfs 
  colnames(df)[colnames(df) == "trait_name_unit"] <- "trait_name"
  
  
  return(df)  # Ensure the function returns the modified data frame
  
}

