# Install libraries ----
install_if_missing <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}

# PFTS ----
# Function for calculating the mean in each df of the list family_continuous_traits
process_family <- function(df) {
  df$trait_value <- as.numeric(df$trait_value)
  df_mean <- df %>%
    group_by(scrubbed_family) %>%
    summarise(mean_trait_value = mean(trait_value, na.rm = TRUE))
  df_mean_complete <- df_mean %>%
    dplyr::left_join(df)
  return(df_mean_complete)
}


# Function for calculating the mean in each df of the list Genus_continuous_traits
process_genus <- function(df) {
  df$trait_value <- as.numeric(df$trait_value)
  df_mean <- df %>%
    group_by(scrubbed_genus) %>%
    summarise(mean_trait_value = mean(trait_value, na.rm = TRUE))
  df_mean_complete <- df_mean %>%
    dplyr::left_join(df)
  return(df_mean_complete)
}

# Function for calculating the mean in each df of the list Species_continuous_traits
process_species<- function(df) {
  df$trait_value <- as.numeric(df$trait_value)
  df_mean <- df %>%
    group_by(scrubbed_species_binomial) %>%
    summarise(mean_trait_value = mean(trait_value, na.rm = TRUE))
  df_mean_complete <- df_mean %>%
    dplyr::left_join(df)
  return(df_mean_complete)
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
  df <- df |> select(taxa,trait_value,trait_name,trait_value,url_source,project_pi,project_pi_contact)
  df <- df |>  distinct(taxa, .keep_all = TRUE)   # Eliminate duplicated taxa
  return(df)  # Ensure the function returns the modified data frame
}

# Append values from column unit to trait_name

continuous_traits_unit <- function(df){
  # change name so it matches that of categorical dfs 
  colnames(df)[colnames(df) == "mean_trait_value"] <- "trait_value"
  
  # Create the new column name from 'trait_name' and 'unit' columns
    df <- df %>%
    mutate(trait_name_unit = paste(trait_name, unit, sep = "_")) %>%
    relocate(trait_name_unit, .after = trait_value)
  
  # Eliminate columns that we do not need
  df <- df %>%
   select(-trait_name,-unit)
  
  # Rename new column so it matches that of categorical dfs 
  colnames(df)[colnames(df) == "trait_name_unit"] <- "trait_name"
  
  
  return(df)  # Ensure the function returns the modified data frame
  
}

