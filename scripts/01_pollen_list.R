## Script for extracting data from Neotoma & get raw pollen list for harmonisation

# 0. Load libraries and functions ----
source("scripts/functions.R")

libraries <- c("neotoma2", "dplyr", "tidyr", "readr")

# Install missing packages
invisible(lapply(libraries, install_if_missing))

# Load the libraries
lapply(libraries, require, character.only = TRUE)

# 1. Download neotoma files and convert to APD structure (i.e. pollen types in columns) ----

## 1.1) Download from neotoma ----
# Read sites_id file to download all from neotoma using neotoma2 R package
datasets_ids_df <- read.csv(normalizePath("metadata/pollen_data/database.csv"))
datasets_ids_df <- datasets_ids_df |> dplyr::filter(Database == "Neotoma") |> select(Site_name_machine_readable,dataset_id,Pollen)

# Ensure site names are character
datasets_ids_df$Site_name_machine_readable <- as.character(datasets_ids_df$Site_name_machine_readable)
datasets_ids_df$Pollen <- as.character(datasets_ids_df$Pollen)

# Create empty list
neotoma_sites_list <- list()

# Loop over ids to download every site
for (i in seq_len(nrow(datasets_ids_df))) {
  dataset_id <- datasets_ids_df$dataset_id[i]  # Corrected column name
  site_name <- datasets_ids_df$Site_name_machine_readable[i]  # Corrected column name
  pollen_type <- datasets_ids_df$Pollen[i]  # Extract pollen type
  
  print(paste("Processing Site:", site_name, " (ID:", dataset_id, ", Pollen:", pollen_type, ")"))  # Track progress
  
  neotoma_site <- tryCatch({
    get_downloads(dataset_id, all_data = TRUE)
  }, error = function(e) {
    print(paste("Error with Site:", site_name, "(ID:", dataset_id, ") ->", e))
    NULL
  })
  
  neotoma_sample <- samples(neotoma_site)  # Extract samples
  
  # Store in list using site name as key, also keeping pollen type
  neotoma_sites_list[[site_name]] <- list(
    samples = neotoma_sample,
    pollen_type = pollen_type
  )
}

## 1.2) Modify neotoma files to match APD structure and save it in their corresponding folder (modern/fossil) -----
# Reshape dataframes
df_list <- lapply(names(neotoma_sites_list), function(site_name) {
  site_data <- neotoma_sites_list[[site_name]]  # Extract the nested list
  pollen_type <- site_data$pollen_type         # Extract pollen type
  
  df <- site_data$samples  # Extract the samples dataframe
  
  # Select important columns
  df <- df |> select(age, agetype, ageolder, ageyounger, variablename, depth, value)
  df$value <- as.numeric(df$value)  # Ensure value is numeric
  
  # Extract unique agetype value
  unique_agetype <- unique(df$agetype)
  
  # Pivot the data: Make variablename values into columns
  neotoma_wide <- df %>%
    pivot_wider(
      names_from = variablename,  # Make each pollen type a column
      values_from = value,        # Use the "value" column for new values
    ) %>%
    select(-agetype)  # Remove agetype column after use
  
  # Rename "age" column to include `agetype`
  colnames(neotoma_wide)[colnames(neotoma_wide) == "age"] <- paste0("age_", gsub(" ", "_", unique_agetype))
  
  # Rename "ageolder" and "ageyounger" columns
  colnames(neotoma_wide)[which(colnames(neotoma_wide) %in% c("ageolder", "ageyounger"))] <- c("ageolder_BP", "ageyounger_BP")
  
  # Return modified dataframe with site name as the key
  return(list(site_name = site_name, pollen=pollen_type,  data = neotoma_wide))
})

# Organise per pollen type
fossil_list <- list()
modern_list <- list()

# Loop through each dataframe in df_list
for (i in seq_along(df_list)) {
  site_data <- df_list[[i]]  # Extract the list containing data and pollen type
  df <- site_data$data         # Extract the dataframe
  site_name <- site_data$site_name # extract site
  pollen_type <- site_data$pollen  # Extract pollen type from the list
  
  # Convert to lowercase to avoid case mismatches
  pollen_type <- tolower(pollen_type)
  
  # Assign to fossil or modern based on pollen_type
  if (pollen_type == "fossil") {
    fossil_list[[site_name]] <- df  # Use site_name instead of i
  } else if (pollen_type == "modern") {
    modern_list[[site_name]] <- df  # Use site_name instead of i
  }
}

# Save dfs into corresponding folders
fossil_folder_path <- normalizePath("data/raw_data/pollen_data/fossil/")

for (name in names(fossil_list)) {
  df <- fossil_list[[name]]
  
  # Convert list-type columns to character to prevent write.csv() errors
  df <- df %>%
    mutate(across(where(is.list), ~ sapply(., toString)))  # Convert lists to comma-separated strings
  
  # Save the dataframe as a CSV file
  write.csv(df, file = file.path(fossil_folder_path, paste0(name, ".csv")), row.names = FALSE)
}

modern_folder_path <- normalizePath("data/raw_data/pollen_data/modern/")
for (name in names(modern_list)) {
  df <- modern_list[[name]]
  
  # Convert list-type columns to character to prevent write.csv() errors
  df <- df %>%
    mutate(across(where(is.list), ~ sapply(., toString)))  # Convert lists to comma-separated strings
  
  # Save the dataframe as a CSV file
  write.csv(df, file = file.path(modern_folder_path, paste0(name, ".csv")), row.names = FALSE)
}

# 2. Extract pollen types from raw files (for taxonomic harmonisation) ----

## 2.1) Fossil records ----
# Set the directory containing your CSV files
folder_path <- normalizePath("data/raw_data/pollen_data/fossil")

# Get a list of all CSV files in the directory
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize a list to store taxa
raw_taxa_list <- list()

# Loop through each file and obtain taxa names
for (file in file_list) {
  # Read the file into R
  df <- readr::read_csv(file, locale = locale(encoding = "latin1"))
  
  # Extract pollen types names (column names)
  raw_taxa <- colnames(df)
  
  # Clean names
  raw_taxa <- trimws(raw_taxa)  # Trim leading/trailing whitespace
  raw_taxa <- gsub(" {2,}", " ", raw_taxa)  # Remove double spaces (replace with a single space)
  
  # Add names to list
  raw_taxa_list[[file]] <- raw_taxa # [[]] because it is a list
}

# Eliminate duplicates in list
raw_taxa_list <- unique(unlist(raw_taxa_list)) # we need to unlist before eliminating duplicates

## 2.2) Modern records ----
# Set the directory containing your CSV files
folder_path <- normalizePath("data/raw_data/pollen_data/modern")

# Get a list of all CSV files in the directory
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize a list to store taxa
raw_taxa_list_modern <- list()

# Loop through each file and obtain taxa names
for (file in file_list) {
  # Read the file into R
  df <- readr::read_csv(file, locale = locale(encoding = "latin1"))
  
  # Extract pollen types names (column names)
  raw_taxa <- colnames(df)
  
  # Clean names
  raw_taxa <- trimws(raw_taxa)  # Trim leading/trailing whitespace
  
  raw_taxa <- gsub(" {2,}", " ", raw_taxa)  # Remove double spaces (replace with a single space)
  
  # Add names to list
  raw_taxa_list_modern[[file]] <- raw_taxa # [[]] because it is a list
}

# Eliminate duplicates in list
raw_taxa_list_modern <- unique(unlist(raw_taxa_list_modern)) # we need to unlist before eliminating duplicates

# 3. Combine lists and save final list ----
raw_taxa <- c(raw_taxa_list,raw_taxa_list_modern)
raw_taxa <- unique(unlist(raw_taxa)) # we need to unlist before eliminating duplicates

# Convert to dataframe
raw_taxa <- as.data.frame(raw_taxa)
colnames(raw_taxa) <- "Original_taxa" 

# Eliminate non-taxa names
unnecessary_rows <- grepl("Fossilva|MADCAP|BP|Depth|depth|sample_name|C14|Age|Calendar|Volume|volume|Cuticles|Counted|diatoms|error|element|Chronology|Protist|grammi|Lab|Layer|lithology|Licopods|Mark|concentration|accumulation|lycopodium|Mass|name|Lycopod|Markers|Burial|sample|Sample|added|Sedimentation|Site|flux|spike|Spike|Sum|Taxonomic|gram|Year|Total|year|weight|code|sum|SUM|COUNTS|thickness|Thickness|code|shrubs|trees|herbs", raw_taxa$Original_taxa)# Identify rows containing "BP" in their names
raw_taxa <- raw_taxa[!unnecessary_rows, , drop = FALSE]

# Save raw taxa list
write.csv(raw_taxa, file = normalizePath("data/raw_data/taxonomy/raw_taxa_list/raw_pollen_types.csv"), row.names = TRUE)
