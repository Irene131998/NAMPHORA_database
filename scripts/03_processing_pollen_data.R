## Script for processing raw pollen data (addition of harmonised pollen names and recalibrated dates, and calculate %)


# ! BEFORE RUNNING THIS, CHECK THE PART OF  (starts_with("V")) IN 1.3) MODERN RECORDS ----
# CHECK HAS_DECIMALS FUNCTION (IT HAS THE starts_with("V") PART)


# 0. Load libraries and functions----

source("scripts/functions.R")
libraries <- c("readxl", "readr", "dplyr", "tidyr", "tibble", "stringr", "tools")

# Install missing packages
invisible(lapply(libraries, install_if_missing))

# Load the libraries
lapply(libraries, require, character.only = TRUE)

# 1. Add new harmonised taxa names to each individual files (sequences)----

## 1.1) Get harmonisation list----

# Read Taxonomy database
taxonomy_pollen_taxa <- readr::read_csv(normalizePath("data/processed_data/taxonomy/harmonised_taxonomy_list.csv"), locale = locale(encoding = "latin1"))

# Select columns
harmonisation_list <- taxonomy_pollen_taxa |> select("Original_taxa","Pollen_type_SM_morphological")

## 1.2) Fossil records----

# Get the list of file paths
folder_path <- normalizePath("data/raw_data/pollen_data/fossil")
file_paths <- list.files(path=folder_path, full.names = TRUE)

# Directory to save the files
output_dir <- normalizePath("data/processed_data/pollen_data/fossil/harmonised_counts")

# Create the directory if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

for (file_path in file_paths) {
  # Read the file into R
  df <- readr::read_csv(file_path, locale = locale(encoding = "latin1"))
  
  # Clean taxa names
  colnames(df) <- colnames(df) %>% 
    trimws() %>%  # Trim whitespace
    gsub("\\s{2,}", " ", .) # Remove double spaces (replace with a single space)
  
  # Transpose df so the taxa names are in the same column, in order to join with harmonised list
  df_t <- t(df) |> as.data.frame() |> tibble::rownames_to_column(var = "Original_taxa")
  
  # Left join by "Original_taxa"
  harmonised_df <- df_t |> 
    dplyr::left_join(harmonisation_list)
  
  # Move harmonised names to first column
  harmonised_df <- harmonised_df %>%
    select(last_col(), everything())
  
  # Fill missing taxa names using "Original_taxa" (get depth/sample/age columns back)
  harmonised_df <- harmonised_df %>%
    mutate(Pollen_type_SM_morphological = 
             coalesce(Pollen_type_SM_morphological, Original_taxa))# coalesce() fills NA values in Pollen_type_SM_morphological with Original_taxa values
  
  harmonised_df <- harmonised_df |> select(-Original_taxa)
  
  # Convert to numeric (if the numbers are stored as characters)
  numeric_df <- harmonised_df %>% 
    group_by(Pollen_type_SM_morphological) %>%
    dplyr::filter(!str_detect(Pollen_type_SM_morphological, "(?i)depth|sample|site|BP|AD/BC|Age")) %>%
    mutate(across(where(is.character), as.numeric))
  
  character_df <- harmonised_df %>% 
    dplyr::filter(str_detect(Pollen_type_SM_morphological, "(?i)depth|sample|site|BP|AD/BC|Age")) # (?i) = case insensitive
  
  # Combine rows from same harmonised taxa
  numeric_df<- numeric_df %>%
    group_by(Pollen_type_SM_morphological) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
  
  # Bind
  total_df <- rbind(character_df,numeric_df)
  
  # Transpose again to original shape
  df_final <- t(total_df) |> as.data.frame()
  
  # Set colnames as the first row (where taxa names are)
  colnames(df_final) <- df_final[1,]
  df_final <- df_final[-1,]
  
  # Reset row names
  rownames(df_final) <- NULL
  
  # Extract the file name without extension
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Define the file name and path for the chronology dataframe
  file_name <- paste0(file_name, ".csv")
  file_path <- file.path(output_dir, file_name)
  
  # Save the dataframe
  write.csv(df_final, file = file_path, row.names = FALSE)
}


## 1.3) Modern records----

# Get the list of file paths
folder_path <- normalizePath("data/raw_data/pollen_data/modern")
file_paths <- list.files(path=folder_path, full.names = TRUE)

# Exclude "desktop.ini"
file_paths <- file_paths[!basename(file_paths) %in% "desktop.ini"]

# Directory to save the files
output_dir <- normalizePath("data/processed_data/pollen_data/modern/harmonised_counts")

# Create the directory if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

for (file_path in file_paths) {
  # Read the file into R
  df <- readr::read_csv(file_path, locale = locale(encoding = "latin1"))
  
  # Clean taxa names
  colnames(df) <- colnames(df) %>% 
    trimws() %>%  # Trim  whitespace
    gsub("\\s{2,}", " ", .) # Remove double spaces (replace with a single space)
  
  # Eliminate column with BP (here is collection date & we do not need it)
  df <- df |> select(-matches("BP"))
  
  # Transpose df so the taxa names are in the same column, in order to join with harmonised list
  df_t <- t(df) |> as.data.frame() |> tibble::rownames_to_column(var = "Original_taxa")
  
  # Left join by "Original_taxa"
  harmonised_df <- df_t |> 
    dplyr::left_join(harmonisation_list)
  
  # Move harmonised names to first column
  harmonised_df <- harmonised_df %>%
    select(last_col(), everything())  # Moves last column to first position
  
  # Fill missing taxa names using "Original_taxa" (get depth/sample/age columns back)
  harmonised_df <- harmonised_df %>%
    mutate(Pollen_type_SM_morphological = 
             coalesce(Pollen_type_SM_morphological, Original_taxa))# coalesce() fills NA values in Pollen_type_SM_morphological with Original_taxa values
  
  harmonised_df <- harmonised_df |> select(-Original_taxa)
  
  # Convert to numeric (if the numbers are stored as characters)
  harmonised_df <- harmonised_df |> mutate(across(starts_with("V"), as.numeric))
  
  # Combine rows from same harmonised taxa
  harmonised_df<- harmonised_df %>%
    group_by(Pollen_type_SM_morphological) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
  
  # Transpose again to original shape
  df_final <- t(harmonised_df) |> as.data.frame()
  
  # Set colnames as the first row (where taxa names are)
  colnames(df_final) <- df_final[1,]
  df_final <- df_final[-1,]
  
  # Reset row names
  rownames(df_final) <- NULL
  
  # Extract the file name without extension
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Define the file name and path for the chronology dataframe
  file_name <- paste0(file_name, ".csv")
  file_path <- file.path(output_dir, file_name)
  
  # Save the dataframe
  write.csv(df_final, file = file_path, row.names = FALSE)
}


# 2. Add calibrated dates to harmonised pollen records----

# Define the two directories
dir_calibration <- normalizePath("data/processed_data/age_calibrated")
dir_pollen_records <- normalizePath("data/processed_data/pollen_data/fossil/harmonised_counts")

# List all CSV files in the first directory
csv_files_dir_calibration <- list.files(path = dir_calibration, pattern = "\\.csv$", full.names = TRUE)

# Empty list to store warnings
log_messages <- list()

# Loop through the CSV files in the first directory
for (csv_file in csv_files_dir_calibration) {
  # Extract the file name from the path
  file_name <- basename(csv_file)
  
  # Define the corresponding file in the second directory
  corresponding_file <- file.path(dir_pollen_records, file_name)
  
  # Check if the corresponding file exists in the second directory
  if (file.exists(corresponding_file)) {
    # Read both CSV files
    df1 <- readr::read_csv(csv_file, locale = locale(encoding = "latin1")) # Calibrated dates
    df2 <- readr::read_csv(corresponding_file, locale = locale(encoding = "latin1")) # Pollen record
    
    # Standardise column name to 'depth' if necessary
    df1 <- df1 |> rename(depth = any_of(c("Depth","depth cm","Depth cm","Depth (cm)","depth","Depth (cm) [cm]","depthcm","depth (cm) [cm]","location/depht (cm)","Depth (cm) (rounded)")))
    df2 <- df2 |> rename(depth = any_of(c("Depth","depth cm","Depth cm","Depth (cm)","depth","Depth (cm) [cm]","depthcm","depth (cm) [cm]","location/depht (cm)","Depth (cm) (rounded)")))
    
    # Select the depth and median columns from calibrated files
    df1 <- df1 |> select(depth, matches("median"))
    
    # Name recalibrated column
    df1 <- df1 |> rename_with(~ "recal_median_BP", matches("median"))
    
    # Combine by depth
    if ("depth" %in% colnames(df2)) {
      
      # Join both dfs
      if (is.numeric(df1$depth) && is.numeric(df2$depth)) { # Numeric depths
        combined_df <- left_join(df2,df1, by = "depth")
        
      } else if (is.character(df1$depth) && is.character(df2$depth)) { # Samples (no depths)
        combined_df <- left_join(df2,df1, by = "depth")
        
      } 
    } else {
      log_messages <- append(log_messages, paste(file_name, "could not be joined: 'depth' column missing in df2"))
    }
    
    
    # Define the output file path 
    output_file <- file.path(dir_pollen_records, file_name) 
    
    # Save the combined data to a new CSV file
    write.csv(combined_df, output_file, row.names = FALSE)
    
    # Store success message
    log_messages <- append(log_messages, paste("Combined and saved:", output_file))
  } else {
    log_messages <- append(log_messages, paste("No matching file for:", file_name))
  }
}



# 3. Percentage calculation----

## 3.1) Get habit list----


habit_list <- taxonomy_pollen_taxa |> select("Pollen_type_SM_morphological","Habit_summarised (for percentage calculation)")

habit_list <- habit_list |> rename(Habit_summarised="Habit_summarised (for percentage calculation)")

habit_list <- habit_list |> unique() |> na.omit()


## 3.2) Calculate base and total sums and percentages----

### 3.2.1) Fossil pollen----


# Get the list of file paths
folder_path <- normalizePath("data/processed_data/pollen_data/fossil/harmonised_counts")
file_paths <- list.files(path=folder_path, full.names = TRUE)

# Directory to save the files
output_dir <- normalizePath("data/processed_data/pollen_data/fossil/harmonised_percentages")

# Create the directory if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

for (file_path in file_paths) {
  # Read the file into R
  df <- readr::read_csv(file_path, locale = locale(encoding = "latin1"))
  
  # Clean taxa names
  colnames(df) <- colnames(df) %>% 
    trimws() %>%  # Trim whitespace
    gsub("\\s{2,}", " ", .) # Remove double spaces (replace with a single space)
  
  # Remove the Pollen Sum column if it exists in the original dataframe.
  if (any(colnames(df) %in% c("Pollen sum", "Total sum"))) {
    df <- df |> select(-one_of(c("Pollen sum", "Total sum")))
  }
  
  # Replace NA values to 0
  df <- df %>%
    mutate(across(
      .cols = !matches("depth|BP"),   # all columns except those with "depth" or "BP"
      ~ replace_na(., 0)
    ))
  
  
  # Transpose df so the taxa names are in the same column, in order to join with habit list
  df_t <- t(df) |> as.data.frame() |> tibble::rownames_to_column(var = "Pollen_type_SM_morphological") # now columns are named V1,V2,etc.
  
  # Left join by
  harmonised_df <- df_t |> 
    dplyr::left_join(habit_list)
  
  # Eliminate columns that have 0/NA in all rows
  harmonised_df <- harmonised_df %>%
    select_if(~ any(!is.na(.)) & any(. != 0, na.rm = TRUE))
  
  # 1) Calculate pollen sum (Trees + Shrubs + Herbs) ----
  pollen_sum <- harmonised_df %>%
    dplyr::filter(Habit_summarised %in% c("Trees", "Shrubs", "Herbs", 
                                          "Herbs or Shrubs or Trees", "Herbs or Shrubs", 
                                          "Shrubs or Trees", "Geophyte")) %>%
    mutate(across(starts_with("V"),as.numeric))
  
  # Sum the columns
  depth_sums <- colSums(select_if(pollen_sum, is.numeric), na.rm = TRUE)
  
  # Create a new row with the sum values
  new_row <- c("Pollen sum", depth_sums)
  
  # Add the new row to the harmonised dataframe
  harmonised_df_sums <- rbind(harmonised_df, new_row) 
  
  # 2) Calculate total sum (Trees + Shrubs + Herbs + Aquatics + Indeterminable)----
  total_sum <- harmonised_df %>%
    dplyr::filter(Habit_summarised %in% c( "Trees", "Shrubs or Trees", "Herbs or Shrubs or Trees", "Indeterminable", "Herbs or Shrubs", "Bryophyta", "Herbs", "Fern", "Algae", "Aquatic", "Geophyte","Fungi", "Parasitic", "Shrubs","Hepaticae", "Monolete","Selaginellaceae", "Trilete", "Unknown")) %>%
    mutate(across(starts_with("V"),as.numeric))
  
  # Sum the columns
  total_sum <- colSums(select_if(total_sum, is.numeric), na.rm = TRUE)
  
  # Create a new row with the sum values
  total_row <- c("Total sum", total_sum)
  
  # Add the new row to the harmonised dataframe
  harmonised_df_sums <- rbind(harmonised_df_sums, total_row) 
  
  # Set the first column as the row names (row names are now the taxa names)
  row.names(harmonised_df_sums) <- harmonised_df_sums[,1]
  
  # 3) Calculate percentages (over pollen sum) ----
  percentages_pollen_sum <- harmonised_df_sums %>% dplyr::filter(Habit_summarised %in% c( "Trees", "Shrubs or Trees", "Herbs or Shrubs or Trees", "Herbs or Shrubs", "Bryophyta", "Herbs", "Fern", "Algae", "Aquatic", "Geophyte","Fungi", "Parasitic", "Shrubs","Hepaticae", "Monolete","Selaginellaceae", "Trilete")) %>%
    mutate(across(starts_with("V"), as.numeric))
  
  # Identify the "Pollen sum" row
  pollen_sum_row <- harmonised_df_sums %>%
    dplyr::filter(Pollen_type_SM_morphological == "Pollen sum") %>%
    select(starts_with("V")) %>%
    mutate(across(starts_with("V"), as.numeric))
  
  # Proceed only if no decimals found in percentages_pollen_sum data
  if (!has_decimals(percentages_pollen_sum)) {
    # Compute percentages by dividing each value by the "Pollen sum" row values
    percentages_pollen_sum <- percentages_pollen_sum %>% 
      mutate(across(starts_with("V"), ~ . / pollen_sum_row[1, cur_column()] * 100)) # [1, cur_column()] selects the value from the "Pollen sum" row in the same column (.)
    
    # Round %
    percentages_pollen_sum <- percentages_pollen_sum %>%
      mutate(across(starts_with("V"), ~ round(.x, 3)))  
  } else {
    # If decimals found, skip percentage calculation
    percentages_pollen_sum <- percentages_pollen_sum
  }
  
  # 4) Calculate percentages (over total sum: Indeterminable & Unknown) ----
  percentages_total_sum <- harmonised_df_sums %>%
    dplyr::filter(Habit_summarised %in% c("Unknown", "Indeterminable"))%>%
    mutate(across(starts_with("V"), as.numeric))
  
  # Identify the "Total sum" row
  total_sum_row <- harmonised_df_sums %>%
    dplyr::filter(Pollen_type_SM_morphological == "Total sum") %>%
    select(starts_with("V")) %>%
    mutate(across(starts_with("V"), as.numeric))
  
  # Proceed only if no decimals found in percentages_total_sum data
  if (!has_decimals(percentages_total_sum)) {
    # Compute percentages by dividing each value by the "Total sum" row values
    percentages_total_sum <- percentages_total_sum %>%
      mutate(across(starts_with("V"), ~ . / total_sum_row[1, cur_column()] * 100)) # [1, cur_column()] selects the value from the "total sum" row in the same column (.)
    
    # Round %
    percentages_total_sum <- percentages_total_sum %>%
      mutate(across(starts_with("V"), ~ round(.x, 3)))  
  } else {
    # If decimals found, skip percentage calculation, optionally keep original data or handle differently
    percentages_total_sum <- percentages_total_sum
  }
  
  # Join together
  percentages_df <- rbind(percentages_pollen_sum,percentages_total_sum)
  
  # Join depth, sums and BP information from original df
  original_df <- harmonised_df_sums%>%
    dplyr::filter(str_detect(Pollen_type_SM_morphological, "(?i)depth|sample|site|BP|AD/BC|Age|Pollen sum|Total sum")) # (?i) = case insensitive
  
  final_df <- rbind(percentages_df,original_df)
  final_df <- final_df |> select(-c(Habit_summarised,Pollen_type_SM_morphological))
  
  # Transpose again to original shape
  final_df_t <- t(final_df) |> as.data.frame() 
  
  # Reset row names
  rownames(final_df_t) <- NULL
  
  # Extract the file name without extension
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Define the file name and path for the chronology dataframe
  file_name <- paste0(file_name, ".csv")
  file_path <- file.path(output_dir, file_name)
  
  # Save the dataframe
  write.csv(final_df_t, file = file_path, row.names = FALSE)
}


### 3.2.2) Modern pollen----

# Get the list of file paths
folder_path <- normalizePath("data/processed_data/pollen_data/modern/harmonised_counts")
file_paths <- list.files(path=folder_path, full.names = TRUE)

# Directory to save the files
output_dir <- normalizePath("data/processed_data/pollen_data/modern/harmonised_percentages")

# Create the directory if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

for (file_path in file_paths) {
  # Read the file into R
  df <- readr::read_csv(file_path, locale = locale(encoding = "latin1"))
  
  # Clean taxa names
  colnames(df) <- colnames(df) %>% 
    trimws() %>%  # Trim  whitespace
    gsub("\\s{2,}", " ", .) # Remove double spaces (replace with a single space)
  
  # Remove the Pollen Sum column if it exists in the original dataframe.
  if (any(colnames(df) %in% c("Pollen sum", "Total sum"))) {
    df <- df |> select(-one_of(c("Pollen sum", "Total sum")))
  }
  
  # Replace NA values to 0
  df <- df %>%
    mutate(across(
      .cols = !matches("depth|BP"),   # all columns except those with "depth" or "BP"
      ~ replace_na(., 0)
    ))
  
  
  # Transpose df so the taxa names are in the same column, in order to join with habit list
  df_t <- t(df) |> as.data.frame() |> tibble::rownames_to_column(var = "Pollen_type_SM_morphological")# now columns are named V1,V2,etc.
  
  # Left join by
  harmonised_df <- df_t |> 
    dplyr::left_join(habit_list)
  
  # Eliminate columns that have 0/NA in all rows
  harmonised_df <- harmonised_df %>%
    select_if(~ any(!is.na(.)) & any(. != 0, na.rm = TRUE))
  
  # 1) Calculate pollen sum (Trees + Shrubs + Herbs) ----
  pollen_sum <- harmonised_df %>%
    filter(Habit_summarised %in% c("Trees", "Shrubs", "Herbs", 
                                   "Herbs or Shrubs or Trees", "Herbs or Shrubs", 
                                   "Shrubs or Trees", "Geophyte")) %>%
    mutate(across(starts_with("V"),as.numeric))
  
  # Sum the columns
  depth_sums <- colSums(select_if(pollen_sum, is.numeric), na.rm = TRUE)
  
  # Create a new row with the sum values
  new_row <- c("Pollen sum", depth_sums)
  
  # Add the new row to the harmonised dataframe
  harmonised_df_sums <- rbind(harmonised_df, new_row) 
  
  # 2) Calculate total sum (Trees + Shrubs + Herbs + Aquatics + Indeterminable)----
  total_sum <- harmonised_df %>%
    filter(Habit_summarised %in% c( "Trees", "Shrubs or Trees", "Herbs or Shrubs or Trees", "Indeterminable", "Herbs or Shrubs", "Bryophyta", "Herbs", "Fern", "Algae", "Aquatic", "Geophyte","Fungi", "Parasitic", "Shrubs","Hepaticae", "Monolete","Selaginellaceae", "Trilete", "Unknown")) %>%
    mutate(across(starts_with("V"),as.numeric))
  
  # Sum the columns
  total_sum <- colSums(select_if(total_sum, is.numeric), na.rm = TRUE)
  
  # Create a new row with the sum values
  total_row <- c("Total sum", total_sum)
  
  # Add the new row to the harmonised dataframe
  harmonised_df_sums <- rbind(harmonised_df_sums, total_row) 
  
  # Set the first column as the row names (row names are now the taxa names)
  row.names(harmonised_df_sums) <- harmonised_df_sums[,1]
  
  
  # 3) Calculate percentages (over pollen sum) ----
  percentages_pollen_sum <- harmonised_df_sums %>% filter(Habit_summarised %in% c( "Trees", "Shrubs or Trees", "Herbs or Shrubs or Trees", "Herbs or Shrubs", "Bryophyta", "Herbs", "Fern", "Algae", "Aquatic", "Geophyte","Fungi", "Parasitic", "Shrubs","Hepaticae", "Monolete","Selaginellaceae", "Trilete")) %>%
    mutate(across(starts_with("V"), as.numeric))
  
  # Identify the "Pollen sum" row
  pollen_sum_row <- harmonised_df_sums %>%
    filter(Pollen_type_SM_morphological == "Pollen sum") %>%
    select(starts_with("V")) %>%
    mutate(across(starts_with("V"), as.numeric))

  # Proceed only if no decimals found in percentages_pollen_sum data
  if (!has_decimals(percentages_pollen_sum)) {
    # Compute percentages by dividing each value by the "Pollen sum" row values
    percentages_pollen_sum <- percentages_pollen_sum %>% 
      mutate(across(starts_with("V"), ~ . / pollen_sum_row[1, cur_column()] * 100)) # [1, cur_column()] selects the value from the "Pollen sum" row in the same column (.)
    
    # Round %
    percentages_pollen_sum <- percentages_pollen_sum %>%
      mutate(across(starts_with("V"), ~ round(.x, 3)))  
  } else {
    # If decimals found, skip percentage calculation
    percentages_pollen_sum <- percentages_pollen_sum
  }
  
  
  # 4) Calculate percentages (over total sum: Indeterminable & Unknown) ----
  percentages_total_sum <- harmonised_df_sums %>%
    filter(Habit_summarised %in% c("Unknown", "Indeterminable"))%>%
    mutate(across(starts_with("V"), as.numeric))
  
  # Identify the "Total sum" row
  total_sum_row <- harmonised_df_sums %>%
    filter(Pollen_type_SM_morphological == "Total sum") %>%
    select(starts_with("V")) %>%
    mutate(across(starts_with("V"), as.numeric))
  
  # Proceed only if no decimals found in percentages_total_sum data
  if (!has_decimals(percentages_total_sum)) {
    # Compute percentages by dividing each value by the "Total sum" row values
    percentages_total_sum <- percentages_total_sum %>%
      mutate(across(starts_with("V"), ~ . / total_sum_row[1, cur_column()] * 100)) # [1, cur_column()] selects the value from the "total sum" row in the same column (.)
    
    # Round %
    percentages_total_sum <- percentages_total_sum %>%
      mutate(across(starts_with("V"), ~ round(.x, 3)))  
  } else {
    # If decimals found, skip percentage calculation, optionally keep original data or handle differently
    percentages_total_sum <- percentages_total_sum
  }
  
  # Join together
  percentages_df <- rbind(percentages_pollen_sum,percentages_total_sum)
  
  # Join depth, sums and BP information from original df
  original_df <- harmonised_df_sums%>%
    filter(str_detect(Pollen_type_SM_morphological, "(?i)depth|sample|site|BP|AD/BC|Age|Pollen sum|Total sum")) # (?i) = case insensitive
  
  final_df <- rbind(percentages_df,original_df)
  final_df <- final_df |> select(-c(Habit_summarised,Pollen_type_SM_morphological))
  
  # Transpose again to original shape
  final_df_t <- t(final_df) |> as.data.frame() 
  
  # Reset row names
  rownames(final_df_t) <- NULL
  
  # Extract the file name without extension
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Define the file name and path for the chronology dataframe
  file_name <- paste0(file_name, ".csv")
  file_path <- file.path(output_dir, file_name)
  
  # Save the dataframe
  write.csv(final_df_t, file = file_path, row.names = FALSE)
}

