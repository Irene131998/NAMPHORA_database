## Script for including published harmonisation lists and number of pollen sequences per pollen type to harmonised_taxonomy_list


#--------------------------------------------------------#
# 0. Load libraries and functions ----
#--------------------------------------------------------#

source("scripts/functions.R")

libraries <- c("readr", "tidyr", "dplyr", "readxl", "openxlsx")

# Install missing packages
invisible(lapply(libraries, install_if_missing))

# Load the libraries
lapply(libraries, require, character.only = TRUE)



#--------------------------------------------------------#
# 1. Add number of pollen sequences per original pollen type ----
#--------------------------------------------------------#


## 1.1) Fossil sequences ----

# Set the directory containing your CSV files
folder_path <- normalizePath("data/raw_data/pollen_data/fossil")

# Get a list of all CSV files in the directory
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Initialise a list to store taxa and their counts
taxa_counts <- list()

# Loop through each file and obtain taxa names
for (file in file_list) {
  # Read the file into R
  df <- readr::read_csv(file, locale = locale(encoding = "latin1"))
  
  # Extract pollen types names (column names)
  raw_taxa <- colnames(df)
  
  # Clean names
  raw_taxa <- trimws(raw_taxa)  # Trim whitespace
  raw_taxa <- gsub(" {2,}", " ", raw_taxa)  # Remove double spaces (replace with a single space)
  
  # Update the count of files for each taxa
  for (taxa in raw_taxa) { # iterates over each element in the raw_taxa vector (names of the taxa extracted from the column names of the CSV file)
    if (taxa %in% names(taxa_counts)) { # if the taxa is already in the list, add one more count
      taxa_counts[[taxa]] <- taxa_counts[[taxa]] + 1
    } else { # if the taxa is not in list yet, add one count
      taxa_counts[[taxa]] <- 1
    }
  }
}

# Convert the list to a dataframe
sequences_df <- data.frame(Original_taxa = names(taxa_counts), Fossil_Sequences = unlist(taxa_counts), stringsAsFactors = FALSE)


## 1.2) Modern sequences ----

# Set the directory containing your CSV files
folder_path <- normalizePath("data/raw_data/pollen_data/modern")

# Get a list of all CSV files in the directory
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Initialise a list to store taxa
raw_taxa_list_modern <- list()

# Initialise a list to store taxa and their counts
taxa_counts <- list()

# Loop through each file and obtain taxa names
for (file in file_list) {
  # Read the file into R
  df <- readr::read_csv(file, locale = locale(encoding = "latin1"))
  
  # Extract pollen types names (column names)
  raw_taxa <- colnames(df)
  
  # Clean names
  raw_taxa <- trimws(raw_taxa)  # Trim whitespace
  raw_taxa <- gsub(" {2,}", " ", raw_taxa)  # Remove double spaces (replace with a single space)
  
  # Update the count of files for each taxa
  for (taxa in raw_taxa) { # iterates over each element in the raw_taxa vector (names of the taxa extracted from the column names of the CSV file)
    if (taxa %in% names(taxa_counts)) { # if the taxa is already in the list, add one more count
      taxa_counts[[taxa]] <- taxa_counts[[taxa]] + 1
    } else { # if the taxa is not in list yet, add one count
      taxa_counts[[taxa]] <- 1
    }
  }
}

sequences_df_2 <- data.frame(Original_taxa = names(taxa_counts), Modern_Sequences = unlist(taxa_counts), stringsAsFactors = FALSE)


## 1.3) Bind dataframes by Original_taxa column ----

sequences_final <- full_join(sequences_df, sequences_df_2, by = "Original_taxa")

unnecessary_rows <- grepl("Fossilva|MADCAP|BP|Depth|depth|sample_name|C14|Age|Calendar|Volume|volume|Cuticles|Counted|diatoms|error|element|Chronology|grammi|Lab|Layer|lithology|Licopods|Mark|concentration|accumulation|lycopodium|Mass|name|Lycopod|Markers|sample|Sample|added|Sedimentation|Site|flux|Spike|Sum|Taxonomic|gram|Year|Total|year|weight", sequences_final$Original_taxa)# Identify rows containing the established pattern in column "Original_taxa"
sequences_final <- sequences_final[!unnecessary_rows, , drop = FALSE]


## 1.4) Add to harmonisation list ----

# Read taxonomy database
taxonomy_pollen_taxa <- readr::read_csv((normalizePath("data/processed_data/taxonomy/harmonised_taxonomy_list.csv")),locale = locale(encoding = "latin1"))

# Insert columns of number of sequences to taxonomy database
taxonomy_pollen_taxa <- left_join(taxonomy_pollen_taxa, sequences_final, by = "Original_taxa")

# Replace NAs with 0
taxonomy_pollen_taxa <- taxonomy_pollen_taxa |>
  mutate(across(c(Fossil_Sequences,Modern_Sequences), ~ tidyr::replace_na(.x, 0)))

# Create new column with total number of sequences
taxonomy_pollen_taxa <- taxonomy_pollen_taxa |> mutate(Total_Sequences = Fossil_Sequences + Modern_Sequences) 

names(taxonomy_pollen_taxa)


#--------------------------------------------------------#
# 2. Add number of pollen sequences per harmonised pollen type ----
#--------------------------------------------------------#

## 2.1) Fossil sequences ----

# Set the directory containing your CSV files
folder_path <- normalizePath("data/processed_data/pollen_data/fossil/harmonised_counts")

# Get a list of all CSV files in the directory
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Initialise a list to store taxa and their counts
taxa_counts <- list()

# Loop through each file and obtain taxa names
for (file in file_list) {
  # Read the file into R
  df <- readr::read_csv(file, locale = locale(encoding = "latin1"))
  
  # Extract pollen types names (column names)
  raw_taxa <- colnames(df)
  
  # Clean names
  raw_taxa <- trimws(raw_taxa)  # Trim whitespace
  raw_taxa <- gsub(" {2,}", " ", raw_taxa)  # Remove double spaces (replace with a single space)
  
  # Update the count of files for each taxa
  for (taxa in raw_taxa) { # iterates over each element in the raw_taxa vector (names of the taxa extracted from the column names of the CSV file)
    if (taxa %in% names(taxa_counts)) { # if the taxa is already in the list, add one more count
      taxa_counts[[taxa]] <- taxa_counts[[taxa]] + 1
    } else { # if the taxa is not in list yet, add one count
      taxa_counts[[taxa]] <- 1
    }
  }
}

# Convert the list to a dataframe
sequences_df <- data.frame(Pollen_type_harmonised = names(taxa_counts), Fossil_Sequences_Pollen_type_harmonised = unlist(taxa_counts), stringsAsFactors = FALSE)


## 2.2) Modern sequences ----

# Set the directory containing your CSV files
folder_path <- normalizePath("data/processed_data/pollen_data/modern/harmonised_counts")

# Get a list of all CSV files in the directory
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Initialise a list to store taxa
raw_taxa_list_modern <- list()

# Initialise a list to store taxa and their counts
taxa_counts <- list()

# Loop through each file and obtain taxa names
for (file in file_list) {
  # Read the file into R
  df <- readr::read_csv(file, locale = locale(encoding = "latin1"))
  
  # Extract pollen types names (column names)
  raw_taxa <- colnames(df)
  
  # Clean names
  raw_taxa <- trimws(raw_taxa)  # Trim whitespace
  raw_taxa <- gsub(" {2,}", " ", raw_taxa)  # Remove double spaces (replace with a single space)
  
  # Update the count of files for each taxa
  for (taxa in raw_taxa) { # iterates over each element in the raw_taxa vector (names of the taxa extracted from the column names of the CSV file)
    if (taxa %in% names(taxa_counts)) { # if the taxa is already in the list, add one more count
      taxa_counts[[taxa]] <- taxa_counts[[taxa]] + 1
    } else { # if the taxa is not in list yet, add one count
      taxa_counts[[taxa]] <- 1
    }
  }
}

sequences_df_2 <- data.frame(Pollen_type_harmonised = names(taxa_counts), Modern_Sequences_Pollen_type_harmonised = unlist(taxa_counts), stringsAsFactors = FALSE)


## 2.3) Bind dataframes by Pollen_type_harmonised column ----

sequences_final <- full_join(sequences_df, sequences_df_2, by = "Pollen_type_harmonised")

unnecessary_rows <- grepl("Fossilva|MADCAP|BP|Depth|depth|sample_name|C14|Age|Calendar|Volume|volume|Cuticles|Counted|diatoms|error|element|Chronology|grammi|Lab|Layer|lithology|Licopods|Mark|concentration|accumulation|lycopodium|Mass|name|Lycopod|Markers|sample|Sample|added|Sedimentation|Site|flux|Spike|Sum|Taxonomic|gram|Year|Total|year|weight", sequences_final$Pollen_type_harmonised)# Identify rows containing the established pattern in column "Pollen_type_harmonised"
sequences_final <- sequences_final[!unnecessary_rows, , drop = FALSE]

## 2.4) Add to harmonisation list ----

# Insert columns of number of sequences to taxonomy database
taxonomy_pollen_taxa <- left_join(taxonomy_pollen_taxa, sequences_final, by = "Pollen_type_harmonised")

# Replace NAs with 0
taxonomy_pollen_taxa <- taxonomy_pollen_taxa  |> 
  mutate(across(c(Fossil_Sequences_Pollen_type_harmonised,Modern_Sequences_Pollen_type_harmonised), ~ tidyr::replace_na(.x, 0)))

# Create new column with total number of sequences
taxonomy_pollen_taxa <- taxonomy_pollen_taxa |> mutate(Total_Sequences_Pollen_type_harmonised = Fossil_Sequences_Pollen_type_harmonised + Modern_Sequences_Pollen_type_harmonised) 

#--------------------------------------------------------#
# 3. Add published harmonisation tables to harmonisation list ----
#--------------------------------------------------------#

## 3.1) Read data ----

# Read Lezine 2009 data
lezine_list <- readxl::read_excel(normalizePath("data/raw_data/taxonomy/harmonisation_lists/Lezine_2009_pollen_types.xls")) 

# Read Mottl et al. 2020 data
mottl_list <- read.csv(normalizePath("data/raw_data/taxonomy/harmonisation_lists/Mottl_etal_Africa_HarmonizationTable.csv"))

# Read APD list
apd_list <- readr::read_csv(normalizePath("data/raw_data/taxonomy/harmonisation_lists/APD_dictionnary_export.csv")) 

## 3.2) Add Lezine (2009) list ----

# Set the first row as the column names
colnames(lezine_list) <- lezine_list[1,]

# Eliminate first row
lezine_list <- lezine_list[-1,]

# Change column names for joining
lezine_list <- lezine_list |> rename(Original_taxa = "POLLEN TAXON (original)")
lezine_list <- lezine_list |> rename(Pollen_type_harmonised_Lezine_2009 = "POLLEN TAXON (grouped)")

# Select essential columns
lezine_list_harmonisation <- lezine_list[, c("Original_taxa", "Pollen_type_harmonised_Lezine_2009")]

# Join with harmonisation list 
taxonomy_pollen_taxa <- taxonomy_pollen_taxa |> 
  dplyr::left_join(lezine_list_harmonisation)


## 3.3) Add Mottl (2020) list ----

# Change column names for joining
mottl_list <- mottl_list |> rename(Original_taxa = "taxon.name")
mottl_list <- mottl_list |> rename(Pollen_type_harmonised_Mottl_2020 = "Proposed_harmonised_names")

# Select essential columns
mottl_list <- mottl_list[, c("Original_taxa", "Pollen_type_harmonised_Mottl_2020")]

# Left join files
taxonomy_pollen_taxa <- taxonomy_pollen_taxa |> 
  dplyr::left_join(mottl_list)

## 3.4) Add APD list ----

# Change column names for joining
apd_list <- apd_list |> rename(Original_taxa = "Taxon (original name)")
apd_list <- apd_list |> rename(Pollen_type_harmonised_APD = "Taxon (revised nomenclature)")

# Select essential columns
apd_list_harmonisation <- apd_list[, c("Original_taxa", "Pollen_type_harmonised_APD")]

# Left join files
taxonomy_pollen_taxa <- taxonomy_pollen_taxa |> 
  dplyr::left_join(apd_list_harmonisation)


## 3.5) Save taxonomy with harmonisation lists and number of pollen sequences per pollen type ----

write.table(
  taxonomy_pollen_taxa, 
  file = normalizePath("data/processed_data/taxonomy/harmonised_taxonomy_list.csv"),
  sep = ",",         
  row.names = FALSE, 
  fileEncoding = "latin1"  # Ensures special characters are correctly saved
)

