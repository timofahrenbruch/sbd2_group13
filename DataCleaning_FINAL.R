# ==============================================================================
# DATA CLEANING SCRIPT - DIGITEC LIVE DATA
# ==============================================================================
# Purpose: Clean and prepare Digitec sales data for analysis
# ==============================================================================

# Clear environment
rm(list = ls())

# Set working directory
setwd("data")

# ==============================================================================
# LOAD LIBRARIES
# ==============================================================================

library(readr)      # Read CSV files
library(readxl)     # Read Excel files
library(stringr)    # String manipulation
library(dplyr)      # Data manipulation
library(dlookr)     # Data quality diagnostics
library(naniar)     # Missing value visualization
library(UpSetR)     # Upset plots
library(ggplot2)    # Plotting
library(corrplot)   # Correlation plots
library(tidyr)      # Data tidying
library(mice)       # Multiple imputation
library(purrr)      # Functional programming
library(scales)     # Scaling functions
library(fontawesome)# Icons

# ==============================================================================
# LOAD DATA
# ==============================================================================

# Read Digitec sales data
data <- read_csv("DigitecLive_Final.csv")

# Read official Swiss municipality data
# Source: https://www.agvchapp.bfs.admin.ch/de/state/query
# Export date: 06. April 2025
mun_data <- read_excel("Gemeindestand.xlsx")
# Rename columns for clarity (comes from different approaches before)
mun_data <- mun_data %>%
  rename(
    GDENR = `BFS Gde-nummer`,    # Municipality number
    GDENAME = `Gemeindename`      # Municipality name
  )

# ==============================================================================
# DATA DIAGNOSIS REPORT (optional)
# ==============================================================================

# Uncomment to generate comprehensive data quality report
# diagnose_web_report(data)

# ==============================================================================
# REMOVE DUPLICATES
# ==============================================================================

cat("\n=== DUPLICATE REMOVAL ===\n")
duplicates_count <- sum(duplicated(data))
cat("Duplicates found:", duplicates_count, "\n")

# Remove duplicates
data <- distinct(data)

# Verify removal
cat("Duplicates after removal:", sum(duplicated(data)), "\n\n")

# ==============================================================================
# MISSING VALUES ANALYSIS
# ==============================================================================

cat("=== INITIAL MISSING VALUES ANALYSIS ===\n")

# Visualize missing values
gg_miss_var(data) + labs(y = "Missing values per variable")

# Show percentage of missing values per variable
miss_var_summary(data) %>%
  arrange(desc(pct_miss)) %>%
  print()

# ==============================================================================
# DATA CLEANING
# ==============================================================================

cat("\n=== CLEANING REDUNDANT COLUMNS ===\n")

# Remove salesPrice column (100% missing)
data <- data %>%
  select(-salesPrice)
cat("✓ Removed 'salesPrice' (100% missing)\n")

# Remove infos.Price (duplicate of salesPrice.amountIncl)
data <- data %>%
  select(-infos.Price)
cat("✓ Removed 'infos.Price' (duplicate information)\n")

# Remove infos.Currency (duplicate of salesPrice.currency)
data <- data %>%
  select(-infos.Currency)
cat("✓ Removed 'infos.Currency' (duplicate information)\n")

# Fill missing currency values with CHF (Swiss specification)
data$salesPrice.currency[is.na(data$salesPrice.currency)] <- "CHF"
cat("✓ Filled missing currency values with 'CHF'\n")

# Clean category column (remove URL prefix)
data$infos.Category <- str_remove(data$infos.Category, "^/de/s1/producttype/")
cat("✓ Cleaned 'infos.Category' column\n\n")

# Show updated missing values
cat("=== MISSING VALUES AFTER INITIAL CLEANING ===\n")
miss_var_summary(data) %>%
  arrange(desc(pct_miss)) %>%
  head(10) %>%
  print()

# ==============================================================================
# MUNICIPALITY DATA MATCHING
# ==============================================================================

cat("\n")
cat("================================================================================\n")
cat("MUNICIPALITY DATA MATCHING\n")
cat("================================================================================\n\n")

# ------------------------------------------------------------------------------
# STEP 0: Validate BFS Numbers
# ------------------------------------------------------------------------------

cat("=== STEP 0: BFS Number Validation ===\n")
rows_before_validation <- nrow(data)

# Find invalid BFS numbers (not in official municipality list)
invalid_bfs <- data %>%
  filter(!is.na(mun_bfsnr) & !mun_bfsnr %in% mun_data$GDENR) %>%
  select(mun_bfsnr, cityName) %>%
  distinct() %>%
  arrange(mun_bfsnr)

if(nrow(invalid_bfs) > 0) {
  cat("WARNING:", n_distinct(invalid_bfs$mun_bfsnr), 
      "invalid BFS numbers found (not in official list)\n")
  cat("\nExamples of invalid BFS numbers:\n")
  print(head(invalid_bfs, 10))
  cat("\n")
  
  # Count affected rows
  rows_with_invalid_bfs <- data %>%
    filter(!is.na(mun_bfsnr) & !mun_bfsnr %in% mun_data$GDENR) %>%
    nrow()
  
  cat("Total rows with invalid BFS numbers:", rows_with_invalid_bfs, "\n")
  
  # Remove rows with invalid BFS numbers
  data <- data %>%
    filter(is.na(mun_bfsnr) | mun_bfsnr %in% mun_data$GDENR)
  
  cat("→ Rows removed\n")
  cat("Remaining rows:", nrow(data), "\n")
  cat("Data loss:", rows_with_invalid_bfs, "rows (",
      round(rows_with_invalid_bfs / rows_before_validation * 100, 2), "%)\n\n")
} else {
  cat("✓ All BFS numbers are valid\n\n")
}

# ------------------------------------------------------------------------------
# STEP 1: Join via Municipality Number
# ------------------------------------------------------------------------------

cat("=== STEP 1: Join via Municipality Number ===\n")
cat("Before join - rows with mun_bfsnr:", sum(!is.na(data$mun_bfsnr)), "\n")

# Join with official municipality data using BFS number
data <- data %>%
  left_join(
    mun_data %>% select(GDENR, GDENAME, Kanton), 
    by = c("mun_bfsnr" = "GDENR")
  ) %>%
  rename(
    cityName_clean = GDENAME,    # Clean municipality name
    canton = Kanton              # Canton
  )

# Verification after Step 1
cat("After join:\n")
cat("  - Rows with mun_bfsnr:", sum(!is.na(data$mun_bfsnr)), "\n")
cat("  - Rows with cityName_clean:", sum(!is.na(data$cityName_clean)), "\n")
cat("  - Rows without mun_bfsnr:", sum(is.na(data$mun_bfsnr)), "\n\n")

# ------------------------------------------------------------------------------
# STEP 2: Name Normalization
# ------------------------------------------------------------------------------

cat("=== STEP 2: Name Normalization ===\n")

# Function to normalize municipality names for matching
# Handles: umlauts, special characters, saint/sankt variations
normalize_name <- function(name) {
  # Replace saint- or saint with st
  name <- gsub("\\bsaint[- ]", "st ", name, ignore.case = TRUE)
  # Replace sankt- or sankt with st
  name <- gsub("\\bsankt[- ]", "st ", name, ignore.case = TRUE)
  # Replace German umlauts
  name <- gsub("ä", "ae", name)
  name <- gsub("ö", "oe", name)
  name <- gsub("ü", "ue", name)
  name <- gsub("Ä", "Ae", name)
  name <- gsub("Ö", "Oe", name)
  name <- gsub("Ü", "Ue", name)
  # Replace French accents
  name <- str_replace_all(name, "é|è|ê", "e")
  name <- str_replace_all(name, "à|â", "a")
  # Replace German ß
  name <- str_replace_all(name, "ß", "ss")
  # Remove all non-alphabetic characters
  name <- gsub("[^A-Za-z ]", "", name)
  # Convert to lowercase
  name <- tolower(name)
  return(name)
}

# Apply normalization to both datasets
mun_data <- mun_data %>%
  mutate(GDENAME_normalized = normalize_name(GDENAME))

data <- data %>%
  mutate(cityName_normalized = normalize_name(cityName))

cat("✓ Name normalization complete\n\n")

# ------------------------------------------------------------------------------
# STEP 3: Join via Normalized Names (for missing numbers)
# ------------------------------------------------------------------------------

cat("=== STEP 3: Join via Normalized Names (for missing numbers) ===\n")

# Create lookup table for name matching
name_lookup <- mun_data %>%
  select(GDENAME_normalized, GDENR, GDENAME, Kanton) %>%
  distinct()

# Check for duplicates in normalized names
duplicates <- name_lookup %>%
  group_by(GDENAME_normalized) %>%
  filter(n() > 1) %>%
  arrange(GDENAME_normalized)

if(nrow(duplicates) > 0) {
  cat("WARNING:", nrow(duplicates), 
      "municipalities with identical normalized names found\n")
  cat("These may cause ambiguous matches:\n")
  print(head(duplicates, 10))
  cat("\n")
}

# Join via normalized names (only for rows still missing mun_bfsnr)
data <- data %>%
  left_join(
    name_lookup,
    by = c("cityName_normalized" = "GDENAME_normalized")
  ) %>%
  mutate(
    # Only fill in if mun_bfsnr was previously NA
    mun_bfsnr = coalesce(mun_bfsnr, GDENR),
    cityName_clean = coalesce(cityName_clean, GDENAME),
    canton = coalesce(canton, Kanton)
  ) %>%
  select(-GDENR, -GDENAME, -Kanton)

# Verification after Step 3
cat("After join via names:\n")
cat("  - Rows with mun_bfsnr:", sum(!is.na(data$mun_bfsnr)), "\n")
cat("  - Rows with cityName_clean:", sum(!is.na(data$cityName_clean)), "\n")
cat("  - Rows without mun_bfsnr:", sum(is.na(data$mun_bfsnr)), "\n\n")

# Check consistency between mun_bfsnr and cityName_clean
inconsistent <- sum(!is.na(data$mun_bfsnr) & is.na(data$cityName_clean))
if(inconsistent > 0) {
  cat("WARNING:", inconsistent, 
      "rows have mun_bfsnr but no cityName_clean!\n\n")
} else {
  cat("✓ All rows with mun_bfsnr also have cityName_clean (consistent)\n\n")
}

# Verify uniqueness of matches (one BFS number should map to one name)
bfs_name_counts <- data %>%
  filter(!is.na(mun_bfsnr)) %>%
  group_by(mun_bfsnr) %>%
  summarise(
    n_citynames = n_distinct(cityName_clean, na.rm = TRUE),
    cityname = first(cityName_clean)
  ) %>%
  arrange(desc(n_citynames))

cat("Match uniqueness check (should always be 1):\n")
print(head(bfs_name_counts, 10))

# ------------------------------------------------------------------------------
# Cleanup: Remove unnecessary columns
# ------------------------------------------------------------------------------

cat("\n=== CLEANUP: Removing Unnecessary Columns ===\n")

columns_before <- ncol(data)

data <- data %>%
  select(-cityName,           # Original (often misspelled) city name
         -vill_name,          # Village name (redundant)
         -mun_canton,         # Canton info (now in 'canton')
         -mun_nhits,          # Number of hits (not needed)
         -cityName_normalized) # Temporary column for matching

columns_after <- ncol(data)
cat("Columns removed:", columns_before - columns_after, "\n\n")

# ==============================================================================
# FINAL DATA QUALITY CHECKS
# ==============================================================================

cat("================================================================================\n")
cat("FINAL DATA QUALITY CHECKS\n")
cat("================================================================================\n\n")

# Check remaining missing values
cat("=== Remaining Missing Values ===\n")
missing <- miss_var_summary(data) %>%
  arrange(desc(pct_miss))
print(head(missing, 10))

# Remove rows without municipality assignment
cat("\n=== Removing Rows Without Municipality Assignment ===\n")
rows_before <- nrow(data)
data <- data %>%
  filter(!is.na(mun_bfsnr))
rows_after <- nrow(data)

cat("Rows before:", rows_before, "\n")
cat("Rows after:", rows_after, "\n")
cat("Data loss:", rows_before - rows_after, "rows (", 
    round((rows_before - rows_after) / rows_before * 100, 2), "%)\n\n")

# Drop all rows with any remaining missing values
cat("=== Removing All Remaining Missing Values ===\n")
rows_before_dropna <- nrow(data)
data <- data %>% drop_na()
rows_after_dropna <- nrow(data)

cat("Rows removed:", rows_before_dropna - rows_after_dropna, "\n\n")

# Final missing value check
cat("=== Final Missing Value Check ===\n")
final_missing <- miss_var_summary(data) %>%
  arrange(desc(pct_miss))
print(head(final_missing, 10))

if(max(final_missing$pct_miss) == 0) {
  cat("\n✓ No missing values remaining!\n")
} else {
  cat("\n⚠ Warning: Some missing values still present\n")
}

# ==============================================================================
# SAVE CLEANED DATA
# ==============================================================================

cat("\n================================================================================\n")
cat("SAVING CLEANED DATA\n")
cat("================================================================================\n\n")

write_csv(data, "DigitecLive_Cleaned.csv")
cat("✓ Cleaned data saved to: DigitecLive_Cleaned.csv\n")
cat("✓ Final dataset size:", nrow(data), "rows ×", ncol(data), "columns\n\n")

cat("================================================================================\n")
cat("CLEANING COMPLETE\n")
cat("================================================================================\n")

# End of script

