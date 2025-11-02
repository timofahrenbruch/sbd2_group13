# clear environment
rm(list = ls())

# set working directory for looking at the data file
setwd("data")

# call important or might be important libraries
library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(dlookr)
library(naniar)
library(UpSetR)
library(ggplot2)
library(corrplot)
library(tidyr)
library(mice)
library(purrr)
library(scales)
library(fontawesome)

# read in the data
data <- read_csv("DigitecLive_Final.csv")
# read the official municipality data for later matching
mun_data <- read_excel("Gemeindestand.xlsx")
mun_data <- mun_data %>%
  rename(
    GDENR = `BFS Gde-nummer`,
    GDENAME = `Gemeindename`
  )

######################## REPORT FOR DATA DIAGNOSIS ########################
# create report for looking at the data diagnosis
diagnose_web_report(data)

######################## DUPLICATES ########################
# check for duplicates
sum(duplicated(data))     # how many?
# delete the duplicates
data <- distinct(data)
# verify that duplicates have been removed
sum(duplicated(data))

######################## MISSING VALUES ########################
# visualize the missing values
gg_miss_var(data) + labs(y = "Missing values per variable")
# show the percentage of missing values per variable
miss_var_summary(data) %>%
  arrange(desc(pct_miss))

######################## CLEANING ########################
# since salesPrice has 100% missing values, we will remove this column
data <- data %>%
  select(-salesPrice)
#since according to the variable description provided by the profs "infos.Price" is the same as "salesPrice.amountIncl" we delete the column "infos.Price"
data <- data %>%
  select(-infos.Price)
# the same applies to "infos.Currency" and "salesPrice.currency"
data <- data %>%
  select(-infos.Currency)

# fill up missing salesPrice.currency with "CHF" since everything is in swiss francs according to the specifications
data$salesPrice.currency[is.na(data$salesPrice.currency)] <- "CHF"

# transform column info.Category (remove URL part)
data$infos.Category <- str_remove(data$infos.Category, "^/de/s1/producttype/")

# show the percentage of missing values per variable
miss_var_summary(data) %>%
  arrange(desc(pct_miss))

############################ CITYNAMES ##############################
# Further cleaning of names for entries without a unique match
# (Normalizing both sources for consistent name comparison)
normalize_name <- function(name) {
  # Replace 'saint-' or 'saint ' with 'ste. '
  name <- gsub("\\bsaint[- ]", "ste. ", name, ignore.case = TRUE)
  # Replace 'sankt-' or 'sankt ' with 'st. '
  name <- gsub("\\bsankt[- ]", "st. ", name, ignore.case = TRUE)
  # Replace German umlauts (lowercase and uppercase)
  name <- gsub("ä", "ae", name)
  name <- gsub("ö", "oe", name)
  name <- gsub("ü", "ue", name)
  name <- gsub("Ä", "Ae", name)
  name <- gsub("Ö", "Oe", name)
  name <- gsub("Ü", "Ue", name)
  name <- str_replace_all(name, "é|è|ê", "e")
  name <- str_replace_all(name, "à|â", "a")
  name <- str_replace_all(name, "ß", "ss")
  # Remove all non-alphabetic characters (keep only letters and spaces)
  name <- gsub("[^A-Za-z ]", "", name)
  return(name)
}

# normalize both datasets
mun_data <- mun_data %>%
  mutate(GDENAME_clean = normalize_name(GDENAME))

data <- data %>%
  mutate(cityName_clean = normalize_name(cityName))

# count total rows where cityName does not match any of GDENAME in mun_data
sum(!data$cityName %in% mun_data$GDENAME)

# join mun_bfsnr ID with GDENR in mun_data and write GDENAME of it to cityname_clean
data <- data %>%
  left_join(mun_data, by = c("mun_bfsnr" = "GDENR")) %>%
  mutate(cityName_clean = GDENAME) %>%
  select(-GDENAME)

# count total rows where cityName_clean match cityName
sum(data$cityName == data$cityName_clean, na.rm = TRUE)

# show how many rows where cityname and cityname_clean match after overwrite
sum(data$cityName == data$cityName_clean, na.rm = TRUE)

# show the percentage of missing values per variable
miss_var_summary(data) %>%
  arrange(desc(pct_miss))

# number of different cityName entries per bfsnr
bfs_name_counts <- data %>%
  group_by(mun_bfsnr) %>%
  summarise(
    n_citynames = n_distinct(cityName, na.rm = TRUE),
    examples = paste0(head(unique(cityName), 3), collapse = ", ")
  ) %>%
  arrange(desc(n_citynames))

# join normalized names to assign bfsnr, canton, official name
data <- data %>%
  left_join(
    mun_data %>% select(GDENR, GDENAME, GDEKTNA, GDENAME_clean),
    by = c("cityName_clean" = "GDENAME_clean")
  ) %>%
  mutate(
    vill_name  = coalesce(vill_name, GDENAME),
    mun_canton = coalesce(mun_canton, GDEKTNA.y),
    mun_bfsnr  = coalesce(mun_bfsnr, GDENR)
  ) %>%
  select(-any_of(c("GDENAME", "GDEKTNA.x", "GDEKTNA.y", "GDENR", "cityName_clean", "GDENAME_clean")))

# number of different cityName entries per bfsnr
bfs_name_counts <- data %>%
  group_by(mun_bfsnr) %>%
  summarise(
    n_citynames = n_distinct(cityName, na.rm = TRUE),
    examples = paste0(head(unique(cityName), 3), collapse = ", ")
  ) %>%
  arrange(desc(n_citynames))

# count total rows where cityName does not match any of GDENAME in mun_data
sum(!data$cityName %in% mun_data$GDENAME)

# join mun_bfsnr ID with GDENR in mun_data and write GDENAME of it to cityname_clean
data <- data %>%
  left_join(mun_data, by = c("mun_bfsnr" = "GDENR")) %>%
  mutate(cityName_clean = GDENAME) %>%
  select(-GDENAME)

# count total rows where cityName_clean match cityName
sum(data$cityName == data$cityName_clean, na.rm = TRUE)

# overwrite cityname with cityName_clean where mun_nhits == 1
data <- data %>%
  mutate(cityName = if_else(mun_nhits == 1, cityName_clean, cityName))

# show how many rows where cityname and cityname_clean match after overwrite
sum(data$cityName == data$cityName_clean, na.rm = TRUE)

# show the percentage of missing values per variable
miss_var_summary(data) %>%
  arrange(desc(pct_miss))

# number of different cityName entries per bfsnr
bfs_name_counts <- data %>%
  group_by(mun_bfsnr) %>%
  summarise(
    n_citynames = n_distinct(cityName, na.rm = TRUE),
    examples = paste0(head(unique(cityName), 3), collapse = ", ")
  ) %>%
  arrange(desc(n_citynames))

# keep only relevant columns
data <- data %>%
  select(-starts_with("GDE"))

# check remaining missing values
miss_var_summary(data) %>%
  arrange(desc(pct_miss))

# delete the rows where nhits is still not 1 --> data loss is not that high...
data <- data %>%
  filter(mun_nhits == 1)

# drop all rows with any missing values
data <- data %>% drop_na()

# check remaining missing values
miss_var_summary(data) %>%
  arrange(desc(pct_miss))

# looking at outliers in the data
# --> we will not look at outliers
# looking at correlated features in the data
# --> we will not look at correlated features

# save the cleaned data as new csv file
write_csv(data, "DigitecLive_Cleaned.csv")
# end of script