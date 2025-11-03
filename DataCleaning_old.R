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
mun_data <- read_excel("Gemeindestand.xlsx") #Excel-Export from https://www.agvchapp.bfs.admin.ch/de/state/query, date = 06. april 2025
mun_data <- mun_data %>%
  rename(
    GDENR = `BFS Gde-nummer`,
    GDENAME = `Gemeindename`
  )

######################## REPORT FOR DATA DIAGNOSIS ########################
# create report for looking at the data diagnosis
#diagnose_web_report(data)

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
# Reset mun_Nhits as we have our own match process
data$mun_nhits <- 0

# ----------------- JOIN WITH ID ---------------------
# count total rows where cityName does not match any of GDENAME in mun_data
sum(!data$cityName %in% mun_data$GDENAME)

# join mun_bfsnr ID with GDENR in mun_data and write GDENAME of it to cityname_clean, kanton to canton and set nhits to 1
data <- data %>%
  left_join(mun_data, by = c("mun_bfsnr" = "GDENR")) %>%
  mutate(cityName_clean = GDENAME,
         canton = Kanton,
         mun_nhits = 1) %>%
  select(-GDENAME, -Kanton)

# count total rows where cityName does not match any of GDENAME in mun_data
sum(!data$cityName_clean %in% mun_data$GDENAME)

# ----------------- NORMALIZE NAMES -----------------
# function to normalize the names of both datasets to allow a join via name of the ones which could not be joined via id
normalize_name <- function(name) {
  # Replace 'saint-' or 'saint ' with 'st. '
  name <- gsub("\\bsaint[- ]", "st ", name, ignore.case = TRUE)
  # Replace 'sankt-' or 'sankt ' with 'st. '
  name <- gsub("\\bsankt[- ]", "st ", name, ignore.case = TRUE)
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
  # Set to lowercase
  name = tolower(name)
  return(name)
}

# normalize both datasets
mun_data <- mun_data %>%
  mutate(GDENAME_normalized = normalize_name(GDENAME))

data <- data %>%
  mutate(cityName_normalized = normalize_name(cityName))

# count the ammount of missing mun_nhits before joining via name
sum(is.na(data$mun_bfsnr))

# join and via name and write GDENR to mun_bfsnr, GDENAME to cityName_clean, kanton to canton and set nhits to 1
data <- data %>%
  left_join(mun_data, by = c("cityName_normalized" = "GDENAME_normalized")) %>%
  mutate(
    was_na_before = is.na(mun_bfsnr),
    mun_bfsnr = if_else(was_na_before, GDENR, mun_bfsnr),
    cityName_clean = if_else(was_na_before, GDENAME, cityName_clean),
    canton = if_else(was_na_before, Kanton, canton),
    mun_nhits = if_else(was_na_before, 1, mun_nhits)
  ) %>%
  select(-was_na_before)
  
# count the ammount of missing mun_nhits after joining via name
sum(is.na(data$mun_bfsnr))

# Through the join we could further match 88 municipalities via name where the bfsnr was missing before

# number of different cityName_clean entries per bfsnr --> shows that there are no ambigous matches
bfs_name_counts <- data %>%
  group_by(mun_bfsnr) %>%
  summarise(
    n_citynames = n_distinct(cityName_clean, na.rm = TRUE),
    examples = paste0(head(unique(cityName_clean), 3), collapse = ", ")
  ) %>%
  arrange(desc(n_citynames))

# keep only relevant columns
data <- data %>%
  select(-cityName, -vill_name, -mun_canton, -mun_nhits, -`Hist.-Nummer.x`, -`Bezirks-nummer.x`, -Bezirksname.x,
         -`Datum der Aufnahme.x`, -cityName_normalized, -`Hist.-Nummer.y`)

# check remaining missing values
missing <- miss_var_summary(data) %>%
  arrange(desc(pct_miss))

# count where nhits 1
sum(data$mun_nhits == 1)

# 0 missing. -> not possible. there is still an error when setting the nhits value 

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