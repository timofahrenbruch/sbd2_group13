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
mun_data <- read_excel("bfsGdeNr.xlsx")


# create report for looking at the data diagnosis
diagnose_web_report(data)


# check for duplicates
sum(duplicated(data))     # how many?
# delete the duplicates
data <- distinct(data)
# verify that duplicates have been removed
sum(duplicated(data))


# visualize the missing values
gg_miss_var(data) + labs(y = "Missing values per variable")
# show the percentage of missing values per variable
miss_var_summary(data) %>%
  arrange(desc(pct_miss))


# since salesPrice has 100% missing values, we will remove this column
data <- data %>%
  select(-salesPrice)
#since according to the variable description provided by the profs "infos.Price" is the same as "salesPrice.amountIncl" we delete the column "infos.Price"
data <- data %>%
  select(-infos.Price)
# the same applies to "infos.Currency" and "salesPrice.currency"
data <- data %>%
  select(-infos.Currency)

# transform column info.Category (remove URL part)
data$infos.Category <- str_remove(data$infos.Category, "^/de/s1/producttype/")

# normalize city names for better matching
normalize_names <- function(x) {
  x <- tolower(x)                              # alles klein
  x <- trimws(x)                               # Leerzeichen an Rändern entfernen
  x <- stringi::stri_trans_general(x, "Latin-ASCII")  # Umlaute & Akzente entfernen
  x <- gsub("\\bst[\\.]?\\b", "sankt", x)      # St. → sankt
  x <- gsub("\\bste[\\.]?\\b", "sainte", x)    # Ste. → sainte
  x <- gsub("\\bsaint[\\.]?\\b", "sankt", x)   # Saint → sankt
  x <- gsub("\\bsainte[\\.]?\\b", "sankt", x)  # Sainte → sankt
  x <- gsub("[^a-z\\s]", "", x)                # nur Buchstaben + Leerzeichen behalten
  x <- gsub("\\s+", " ", x)                    # doppelte Leerzeichen entfernen
  trimws(x)
}

# creating duplicated city name column for matching
mun_data <- mun_data %>%
  mutate(GDENAME_clean = normalize_names(GDENAME))

# normalize city names in data for matching where nhits != 1
data <- data %>%
  mutate(cityName_clean = if_else(
    mun_nhits != 1,
    normalize_names(cityName),
    cityName
  ))

# matching with a loop
for (i in seq_len(nrow(data))) {
  city <- data$cityName_clean[i]
  match_row <- mun_data %>%
    filter(GDENAME_clean == city)
  
  if (nrow(match_row) == 1) {
    data$vill_name[i]  <- match_row$GDENAME    # offizieller Gemeindename
    data$mun_canton[i] <- match_row$GDEKTNA    # Kanton
    data$mun_bfsnr[i]  <- match_row$GDENR      # BFS-Nr.
    data$mun_nhits[i]  <- 1                    # Treffer markieren
  }
}

# are there now less missing values in the data (in %)?
miss_var_summary(data) %>%
  arrange(desc(pct_miss))
# --> only a minimal part less missing values...

# delete the rows where nhits is still not 1 --> data loss is not that high...
data <- data %>%
  filter(mun_nhits == 1)

# looking at outliers in the data
# --> we will not look at outliers
# looking at correlated features in the data
# --> we will not look at correlated features

# save the cleaned data as new csv file
write_csv(data, "DigitecLive_Cleaned.csv")
# end of script




