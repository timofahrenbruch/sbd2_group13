# set working directory for looking at the data file 
setwd("/Users/timofahrenbruch/Projekte/BFH/sbd2_group13/data")

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
which(duplicated(data))   # which rows?
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


# matching the data of missing mun_bfsnr, vill_name & mun_canton
# show the problematic rows (where mun_bfsnr is missing or mun_nhits != 0)
problematic <- data %>%
  filter(mun_nhits != 1 | is.na(mun_bfsnr)) %>%
  distinct(cityName, vill_name, mun_canton, mun_bfsnr, mun_nhits)
head(problematic)

# match the missing values with the official municipality data





# save the cleaned data as new csv file
write_csv(data, "DigitecLive_Cleaned.csv")





