# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# --------------------- DATA QUALITY & VISUALIZATION IN R -------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #


# In this script, we learn how we can use R to investigate, detect and correct 
# data quality issues, as well as, visualize.

# Specifically, we will learn:
#   - How to detect and visualize missing values, special values, etc. 
#   - How to detect and visualize outliers 
#   - How to detect and visualize highly correlated features 
#   - The different approaches to dealing with missing values
#   - The different approaches to dealing with outliers 


# We start by calling all the libraries that we are going to use. Remember - 
# we install packages once, but we call the libraries every time we need to use 
# a specific function. 
#install.packages("readr")
#install.packages("dplyr")
#install.packages("dlookr")
#install.packages("naniar")
#install.packages("UpSetR")
#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("tidyr")
#install.packages("mice")
#install.packages("purrr")
#install.packages("scales")   
#install.packages("fontawesome")

# We call all the libraries that we are going to use
library(readr)
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

# We set the working directory and we call the data that we are going to use. 
# Please import the csv file "data_loans.csv".
setwd("/Users/timofahrenbruch/Projekte/BFH/sbd2_group13/data-cleaning theory")
data_loans <- read_csv("data_loans_final.csv")
data <- data_loans # We always make a copy from the original dataset and work on the copy


# As always, we start with checking the dimension, structure, head and tail of our dataset 
dim(data)
str(data)
head(data)
tail(data)

# We check the summary of all variables included. Already here we can identify some data quality issues. What can you tell about our data?
summary(data)
overview(data)      # Functionality offered by the dlookr package 
# – observations : number of observations (number of rows)
# – variables : number of variables (number of columns)
# – values : number of values (number of cells. rows * columns)
# – memory size : an estimate of the memory that is being used to store an R object.
# – duplicate observation: number of duplicate cases(observations).
# – complete observation : number of complete cases(observations). i.e., have no missing values.
# – missing observation : number of observations that has missing values.
# – missing variables : number of variables that has missing values.
# – missing values : number of values(cells) that has missing values.
# – numerics : number of variables that is data type is numeric.
# – integers : number of variables that is data type is integer.
# – factors : number of variables that is data type is factor.
# – characters : number of variables that is data type is character.
# – Dates : number of variables that is data type is Date.
# – POSIXcts : number of variables that is data type is POSIXct.
# – others : number of variables that is not above


# Check for duplicates
sum(duplicated(data))     # how many?
which(duplicated(data))   # which rows? 
print(data[c(171826, 190140, 191506),])


# We can also have a visual representation of the overview
overview <- overview(data)
plot(overview)


# In the next step, we use functions from the dlookr package to diagnose some data quality issues
# Function: diagnose()

# Output: 
#   * variables : variable names
#   * types : the data type of the variables
#   * missing_count : number of missing values
#   * missing_percent : percentage of missing values
#   * unique_count : number of unique values
#   * unique_rate : rate of unique value. unique_count / number of observation
diagnose(data, 1:20)


# We are free to apply this function to any specific variable, not necessarily the full list of features 
diagnose(data, loan_amnt)       # If we want to diagnose only one selected variable (eg. loan amount)
diagnose(data, -loan_amnt)      # If we want to diagnose all apart from one variable



# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #
# ----------------------------- MISSING VALUES ----------------------------- #
# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #


# In the next step, we specifically look at missing values (NAs).
# We start by running some visualizations of missing value available in the UpSetR package. 

# The next plot, gg_miss_var() shows the number of missing values in each variable in a dataset. 
gg_miss_var(data) + labs(y = "Missing values per variable")


# We can also facet as per another variable of interest (eg. the loan status).
gg_miss_var(data,
            facet = loan_status)


# An upset plot from the UpSetR package can be used to visualise the patterns of missingness, 
# or rather the combinations of missingness across cases. 
gg_miss_upset(data)


# Missing categorical variables
# This plot shows the number of missing data in each column, 
# broken down by a categorical variable from the dataset. In our case, we choose the emp_lenght
gg_miss_fct(x = data, fct = emp_length)


# Next, we show visualization of missing value available in the dlookr package
data %>%
  select(1:20) %>%
  plot_na_pareto(col = "blue", main = "Pareto Chart for all variales")


# In case, we only want a table, we set the option plot = F
data %>%
  select(1:20) %>%
  plot_na_pareto(col = "blue", plot = FALSE)



# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #
# -------------------------------- OUTLIERS -------------------------------- #
# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #


# The dlookr package allows us to also diagnose the numerical features included in the dataset, specifically
# looking at outliers 

# Outputs:
#   * min : minimum value
#   * Q1 : 1/4 quartile, 25th percentile
#   * mean : arithmetic mean
#   * median : median, 50th percentile
#   * Q3 : 3/4 quartile, 75th percentile
#   * max : maximum value
#   * zero : number of observations with a value of 0
#   * minus : number of observations with negative numbers
#   * outlier : number of outliers

data$loan_status <- as.factor(data$loan_status)
diagnose_numeric(data, 1:20)


# The package allows us to also diagnose the categorical features included in the dataset.

# Output: 
#   * variables : variable names
#   * levels: level names
#   * N : number of observation
#   * freq : number of observation at the levels
#   * ratio : percentage of observation at the levels
#   * rank : rank of occupancy ratio of levels

diagnose_category(data, 1:20)
cat <- diagnose_category(data)


# In the next step, we use diagnose_outlier() function in the dlookr package 

# Outputs: 
#   * outliers_cnt : number of outliers
#   * outliers_ratio : percent of outliers
#   * outliers_mean : arithmetic average of outliers
#   * with_mean : arithmetic average of with outliers
#   * without_mean : arithmetic average of without outliers
diagnose_outlier(data) 


# In the next step, we plot a selected variable with and without the outliers.
# This step is crucial as it gives us some indication on how outliers affect the variables we are considering
data %>%
  plot_outlier(loan_amnt)


# This is a way to plot all variables with and without the outliers. Remember the pipe operator! 
data %>%
  plot_outlier(diagnose_outlier(data) %>%
                 filter(outliers_ratio >= 0.5) %>%          # dplyr
                 select(variables) %>%
                 unlist())


# Create a report -- this might take a while
diagnose_web_report(data)



# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #
# ----------------------- HIGHLY CORRELATED VALUES ------------------------- #
# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #


# We start by checking for calculating the correlation coefficient between the numeric features
data_num <- data %>%
  select_if(is.numeric)  # we create a dataset only with numeric variables 
data_num <- as.data.frame(data_num)
cor(data_num)         # Why do we get NAs??


# We need complete.cases()
data_num_comp <- data_num[complete.cases(data_num),]
corr <- cor(data_num_comp)
corrplot(corr)

# Let's have a list!
corr %>%
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  gather(var2, value, -var1) %>% 
  filter(value != 1) %>% 
  arrange(desc(value))


# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #
# --------------- DEALING WITH SOME DATA QUALITY ISSUES -------------------- #
# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #


# In appendex to this script, you have the implementation of all different 
# methods to deal with data quality issues.


# Missing values 

# One variable, "missing since last delinquency", has over 50% of the observations missing. 
# In our use case, this represent an important feature hence we should not remove it. 
# We will instead create a factor variable. 

data$mths_delinq_factor <- cut(data$mths_since_last_delinq, breaks=c(0, 16, 31, 50, 100, 152))
data$mths_delinq_factor <- addNA(data$mths_delinq_factor)
str(data$mths_delinq_factor)
levels(data$mths_delinq_factor)
summary(data$mths_delinq_factor)
data <- data[-20]


# For all other variables, we would remove rows with missing values
data <- data[complete.cases(data),]


# Outliers (remember, different choices are available in the appendix)

# Capping -- Simple & frequently used 

# Let's create a function that caps outliers 
outlier <- function(x){
  quantiles <- quantile(x, c(.05, .95))
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  x
}   

# Let's create the new dataset with capped outliers 
data_num <- data %>%
  select_if(is.numeric)
data_other <- data %>%
  select_if(negate(is.numeric))


data_new <- map_df(data_num, outlier)
data_outliers_capped <- cbind(data_new,data_other)
summary(data_outliers_capped)
summary(data)

data <- data_outliers_capped

# Correlated features 
data_num <- data %>%
  select_if(is.numeric)

corr_new <- cor(data_num)

corr_new %>%
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  gather(var2, value, -var1) %>% 
  filter(value != 1) %>% 
  arrange(desc(value))

# We remove the top correlated features (funded amount, funded amount invested, installment, fico_range_high)
data <- data[-c(2,3,5,9)]
data_num <- data %>%
  select_if(is.numeric)

corr_new <- cor(data_num)

corr_new %>%
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  gather(var2, value, -var1) %>% 
  filter(value != 1) %>% 
  arrange(desc(value))


# At this point, we can save the dataset
write.csv(data, "data_after_processing.csv")



# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #
# ------------------------------- APPENDIX --------------------------------- #
# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #


# Missing values
# 1) Complete cases: Remove all rows in which at least 1 observation is missing 
# 2) Column-wise followed by row-wise deletion 
# 3) Imputation 


# 1) Complete cases
data_comp <- data[complete.cases(data),]


# Remember: deleting missing data leads to the following major issues:
# - Data loss
# - Bias data


# 2) Column-wise followed by row-wise deletion
diagnose(data)

# mths_since_last_delinq -- should be removed!
data <- data[,-20]
data_comp_2 <- data[complete.cases(data),] # much better

# Look what difference that made in terms of data loss!

# 3) Imputation 
# Through the dlookr package 
# method : method of missing value imputation.
# predictor is numerical variable
# What can we impute? 
#   * "mean" : arithmetic mean
#   * "median" : median
#   * "mode" : mode
#   * "capping" : Impute the upper outliers with 95 percentile, and Impute the lower outliers with 5 percentile.

# In case we made some changes ... 
data_loans <- read_csv("data_loans.csv")
data <- data_loans[-1] # We always make a copy from the original dataset and work on the copy


# Let's look at dti 
sum(is.na(data$dti)) # 235 missing values


# We can substitute missing values with one of the options above: mean, median, mode, capping. 
# Lets try with the median and the dti variable 
imp <- imputate_na(data, dti, method = "median")
summary(imp)
plot(imp)


# MICE package: Multivariate Imputation by Chained Equations
# Predictive Mean Matching (PMM) is a technique of imputation that estimates the likely values of missing data by matching to the observed values/data. 
# This can be carried out either by singular imputations or multiple imputations.

# In case we made some changes ...
data_loans <- read_csv("data_loans.csv")
data <- data_loans[-1] # We always make a copy from the original dataset and work on the copy

# Run mice 
tempData <- mice(data, m=5, maxit=5, meth='pmm', seed=500) 
# meth='pmm' refers to the imputation method. In this case, we are using predictive mean matching as imputation method. 
# m=5 refers to the number of imputed datasets. Five is the default value.

data_comp <- complete(tempData, 1) # Select one of the imputed datasets 
summary(data_comp)   # What are u noticing??? 

data_num_comp <- data_num[complete.cases(data_num),]
cor(data_num_comp$loan_amnt, data_num_comp$funded_amnt)
cor(data_num_comp$loan_amnt, data_num_comp$funded_amnt_inv)
cor(data_num_comp$fico_range_low, data_num_comp$fico_range_high)


data_comp_3 <- data_comp[complete.cases(data_comp),]


# Dealing with outliers 
# Rules:
#   * If error – remove
#   * If not part of the population – remove
#   * If natural part of the population - keep


# Methods of dealing:
# 1) Removing outliers (remember the rules)
# 2) Capping
# 3) Imputing 

# 1) Removing 
# All values that are below or above the 5% and 95% quantile, are set to NAs

# In case we made some changes ...
data_loans <- read_csv("data_loans.csv")
data <- data_loans[-1] # We always make a copy from the original dataset and work on the copy

outlier <- function(x){
  quantiles <- quantile(x, c(.05, .95))
  x[x < quantiles[1]] <- NA
  x[x > quantiles[2]] <- NA
  x
}  

data_new <- data[complete.cases(data),]
data_num <- data_new %>%
  select_if(is.numeric)
data_cat <- data_new %>%
  select_if(is.character)

data_new <- map_df(data_num, outlier)
data_outliers_removed <- cbind(data_new,data_cat)
data_outliers_removed <- data_outliers_removed[complete.cases(data_outliers_removed),]


# 2) Capping 
# In case we made some changes ...
data_loans <- read_csv("data_loans.csv")
data <- data_loans[-1] # We always make a copy from the original dataset and work on the copy

# Simple & frequently used 
outlier <- function(x){
  quantiles <- quantile(x, c(.05, .95))
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  x
}   

data_new <- data[complete.cases(data),]
data_num <- data_new %>%
  select_if(is.numeric)
data_cat <- data_new %>%
  select_if(is.character)

data_new <- map_df(data_num, outlier)
data_outliers_capped <- cbind(data_new,data_cat)
data_outliers_capped <- data_outliers_capped[complete.cases(data_outliers_capped),]


# 3) Imputing 
# Using the dlookr package and the function impute_outliers()
# method : method of missing value imputation.
# predictor is numerical variable
#   * "mean" : arithmetic mean
#   * "median" : median
#   * "mode" : mode
#   * "capping" : Impute the upper outliers with 95 percentile, and Impute the lower outliers with 5 percentile.


# In case we made some changes ...
data_loans <- read_csv("data_loans.csv")
data <- data_loans[-1] # We always make a copy from the original dataset and work on the copy

diagnose_numeric(data)
imp_income <- imputate_outlier(data, annual_inc, method = "median")
imp_income
summary(imp_income)



