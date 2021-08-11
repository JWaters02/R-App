##############################
# Get Unemployment data
##############################
# Load libraries
library(tidyverse)


# Load functions
source("src/input_datasets.R")


# Extract Unemployment rate data
unemployment_data <- function() {
  return(filter(total_df, SeriesName == 'Unemployment'))
}

unemployment_monthly_data <- function() {
  return(filter(unemployment_data(), Frequency == 'Month'))
}

unemployment_monthly_extremes <- function() {
  min <- head(unemployment_monthly_data(), 1)
  max <- tail(unemployment_monthly_data(), 1)
  return(bind_rows(min, max))
}