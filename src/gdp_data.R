##############################
# Get GDP data
##############################
# Load libraries
library(tidyverse)


# Load functions
source("src/input_datasets.R")


# Extract GDP data
gdp_data <- function() {
  return(filter(total_df, SeriesName == 'GDP'))
}

gdp_yearly_data <- function() {
  return(filter(gdp_data(), Frequency == 'Annual'))
}

gdp_yearly_extremes <- function() {
  min <- head(gdp_yearly_data(), 1)
  max <- tail(gdp_yearly_data(), 1)
  return(bind_rows(min, max))
}

gdp_quarterly_data <- function() {
  return(filter(gdp_data(), Frequency == 'Quarter'))
}

gdp_quarterly_extremes <- function() {
  min <- head(gdp_quarterly_data(), 1)
  max <- tail(gdp_quarterly_data(), 1)
  return(bind_rows(min, max))
}