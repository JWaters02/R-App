##############################
# Get CPI data
##############################
# Load libraries
library(tidyverse)


# Load functions
source("src/input_datasets.R")


# Extract Consumer price index data
cpi_data <- function() {
  return(filter(total_df, SeriesName == 'CPI'))
}

cpi_yearly_data <- function() {
  return(filter(cpi_data(), Frequency == 'Annual'))
}

cpi_yearly_extremes <- function() {
  min <- head(cpi_yearly_data(), 1)
  max <- tail(cpi_yearly_data(), 1)
  return(bind_rows(min, max))
}

cpi_quarterly_data <- function() {
  return(filter(cpi_data(), Frequency == 'Quarter'))
}

cpi_quarterly_extremes <- function() {
  min <- head(cpi_quarterly_data(), 1)
  max <- tail(cpi_quarterly_data(), 1)
  return(bind_rows(min, max))
}

cpi_monthly_data <- function() {
  return(filter(cpi_data(), Frequency == 'Month'))
}

cpi_monthly_extremes <- function() {
  min <- head(cpi_monthly_data(), 1)
  max <- tail(cpi_monthly_data(), 1)
  return(bind_rows(min, max))
}