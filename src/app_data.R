##############################
# Loads the data into a form the app requires from the total_df
##############################
# Load libraries
library(tidyverse)
library(shiny)


# Load total_df
source("src/input_datasets.R")


# Generates a vector of names of the data sets
get_series_names <- function() {
  datasets_df <- distinct(select(total_df, SeriesName))
  dataset <- datasets_df[,1]
  return(dataset)
}

# Generates a vector of frequency names of the series names specified
get_frequency_types <- function(seriesName) {
  datasets_df <- filter(total_df, SeriesName == seriesName)
  frequency_types <- distinct(select(datasets_df, Frequency))
  dataset <- frequency_types[,1]
  return(dataset)
}

# Generates the min and max dates of the first data set
get_first_series_date_extremes <- function() {
  min <- strtoi(format(get_series_date_extremes(get_series_names()[1], get_frequency_types(get_series_names()[1]))[1, 1], "%Y"))
  max <- strtoi(format(get_series_date_extremes(get_series_names()[1], get_frequency_types(get_series_names()[1]))[2, 1], "%Y"))
  return(c(min, max))
}

# Generates a vector of the first and last row of the date column of the series name and frequency specified
get_series_date_extremes <- function(seriesName, frequency) {
  datasets_df <- select(filter(filter(total_df, SeriesName == seriesName), Frequency == frequency), Date)
  min <- head(datasets_df, 1)
  max <- tail(datasets_df, 1)
  return(bind_rows(min, max))
}

# Generates a data frame of only the dates and values columns of a specified series name and frequency
get_series_data <- function(seriesName, frequency) {
  datasets_df <- select(filter(filter(total_df, SeriesName == seriesName), Frequency == frequency), Date:Value)
  return(datasets_df)
}

# Turns date column of series data to string date
convert_to_string_date_series_data <- function(seriesName, frequency) {
  datasets_df <- get_series_data(seriesName, frequency)
  datasets_df[,1] <- format(datasets_df[,1], "%d-%m-%Y")
  return(datasets_df)
}

# Gets the source name and link from a specified series
get_series_sources <- function(seriesName) {
  sourceLinks_df <- distinct(select(filter(total_df, SeriesName == seriesName), SourceLink))
  sourceNames_df <- distinct(select(filter(total_df, SeriesName == seriesName), SourceName))
  return(c(sourceLinks_df[1], sourceNames_df[1]))
}

# Gets various summary statistics for specified data
get_summary_statistics <- function(seriesName, frequency) {
  datasets_df <- select(get_series_data(seriesName, frequency), Value)
  quantiles <- quantile(datasets_df[1,], probs = c(.25, .5, .75))
  print(quantiles)
  
  min <- summarise(datasets_df, min(Value))
  max <- summarise(datasets_df, max(Value))
  mean <- summarise(datasets_df, mean(Value))
  median <- summarise(datasets_df, median(Value))
  var <- summarise(datasets_df, var(Value))
  sd <- summarise(datasets_df, sd(Value))
  lq <- quantiles[1]
  mq <- quantiles[2]
  uq <- quantiles[3]
  iqr <- summarise(datasets_df, IQR(Value))
  return(c(min, max, mean, median, var, sd, lq, mq, uq, iqr))
}

# Generates the data frame for the table of summary stats for a specified series and frequency
generate_summary_dataframe <- function(seriesName, frequency) {
  stat_names = c('Minimum', 'Maximum', 'Mean', 'Median', 'Variance', 'Standard Deviation',
                 'Lower Quartile', 'Middle Quartile', 'Upper Quartile', 'Inter Quartile Range')
  stats_df <- get_summary_statistics(seriesName, frequency)
  df <- data.frame(stat_names, t(t(stats_df)))
  colnames(df) <- c('Summary', 'Value')
  return(df)
}
