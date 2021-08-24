##############################
# Functions
##############################
# TODO: Fix legend not being put at bottom of plot


# Load libraries
library(shiny)
library(stringr)
library(tidyverse)
library(scales)


# Convert quarterly dates to the right format
convert_quarterly_dates <- function(dates_to_convert, match_cols, match_regex) {
  quarterly_dates <- c("31-Mar-", "30-Jun-", "30-Sep-", "30-Dec-")
  match <- str_match(dates_to_convert, match_regex)
  output <- str_c(quarterly_dates[strtoi(match[,match_cols[1]])], match[,match_cols[2]])
  dates <- strptime(output, "%d-%b-%Y", "GMT")
  return(dates)
}

# Convert month string to index number
monthToNum <- function(month) {
  return(match(month, c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep',  'oct', 'nov', 'dec')))
}

# Convert monthly dates to the right format
convert_monthly_dates <- function(dates_to_convert, match_col, match_regex) {
  monthly_dates <- c("01-31", "02-28", "03-31", "04-30", "05-31", "06-30", "07-31", "08-31", "09-30", "10-31", "11-30", "12-31")
  match <- str_match(dates_to_convert, match_regex)
  lowercase_month <- str_to_lower(match[,3])
  month_index <- monthToNum(lowercase_month)
  output <- str_c(match[,match_col], "-", monthly_dates[month_index])
  dates <- strptime(output, "%Y-%m-%d", "GMT")
  return(dates)
}

# Plot the values given to a line graph
plot_line_data <- function(df, limits, dataName = "Value", isPlottingMultiple = FALSE) {
  limits2=c(as.Date(paste0(limits[1],'-01-31')), as.Date(paste0(limits[2],'-12-31')))
  if (!isPlottingMultiple) {
    ggplot(data = df) +
      geom_line(aes(x = as.Date(Date), y = Value), linetype = "solid", color = "red") +
      scale_x_date(name = "Year",
                   limits = limits2,
                   labels = date_format("%Y")) +
      scale_y_continuous(name = dataName, labels = comma)
  } else {
    ggplot(data = df) +
      geom_line(aes(x = as.Date(Date), y = Value, group = SeriesName, color = SeriesName), linetype = "solid") +
      theme(legend.position = "bottom") + 
      scale_x_date(name = "Year",
                   limits = limits2,
                   labels = date_format("%Y")) +
      scale_y_continuous(name = dataName, labels = comma)
  }
}

# Plot the values given to a box plot
plot_box_data <- function(df) {
  ggplot(data = df) +
    geom_boxplot(aes(y = Value))
}
