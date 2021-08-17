##############################
# Load in your new data sets here
# Date format is yyyy-mm-dd e.g. 2020-04-01
##############################
# Load libraries
library(stringr)
library(tidyverse)
library(scales)


# Load functions
source("src/functions.R")


# Load GDP data
gdp_df <- read.csv(file = './data/GDP (seasonally adjusted).csv', header = TRUE)
colnames(gdp_df) <- c('DateChar', 'GDP')
quarter_regex <- "(\\d+) Q(\\d)"

gdp_df2 = gdp_df %>%
  mutate(SeriesName = 'GDP') %>%
  mutate(Frequency = ifelse(str_detect(DateChar, 'Q') == TRUE,'Quarter', 'Annual')) %>%
  mutate(Value = GDP) %>%
  mutate(Date = case_when(
    Frequency == 'Quarter' ~ convert_quarterly_dates(DateChar, c(3, 2), quarter_regex),
    Frequency == 'Annual'  ~ as.POSIXlt(as.Date(paste0(DateChar, '-12-31'), "%Y-%m-%d", "GMT")),
  )) %>%
  mutate(SourceLink = 'https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/abmi/pn2') %>%
  mutate(SourceName = 'Office for National Statistics') %>%
  select(SeriesName, Frequency, Date, Value, SourceLink, SourceName)

# Calculate GDP Growth
gdp_growth = gdp_df2 %>%
  arrange(Frequency, Date) %>%
  group_by(Frequency) %>%
  mutate(oldValue=Value) %>%
  mutate(lagOldValue=dplyr::lag(oldValue, order_by = Date)) %>%
  mutate(Value=oldValue / lagOldValue -1) %>%
  mutate(SeriesName='GDPGrowth') %>%
  select("SeriesName", "Frequency", "Date", "Value", "SourceLink", "SourceName") %>%
  filter(is.na(Value)==FALSE)

# Load consumer price index data
cpi_df <- read.csv(file = './data/CPIH Annual Rate All Items 2015=100.csv', header = TRUE)
colnames(cpi_df) <- c('DateChar', 'CPI')
months <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')
quarter_regex <- "(\\d+) Q(\\d)"
month_regex <- "(\\d+) (\\w+)"

cpi_df2 = cpi_df %>%
  mutate(SeriesName = 'CPI') %>%
  mutate(Frequency = case_when(
    str_detect(DateChar, 'Q') == TRUE ~ 'Quarter',
    (str_match(DateChar, month_regex)[,3] %in% months) == TRUE ~ 'Month',
    TRUE ~ 'Annual'
  )) %>%
  mutate(Value = CPI) %>%
  mutate(Date = case_when(
    Frequency == 'Quarter' ~ convert_quarterly_dates(DateChar, c(3, 2), quarter_regex),
    Frequency == 'Month' ~ convert_monthly_dates(DateChar, 2, month_regex),
    Frequency == 'Annual'  ~ as.POSIXlt(as.Date(paste0(DateChar, '-12-31'), "%Y-%m-%d", "GMT"))
  )) %>%
  mutate(SourceLink = 'https://www.ons.gov.uk/file?uri=%2femploymentandlabourmarket%2fpeopleinwork%2femploymentandemployeetypes%2fdatasets%2femploymentunemploymentandeconomicinactivityforpeopleaged16andoverandagedfrom16to64seasonallyadjusteda02sa%2fcurrent/a02sadec2020.xls') %>%
  mutate(SourceName = 'Office for National Statistics') %>%
  select(SeriesName, Frequency, Date, Value, SourceLink, SourceName)


# Load unemployment rate data
unemployment_df <- read.csv(file = './data/Unemployment rate (seasonally adjusted).csv', header = TRUE)
colnames(unemployment_df) <- c('DateChar', 'Unemployment')
month_regex <- "(\\w+)-(\\w+) (\\d+)"

unemployment_df2 = unemployment_df %>%
  mutate(SeriesName = 'Unemployment') %>%
  mutate(Frequency = 'Month') %>%
  mutate(Value = Unemployment) %>%
  mutate(Date = convert_monthly_dates(DateChar, 4, month_regex)) %>%
  mutate(SourceLink = 'https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindices/current/mm23.xlsx') %>%
  mutate(SourceName = 'Office for National Statistics') %>%
  select(SeriesName, Frequency, Date, Value, SourceLink, SourceName)


# Load house price index data
hpi_df <- read.csv(file = './data/House price index.csv', header = TRUE)
colnames(hpi_df) <- c('DateChar', 'AHP', 'HPI Q1', 'HPI', 'Quarterly Change', 'Year Change')
quarter_regex <- "Q(\\d) (\\d+)"

hpi_df2 = hpi_df %>%
  mutate(SeriesName = 'HPI') %>%
  mutate(Frequency = 'Quarter') %>%
  mutate(Value = HPI) %>%
  mutate(Date = convert_quarterly_dates(DateChar, c(2, 3), quarter_regex)) %>%
  mutate(SourceLink = 'https://www.nationwidehousepriceindex.co.uk/download/uk-quarterly-indices-post-91') %>%
  mutate(SourceName = 'Nationwide') %>%
  select(SeriesName, Frequency, Date, Value, SourceLink, SourceName)


# Combine data frames into one
total_df <- bind_rows(gdp_df2, gdp_growth, cpi_df2, unemployment_df2, hpi_df2) %>%
  mutate(Date=as.Date(Date))

rm('cpi_df2',
   'cpi_df',
   'gdp_df2',
   'gdp_df',
   'gdp_growth',
   'hpi_df',
   'hpi_df2',
   'unemployment_df2',
   'unemployment_df')

