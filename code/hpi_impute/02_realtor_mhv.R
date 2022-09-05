# Load packages ####
library(tidyverse)
library(lubridate)

# source: https://www.realtor.com/research/data/

# Load data ####
setwd()
realtor_data <- 
  read_parquet("~/fmv/data/mhv_impute/realtor/realtor_listing.pqt")


# Clean county and state names ####
realtor_data %<>%
  separate(
    col = county_name,
    into = c('county_name', 'state_name'),
    sep = ", ",
    fill = "right"
  ) %>%
  mutate(county_name = str_to_title(county_name),
         state_name = toupper(state_name))

# Remove data quality note ####
realtor_data %<>%
  rename(date = "month_date_yyyymm") %>%
  filter(str_detect(date, "^[:digit:]{6}$"))

# Remove AK and HI

realtor_data %<>%
  filter(!(state_name %in% c("AK", "HI", "MH")))


# Parse dates ####
realtor_data %<>%
  mutate(date = ym(date),
         year = year(date),
         month = month(date)) %>%
  relocate(date, year, month)


# Clean fips codes ####
realtor_data %<>%
  mutate(fips = case_when(
    nchar(county_fips) == 4 ~ paste0("0",county_fips),
    TRUE ~ as.character(county_fips)
  ), .keep = "unused") %>%
  relocate(fips, .after = "month")



# Select median listing price ####
realtor_data %<>%
  select(date, year, month, fips, 
         county_name, state_name, 
         median_listing_price,
         quality_flag)

# Replace quality_flag NA's with 0
realtor_data %<>%
  mutate(quality_flag = replace_na(quality_flag, 0))



# Summarise and save county-level median_listing_price from 2017,
# which was the year with the best quality_flag good:bad ratio
medhomeval <- 
  realtor_data %>%
  filter(year == 2017) %>%
  group_by(fips) %>%
  summarise(
    median_listing_price = mean(median_listing_price)
  ) %>%
  mutate(year = 2017)

write_parquet(medhomeval,
              "~/fmv/data/hpi_impute/realtor/medhomeval.pqt")

