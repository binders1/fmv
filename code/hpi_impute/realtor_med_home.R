# Load packages ####
library(tidyverse)
library(lubridate)

# source: https://www.realtor.com/research/data/

# Load data ####
setwd("~/fmv/data/hpi_impute/realtor")
realtor_data <- 
  read_parquet("realtor_listing.pqt")


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




# VIZ ####
viz_data <-
  realtor_data %>% 
  mutate(quality_flag = replace_na(quality_flag, 0)) %>%
  count(state_name, month, quality_flag) %>%
  group_by(state_name, month) %>%
  mutate(total = sum(n),
         prop = n/total,
         quality_flag = factor(quality_flag))
  
viz_data %>%
  ggplot(aes(month, prop,
             colour = quality_flag)) + 
  geom_point() +
  
  geom_smooth() +
  
  scale_y_continuous(limits = c(0,1)) +
  
  scale_x_continuous(breaks = seq_len(12)) +
  
  theme_minimal()
  


realtor_data %>%
  mutate(quality_flag = replace_na(quality_flag, 0),
         year = fct_reorder(as.character(year), -year)) %>%
  filter(quality_flag == 0) %>%
  count(state_name, year) %>%
  group_by(state_name) %>%
  mutate(
    total = sum(n),
    prop = n/total
  ) %>%
  ungroup() %>%
  
  mutate(state_name = fct_reorder(state_name, prop),
         ) %>%
  
  ggplot(aes(state_name, prop, fill = year)) +
  
  geom_bar(stat = "identity", position = "stack") +
  
  scale_fill_viridis_d()



realtor_data %>%
  count(year, quality_flag) %>%
  add_row(year = 2016, quality_flag = 1, n = 0) %>%
  mutate(quality_flag = fct_reorder(as.character(quality_flag), quality_flag)) %>%
  ggplot(aes(factor(year), n, fill = quality_flag)) +
  geom_bar(stat = "identity", position = "dodge") +
  
  scale_fill_viridis_d(begin = 0.3, end = 0.8) +
  
  theme_linedraw()



county_shp <- st_read("~/fmv/data/county_shp/cb_2018_us_county_20m.shp")



medhomeval %>%
  left_join(county_shp %>%
              mutate(fips = paste0(STATEFP, COUNTYFP)), 
            by = c("fips")) %>%
  
  st_as_sf() %>%
  ggplot(aes(fill = log(median_listing_price))) +
  
  geom_sf(size = 0.1, alpha = 0.8) +
  
  coord_sf(crs = st_crs(3347)) +
  
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(8, "YlGnBu"))




