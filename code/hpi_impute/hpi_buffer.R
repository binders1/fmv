library(sf)

setwd("~/fmv/data/county_shp")

county_shp <- st_read("cb_2018_us_county_20m.shp")

county_clean <- 
  county_shp %>% 
  janitor::clean_names() %>%
  select(fips = "geoid", name, state = "statefp") %>%
  filter(!(state %in% c("02", "15", "72"))) %>%
  arrange(fips) %>%
  st_as_sf()

county_buffer <- st_buffer(county_clean[1:10,], 150000)

# Intersect Analysis ####

intersect <- st_intersects(county_buffer, county_clean, sparse = F)
  

intersect %<>% 
  data.frame() %>%
  mutate(across(everything(), as.numeric)) %>%
  rowid_to_column(var = "county_1") %>%
  pivot_longer( 
    cols = !county_1,
    names_to = "county_2",
    values_to = "intersect",
    names_prefix = "X"
  ) %>%
  mutate(county_2 = as.integer(county_2)) %>%
  filter(intersect == 1) %>%
  select(!intersect)
  

county_lookup1 <-
  county_clean %>%
  st_drop_geometry() %>%
  rowid_to_column() %>%
  select(county_1 = "rowid", fips)

county_lookup2 <-
  county_clean %>%
  st_drop_geometry() %>%
  rowid_to_column() %>%
  select(county_2 = "rowid", fips)


intersect %<>%
  left_join(county_lookup1, by = "county_1") %>%
  left_join(county_lookup2, by = "county_2") %>%
  transmute(
    county_1 = fips.x,
    county_2 = fips.y
  )

intersect %>% count(county_1) %>% summarise(max = max(n))

write_parquet(intersect,
              "~/fmv/data/hpi_buffer/hpi_buffer.pqt"
              )







  
