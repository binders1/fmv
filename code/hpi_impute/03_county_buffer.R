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


# Centroid calcs ####
county_centroid <- st_centroid(county_clean) %>%
  select(!c(name, state))

first_geo_join <- 
  intersect %>%
  left_join(
    county_centroid %>%
      rename(county_1 = "fips"),
    by = "county_1") %>%
  rename(geo_1 = "geometry")


second_geo_join <-
  first_geo_join %>%
  left_join(
    county_centroid %>%
      rename(county_2 = "fips"),
    by = "county_2") %>%
  rename(geo_2 = "geometry")



# Calc distance between centroids ####
centroid_dist_calc <-
  second_geo_join %>%
    mutate(
      dist_m = st_distance(geo_1, geo_2, by_element = T)
      ) %>%
  transmute(
    fips = county_1,
    buffer_fips = county_2,
    dist_m = as.double(dist_m)
  )


# Write buffer/distance dataframe to file ####
write_parquet(centroid_dist_calc,
              "~/fmv/data/hpi_impute/hpi_buffer.pqt"
              )
