
county_buffer <- function(buffer_km) {
  
  buffer_m <- buffer_km*1000
  
  # Load and clean counties shapefile
  county_file <- 
    list.files(county_dir, pattern = "shp$",
               full.names = TRUE)
  
  county_shp <- st_read(county_file)
  
  county_clean <- 
    county_shp %>% 
    janitor::clean_names() %>%
    select(fips = "geoid", name, state = "statefp") %>%
    filter(!(state %in% c("02", "15", "72"))) %>%
    arrange(fips) %>%
    st_as_sf()
  
  
  # Create county buffer features
  county_buffer <- 
    st_buffer(county_clean,
              buffer_m)
  
  
  # Find counties within each county's buffer
  intersect <- 
    st_intersects(
      county_buffer, 
      county_clean, 
      sparse = TRUE
      )
  
  intersect %<>%
    data.frame() %>%
    rename(
      focal_county = row.id,
      neighbor = col.id
    )
  
  # Create focal/neighbor lookup table
  
  county_lookup <-
    county_clean %>% 
    st_drop_geometry() %>%
    rowid_to_column() %>%
    select(rowid, fips)
  
  intersect %<>%
    left_join(county_lookup,
              by = c("focal_county" = "rowid")
              ) %>%
    left_join(county_lookup,
              by = c("neighbor" = "rowid")
              ) %>%
    transmute(
      focal_county = fips.x,
      neighbor = fips.y
    ) 
  
  # Calculate Centroids
  county_centroid <- 
    st_centroid(county_clean) %>%
    select(!c(name, state))
  
  # Add centroid geometry to buffer table
  intersect_geom <-
    intersect %>%
    left_join(
      county_centroid,
      by = c("focal_county" = "fips")
    ) %>%
    left_join(
      county_centroid,
      by = c("neighbor" = "fips")
    ) %>%
    rename(
      focal_geom = geometry.x,
      neighbor_geom = geometry.y
    )
  
  # Calculate distance between centroids ####
  intersect_geom %>%
    mutate(
      dist_m = 
        st_distance(
          focal_geom, neighbor_geom,
          by_element = TRUE
          )
    ) %>%
    transmute(
      fips = focal_county,
      neighbor_fips = neighbor,
      dist_m = as.double(dist_m)
    )
  
}

