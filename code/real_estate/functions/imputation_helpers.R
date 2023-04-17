
# Combine distance generation and imputation into single function
impute_pipeline <- function(focal_fips, 
                            buffer_neighbors, 
                            all_study_with_missing) {
  
  impute_complete <-
    neighborhood_distance(focal_fips, buffer_neighbors, all_study_with_missing) %>%
    mhv_impute(focal_fips)
  
  message("Completed: ", focal_fips, "\n")
  
  impute_complete
  
}

# Generates distances between focal county and all buffer neighbors
neighborhood_distance <- function(focal_fips, 
                                  buffer_neighbors, 
                                  all_study_with_missing) {
  
  ### Grab counties within 150km buffer neighborhood ####
  current_neighbors <- 
    buffer_neighbors %>%
    filter(fips == focal_fips,
           neighbor_fips != focal_fips) %>%
    pull(neighbor_fips)
  
  neighbor_distances <-
    buffer_neighbors %>%
    filter(fips == focal_fips,
           neighbor_fips != focal_fips) %>%
    select(
      fips = "neighbor_fips",
      inv_dist_sq
    )
  
  all_study_with_missing %>%
    filter(fips %in% current_neighbors) %>%
    left_join(neighbor_distances,
              by = "fips")
  
}

# Calculates annual median home value mean in buffer neighbors
# weighted by the inverse square distance from focal county centroid
mhv_impute <- function(distance_data, focal_fips) {
  
  distance_data %>%
    group_by(year) %>%
    summarise(
      mhv_imputed =
        weighted.mean(medhomeval,
                      inv_dist_sq,
                      na.rm = TRUE)
    ) %>%
    
    mutate(fips = focal_fips) %>%
    
    relocate(fips)
  
}
