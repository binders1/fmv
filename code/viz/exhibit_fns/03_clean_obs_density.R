
clean_obs_density <- function() {
  
  # Create tessellation ####
  
  # EPSG:5070 projection of states
  state_5070 <- st_transform(us_states, 
                             crs=st_crs(5070))
  
  # Set cell size
  cs <- 10e+03
  
  # Make tesselation of CONUS
  state_grid <- sf::st_make_grid(state_5070,
                                 cellsize = cs,
                                 square = FALSE)
  
  # Retrieve hex's that overlap with states
  overlap_idx <- 
    st_intersects(state_grid, state_5070) %>%
    map_int(., 
            length) %>% 
    as.logical()
  
  # Filter to only overlapping hex's
  state_tessel <-
    state_grid[overlap_idx]
  
  # Trim tessellation to fit state polygon extent 
  tessel_crop <-
    st_intersection(state_tessel,
                    state_5070)
  
  # Load full cleaned data ####
  
  clean_to_load <-
    list.files(clean.dir, 
               full.names = TRUE)
  
  clean_sf <-
    map_dfr(clean_to_load, read_parquet) %>%
    select(sid, x, y) %>%
    st_as_sf(coords = c("x", "y"), crs = st_crs(5070))
  
  
  # Combine obs and hex tessellation ####
  
  # Bind predicted obs to hex's
  clean_intersect <-
    st_intersects(tessel_crop,
                  clean_sf) %>%
    map_int(., length)
  
  # Count obs in each hex
  tessel_clean_count <-
    tessel_crop %>%
    st_as_sf() %>%
    mutate(n = clean_intersect) %>%
    st_set_geometry(., "geometry") %>%
    filter(!str_detect(st_geometry_type(.), "POINT"))
  
  # VIZ ####
  
    ggplot() +
    
    geom_sf(data = tessel_clean_count, aes(fill = n), size = 0) +
    
    geom_sf(data = state_5070, 
            fill = NA, size = 0.3) +
    
    scale_fill_gradientn(
      colours = c("grey85", brewer.pal(9, "YlOrRd")),
      trans = "pseudo_log",
      breaks = c(0, 10^seq(3))
    ) +
    
    labs(
      fill = "Observed Sales"
    ) +
    
    guides(
      fill = guide_colorbar(
        barwidth = 10,
        barheight = 0.6,
        frame.colour = "black",
        ticks.colour = "black",
        title.hjust = 0.5,
        title.position = "top"
      )
    ) +
    
    fmv_theme +
    
    theme(axis.text = element_blank(),
          legend.position = "bottom")
  
}
