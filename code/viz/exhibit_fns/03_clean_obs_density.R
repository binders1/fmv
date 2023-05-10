
clean_obs_density <- function() {
  
  # Load full cleaned data ####
  clean_to_load <-
    list.files(clean.dir, 
               full.names = TRUE,
               pattern = "pqt$")
  
  clean_sf <-
    map_dfr(clean_to_load, read_parquet) %>%
    select(sid, x, y) %>%
    st_as_sf(coords = c("x", "y"), crs = st_crs(5070))
  
  # EPSG:5070 projection of states
  state_5070 <- 
    st_transform(us_states, crs = st_crs(5070))
  
  # Combine obs and hex tessellation ####
  
  # Bind predicted obs to hex's
  clean_intersect <-
    st_intersects(conus_tessel_sf, clean_sf) %>%
    map_int(length)
  
  # Count obs in each hex
  tessel_clean_count <-
    conus_tessel_sf %>%
    st_as_sf() %>%
    mutate(n = clean_intersect) %>%
    st_set_geometry("geometry") %>%
    filter(!str_detect(st_geometry_type(.), "POINT"))
  
  # VIZ ####
  
    ggplot() +
    
    geom_sf(data = state_5070, fill = "grey85", size = 0) +
    
    geom_sf(data = tessel_clean_count, aes(fill = n), size = 0) +
    
    geom_sf(data = state_5070, fill = NA, size = 0.3) +

    scale_fill_gradientn(
      colours = c("grey85", brewer.pal(9, "YlOrRd")),
      na.value = NA,
      trans = "log10",
      breaks = log_breaks(6),
      labels = label_number(scale_cut = cut_short_scale())
    ) +
    
    labs(
      fill = "Observed\nSales"
    ) +
    
    guides(
      fill = guide_colorbar(
        barwidth = 0.4,
        barheight = 8,
        frame.colour = "black",
        ticks.colour = "black",
        title.hjust = 0,
        title.position = "top"
      )
    ) +
    
    fmv_theme +
    
    theme(axis.text = element_blank(),
          legend.position = "right")
  
}
