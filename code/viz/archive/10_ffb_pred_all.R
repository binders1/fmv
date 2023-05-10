

ffb_pred_all <- function() {
  
  predict_bldg <-
    file.path(m.dir, "predict_everything") %>%
    list.files(full.names = TRUE) %>%
    .[!str_detect(., "no")]
  
  all_predictions <-
    map_dfr(
      .x = predict_bldg,
      .f = ~ load_pred_all(file = .x, .pred, sid, x, y)
    ) %>%
    rowid_to_column(var = "sid_row")
  
  #================================================================
  # Spatial analysis ####
  #================================================================
  
  tessel_crop <- make_us_tessel(us_states)
  
  tessel_sf <-
    tibble(geometry = tessel_crop) %>%
    rowid_to_column(var = "cell_id") %>%
    st_set_geometry(value = "geometry")
  
  
  tessel_pred <-
    # create df of just tessel cell id's
    tessel_sf %>% 
    st_drop_geometry() %>%
    
    # create list-column of all the sid's contained within each cell
    mutate(
      sid_row = 
        map(
          st_contains(tessel_crop, all_predictions), 
          as.character
        ) 
    ) %>%
    
    # unnest sid's contained within each cell
    unnest(sid_row) %>%
    
    mutate(sid_row = as.integer(sid_row)) %>%
    
    # perform join to attach soil attributes
    left_join(
      all_predictions %>% st_drop_geometry(),
      by = "sid_row"
    ) %>%
    
    # group by tessel cell
    group_by(cell_id) %>%
    
    # average predicted FMV within each cell
    summarise(
      mean_log_priceadj_ha = mean(.pred, na.rm = TRUE)
    ) %>%
    
    # join cell spatial attributes
    right_join(
      tessel_sf,
      by = "cell_id"
    ) %>% 
    
    # create sf object out of the cells 
    st_set_geometry(value = "geometry") %>%
    
    # filter out any POINT sfc rows from geometry
    filter(!str_detect(st_geometry_type(.), "POINT")) %>%
    
    mutate(across(.fns = replace_na, 0))
  
  # ============================================
  # Plot with geom_sf()
  # ============================================
  
  ggplot() +
    
    geom_sf(data = state_5070, 
            fill = "lightgrey", colour = NA,
            size = 0.3) +
    
    geom_sf(data = tessel_pred, 
            aes(fill = exp(mean_log_priceadj_ha)), 
            colour = NA) +
    
    geom_sf(data = state_5070, 
            fill = NA,
            size = 0.3) +
    
    scale_fill_gradientn(
      colours = brewer.pal(9, "RdYlGn") %>% rev(),
      na.value = NA,
      trans = "log10",
      labels = c("1", "10", "100", "1k", "10k", "100k"),
      breaks = 10^(0:5)
    ) +
    
    labs(
      fill = "$/ha"
    ) +
    
    guides(
      fill = guide_colorbar(
        barwidth = 17,
        barheight = 0.6,
        frame.colour = "black",
        ticks.colour = "black",
        title.hjust = 0.5,
        title.position = "top"
      )
    ) +
    
    coord_sf(crs = st_crs(2163)) +
    
    theme(
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.position = "bottom",
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    )
}





