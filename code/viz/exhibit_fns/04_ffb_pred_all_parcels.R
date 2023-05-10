
ffb_pred_all_parcels <- function() {
  
  predict_all_pc <-
    file.path(m.dir, "model_all_parcels") %>%
    list.files(full.names = TRUE)
  
  all_predictions <-
    map_dfr(
      predict_all_pc,
      ~ load_pred_all(file = .x, pid, .pred, x, y)
    ) %>%
    rowid_to_column(var = "pidrow")
  
  # EPSG:5070 projection of states
  state_5070 <- 
    st_transform(us_states, crs = st_crs(5070))
  
  #================================================================
  # Spatial analysis ####
  #================================================================
  
  tessel_pred <-
    # create df of just tessellation cell identifiers
    conus_tessel_sf %>% 
    st_drop_geometry() %>%
    
    # create list-column of all the sid's contained within each cell
    mutate(
      pidrow = 
        map(
          st_contains(conus_tessel_sf, all_predictions), 
          as.character
        ) 
    ) %>%
    
    # unnest sid's contained within each cell
    unnest(pidrow) %>%
    
    mutate(pidrow = as.integer(pidrow)) %>%
    
    # perform join to attach soil attributes
    left_join(
      all_predictions %>% st_drop_geometry(),
      by = "pidrow"
    ) %>%
    
    # group by tessel cell
    group_by(cell_id) %>%
    
    # average predicted FMV within each cell
    summarise(
      mean_priceadj_ha = mean(exp(.pred), na.rm = TRUE)
    ) %>%
    
    # join cell spatial attributes
    right_join(
      conus_tessel_sf,
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
            aes(fill = mean_priceadj_ha), 
            colour = NA) +
    
    geom_sf(data = state_5070, 
            fill = NA,
            size = 0.3) +
    
    scale_fill_gradientn(
      colours = rev(brewer.pal(9, "RdYlGn")),
      na.value = NA,
      trans = "log10",
      breaks = log_breaks(7),
      labels = label_number(scale_cut = cut_short_scale())
    ) +
    
    labs(
      fill = "$/ha"
    ) + 
    guides(
      fill = guide_colorbar(
        barwidth = 0.4,
        barheight = 8,
        frame.colour = "black",
        ticks.colour = "black",
        title.hjust = 0.5,
        title.position = "top"
      )
    ) +
    #guides(
    #  fill = guide_colorbar(
    #    barwidth = 17,
    #    barheight = 0.6,
    #    frame.colour = "black",
    #    ticks.colour = "black",
    #    title.hjust = 0.5,
    #    title.position = "top"
    #  )
    #) +
    
    coord_sf(crs = st_crs(2163)) +
    
    theme(
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.position = c(1, 0.4),
      legend.background = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    )
  
}


