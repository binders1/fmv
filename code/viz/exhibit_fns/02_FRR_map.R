
FRR_map <- function() {
  
  ggplot() +
    
    geom_sf(data = frr_shp, 
            aes(fill = frr_name),
            colour = NA) +
    
    geom_sf(data = us_counties, 
            fill = NA,
            colour = "white",
            size = 0.05) +
    
    geom_sf(data = us_states, fill = NA,
            size = 0.3) +
    
    scale_fill_manual(
      values = frr_colors
    ) +
    
    coord_sf(crs = st_crs(2163)) +
    
    labs(
      fill = "Farm Resource Region"
    ) +
    
    fmv_theme +
    
    theme(
      axis.text = element_blank()
    )  
  
}
