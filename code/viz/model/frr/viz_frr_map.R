source("~/fmv/code/viz/model/frr/viz_frr_prep.R")

ggplot() +
  
  geom_sf(data = frr_shp, 
          aes(fill = frr_name),
          colour = NA) +
  
  geom_sf(data = us_states, fill = NA,
          size = 0.3) +
  
  geom_sf(data = us_counties, 
          fill = NA,
          colour = "white",
          size = 0.05) +
  
  scale_fill_manual(
    values = frr_colors
  ) +
  
  coord_sf(crs = st_crs(2163)) +
  
  labs(
    fill = "Farm Resource Region"
  ) +
  
  theme(
    text = element_text(size = 25, family = "Source Sans Pro"),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )