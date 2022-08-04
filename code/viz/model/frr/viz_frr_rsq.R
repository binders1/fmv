

# Map County-level R^2 ####

## Load all FRR predictions ####
pred_paths <-
  file.path(pred_dir, list.files(pred_dir))


collect_frr_rsq <- tibble()

for (i in seq_len(length(pred_paths))) {
  
  frr_pred <- 
    read_parquet(pred_paths[[i]])
  
  frr_pred %<>%
    transmute(
      frr_name = frr_name,
      fips = fips,
      log_priceadj_ha,
      .pred) %>%
    group_by(fips) %>%
    summarise(rsq = cor(.pred, log_priceadj_ha)^2)
  
  collect_frr_rsq <- 
    rbind(
      collect_frr_rsq,
      frr_pred
    )
  
}



## Join with sf counties ####

frr_rsq_spatial <-
  us_counties %>%
  left_join(collect_frr_rsq,
            by = "fips")

## Map R2 ####
ggplot() +
  
  geom_sf(data = us_counties,
          fill = "grey90",
          colour = NA) +
  
  geom_sf(data = frr_rsq_spatial, 
          aes(fill = rsq), colour = NA,
          alpha = 0.8) +
  
  geom_sf(data = us_states, 
          colour = "grey30", 
          size = 0.3, 
          fill = NA) +
  
  scale_fill_viridis_b(n.breaks = 7,
                       na.value = NA) +
  
  coord_sf(crs = st_crs(2163)) +
  
  labs(    
    title = "R-Squared by County",
    subtitle = "Farm resource region full model.",
    fill = "R-Squared",
    colour = "Farm Resource Region"
  ) +
  
  theme(
    text = element_text(size = 25, family = "Source Sans Pro"),
    plot.title = element_text(size = 30, face = "bold"),
    plot.margin = margin(rep(15, 4)),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
