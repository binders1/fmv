
# Load Packages ####
library(patchwork)

# Load Font ####

font <- "Open Sans"
loadFont(font)

# Performance ####

# bad states 
state_rm <-
  c("02", "15", "60", "66", "69", "72", "78")

# State shp 
state_shp <- list.files("~/fmv/data/spatial/state_shp", 
                        pattern = ".shp$", 
                        full.names = T)
us_states <- st_read(state_shp)

us_states %<>%
  filter(!(STATEFP %in% c("02", "15", "60", "66", 
                          "69", "72", "78")))


## County shp ####

county_shp <- list.files("~/fmv/data/spatial/county_shp", 
                         pattern = ".shp$", 
                         full.names = T)
# County shp
us_counties <-
  st_read(
    county_shp
  )

us_counties %<>%
  filter(!(STATEFP %in% state_rm)) %>%
  select(
    fips = "GEOID"
  )

# fcb MSE spatial


county_performance <-
  map(c("ncb", "fcb"), 
      ~ loadResults(model = .x, res_type = "performance")) %>%
  data.table::rbindlist(., fill = T) %>%
  tibble() %>%
  select(model, fips, mse, rsq) %>%
  mutate(
    model = if_else(model== "fcb", "Full", "Nolte"),
    model = fct_relevel(model, c("Full", "Nolte"))
  )

county_spatial_perf <-
  us_counties %>%
  left_join(county_performance,
            by = "fips")

county_mse_90pct <-
  quantile(county_spatial_perf$mse, probs = .9, na.rm = TRUE)
 
county_spatial_mse <-
  county_spatial_perf %>%
    filter(mse <= county_mse_90pct) %>%
  na.omit()
    


# Compare MSE ####
county_mse <-
  ggplot() +
  
  geom_sf(data = us_counties,
          fill = "white",
          colour = "white",
          size = 0) +
  
  geom_sf(data = us_states, 
          colour = "grey30", 
          size = 0.3, 
          fill = NA) +
  
  geom_sf(data = county_spatial_mse,
          aes(fill = mse), colour = NA,
          alpha = 0.8) +
  
  facet_wrap(~model, nrow = 1) +
  
  scale_fill_gradientn(
    colors = rev(brewer.pal(9, "RdYlGn")),
    na.value = "white",
    limits = c(0,NA),
    breaks = seq(0, 1.25, by = 0.25)
  ) +
  
  guides(fill = guide_colorbar(barwidth = 0.5, 
                                barheight = 10,
                               frame.colour = "black",
                               ticks.colour = "black")) +
  
  coord_sf(crs = st_crs(2163))+
  
  labs(
    fill = "MSE"
  ) +
  
  theme(
    text = element_text(size = 25, family = font),
    legend.text = element_text(size = 18),
    plot.title = element_text(size = 30, face = "bold"),
    plot.margin = margin(rep(15, 4)),
    panel.background = element_blank(),
    strip.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


# R-squared ####
county_rsq <-
  ggplot() +

  geom_sf(data = na.omit(county_spatial_perf),
          aes(fill = rsq), colour = NA,
          alpha = 1) +
  
  geom_sf(data = us_states, 
          colour = "grey30", 
          size = 0.3, 
          fill = NA) +
  
  geom_sf(data = us_counties,
          fill = NA,
          colour = "white",
          size = 0) +

  
  facet_wrap(~model, nrow = 1) +
  
  scale_fill_gradientn(
    colors = brewer.pal(9, "RdYlBu"),
    limits = c(0,1),
    breaks = seq(0, 1, by = 0.25),
    na.value = NA
  ) +
  
  guides(fill = guide_colourbar(barwidth = 0.5, 
                                barheight = 10,
                                frame.colour = "black",
                                ticks.colour = "black")) +
  
  coord_sf(crs = st_crs(2163))+
  
  labs(
    title = "County Model Performance Comparison",
    subtitle = "R-Squared (top) and mean squared error (bottom) by county. >90th percentile of MSE removed for clarity.",
    fill = "R-Sq"
  )+
  
  theme(
    text = element_text(size = 25, family = font),
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 22),
    plot.margin = margin(rep(15, 4)),
    panel.background = element_blank(),
    legend.spacing = unit(0, units = "pt"),
    strip.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.text = element_text(size = 18)
  )

(county_rsq)/
  county_mse
  


