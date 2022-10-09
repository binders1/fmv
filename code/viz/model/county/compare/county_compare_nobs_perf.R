

# Load Font ####
font <- "Open Sans"
loadFont(font)

# Load results ####
perform_density <-
  map("ncb", 
      ~ loadResults(model = .x, res_type = "performance")) %>%
  data.table::rbindlist(., fill = T) %>%
  select(fips, model, rsq, mse, n_obs) %>%
  pivot_longer(
    cols = c(rsq, mse),
    names_to = "stat",
    values_to = "value"
  ) %>%
  mutate(stat = if_else(stat == "rsq", 
                        "R-Squared", 
                        "Mean Squared Error")
         )

# VIZ ####  
perform_density %>%
  ggplot(aes(n_obs, value)) +
  
  geom_point(colour = "grey75",
             alpha = 0.4,
             size = 1) +
  
  geom_smooth(
    size = 0.5, method = "loess", 
    se = FALSE,
    colour = brewer.pal(4, "Paired")[4]
    ) +
  
  geom_hline(yintercept = 0) +
  
  facet_wrap(~stat, scales = "free_y") +
  
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(limits = c(0, NA)) +
  
  labs(
    x = "\n# Observations in Model (Train + Test)",
    y = NULL 
  ) +
  
  theme(
    text = element_text(size = 24, family = font),
    
    axis.ticks = element_blank(), 
    
    plot.title = element_text(size = 28, face = "bold"),
    plot.subtitle = element_text(size = 20),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.background = element_blank(),
    plot.margin = margin(rep(20, 4)),
    
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey70",
                                      size = 0.3),
    panel.spacing = unit(20, "pt"),
    
    legend.key = element_blank(),
    legend.title = element_blank(),
    
    strip.background = element_blank()
  )

  
  png(
    filename = "~/output/exhibits/compare_county_nobs_perf.png",
    res = 600,
    width = 6,
    height = 4,
    units = "in",
    pointsize = 10
  )
dev.off()
