# Predictions ####

pred_path <-
  file.path(pred_dir,
            list.files(file.path(pred_dir))
  )

frr_pred <- 
  map_dfr(pred_path,
          read_parquet) %>%
  slice_sample(n = 2e+05)


frr_pred %>%
  ggplot(aes(.pred, log_priceadj_ha)) +
  geom_point(alpha = 0.2, color = "#327ba8", size = 0.5) +
  geom_smooth(method= "lm", lty = 2, se = T, size = 0.8, 
              colour = "red")+
  geom_abline(lty = 1, size = 0.4, color = "gray50")+
  
  scale_x_continuous(limits = c(-5,15))+
  scale_y_continuous(limits = c(-5,15))+
  
  
  labs(title = "Prediction Accuracy: Farm Resource Regions",
       subtitle = "Extremely Randomized Trees. Cost units in logged 2020 $/ha",
       x = "Estimated Cost",
       y = "Actual Cost")+
  
  coord_equal(1)+
  
  theme(axis.ticks = element_blank(), 
        text = element_text(size = 24, family = "Source Sans Pro"),
        plot.title = element_text(size = 28, face = "bold"),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 16, face = "italic"),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey92"),
        panel.border = element_rect(colour = "black", fill = NA), 
        strip.background = element_blank(), 
        plot.background = element_blank())




# Accuracy by year ####

frr_pred %>%
  select(frr_name, .pred, log_priceadj_ha, date) %>%
  mutate(error = log_priceadj_ha - .pred,
         year = year(date)) %>%
  
  ggplot(aes(date, error)) +
  
  geom_point(size = 1, alpha = 0.3, colour = "#327ba8") +
  
  geom_smooth(method= "lm", lty = 1, se = F, size = 0.8, 
              colour = "red") +
  
  scale_colour_manual(
    values = frr_colors
  ) +
  
  scale_x_datetime(expand = c(0.05,0.05)) +
  
  labs(
    title = "Prediction Residual by Date: Farm Resource Region",
    subtitle = "Residual in logged 2020 $. Line of best fit in red",
    y = "Residual",
    x = NULL
  ) +
  
  theme(
    text = element_text(size = 22, family = "Source Sans Pro"),
    plot.title = element_text(size = 30, face = "bold"),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "grey30"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.5, colour = "grey80"),
    axis.ticks.y = element_line(size = 0.5, colour = "grey80"),
    plot.margin = margin(rep(15, 4))
  )


