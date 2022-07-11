## Predicted-Actual Visualization ####

setwd("~/fmv/data/model/county")

full_predictions <- map_dfr(states, 
                            ~ read_parquet(paste0("rf/predictions/pred_" ,
                                                  .x, ".pqt")))

nolte_predictions <- map_dfr(states, 
                             ~ read_parquet(paste0("nolte/predictions/pred_" ,
                                                   .x, ".pqt")))


nolte_predictions %>% 
  slice_sample(n = 50000) %>%
  ggplot(aes(.pred, log_priceadj_ha)) +
  
  geom_point(alpha = 0.2, color = "#327ba8", size =0.5) +
  geom_smooth(method= "lm", lty = 2, se = T, size = 0.8, 
              colour = "red")+
  geom_abline(lty = 1, size = 0.4, color = "gray50")+
  
  scale_x_continuous(limits = c(-2,15))+
  scale_y_continuous(limits = c(-2,15))+
  
  
  labs(title = "Prediction Accuracy: Nolte",
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