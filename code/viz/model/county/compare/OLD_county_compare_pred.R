county_compare_pred

county_results <-
  map(c("nch", "ncb", "fcb"), 
    ~ loadResults(model = .x, res_type = "predictions"))


pred_sample <-
  data.table::rbindlist(county_results, fill = T) %>%
  tibble() %>%
  slice_sample(n = 1e+05) %>%
  select(model, .pred, log_priceadj_ha) %>%
  mutate(
    model = case_when(
      model == "fcb" ~ "Full",
      model == "ncb" ~ "Nolte (w/o HPI)",
      model == "nch" ~ "Nolte (w/ HPI)"
    ) 
    )



pred_sample %>%
  
  ggplot(aes(.pred, log_priceadj_ha)) +
  
  geom_point(alpha = 0.2, color = "#327ba8", size =0.5) +
  geom_smooth(method= "lm", lty = 2, se = T, size = 0.8, 
              colour = "red")+
  geom_abline(lty = 1, size = 0.4, color = "gray50")+
  
  scale_x_continuous(limits = c(-2,15))+
  scale_y_continuous(limits = c(-2,15))+
  
  facet_wrap(~model) +
  
  
  labs(
    title = "County Model Comparison: Predicted vs. Actual Cost",
    x = "Estimated Cost",
    y = "Actual Cost") +
  
  coord_equal(1) +
  
  theme(axis.ticks = element_blank(), 
        text = element_text(size = 24, family = "Source Sans Pro"),
        plot.title = element_text(size = 28, face = "bold"),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 16, face = "italic"),
        plot.background = element_blank(),
        plot.margin = margin(rep(20, 4)),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey92"),
        panel.border = element_rect(colour = "black", fill = NA), 
        strip.background = element_blank()
        )



