# Load Font ####
font <- "Open Sans"
loadFont(font)


mods_to_load <-
  c("ffb","fcb")

ffb_fcb_pred <-
  map(mods_to_load, 
      ~ loadResults(model = .x, res_type = "predictions"))


pred_sample <-
  data.table::rbindlist(ffb_fcb_pred, fill = TRUE) %>%
  slice_sample(n = 2e+05) %>%
  tibble() %>%
  select(model, .pred, log_priceadj_ha)%>%
  mutate(
    model = case_when(
      model == "ffb" ~ "FRR Models",
      model == "fcb" ~ "County Models"
    ),
    model = fct_relevel(model, c("FRR Models", "County Models"))
  )



pred_sample %>%
  
  ggplot(aes(.pred, log_priceadj_ha)) +
  
  geom_point(alpha = 0.2, size =0.5, colour = "#327ba8") +
  geom_smooth(method= "lm", lty = "longdash", se = T, size = 0.8,
              colour = "red")+
  geom_abline(lty = 1, size = 0.4, color = "gray50")+
  
  scale_x_continuous(limits = c(-2,15))+
  scale_y_continuous(limits = c(-2,15))+

  facet_wrap(~model) +
  
  
  labs(
    title = "Model Comparison: Predicted vs. Actual Cost",
    subtitle = "Predicted vs. actual in FRR and county models. Cost in logged 2020 USD per hectare.",
    x = "Predicted Cost",
    y = "Actual Cost") +
  
  coord_equal(1) +
  
  theme(axis.ticks = element_blank(), 
        text = element_text(size = 24, family = font),
        plot.title = element_text(size = 28, face = "bold"),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 16, face = "italic"),
        plot.background = element_blank(),
        plot.margin = margin(rep(20, 4)),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey90", size = 0.3),
        panel.border = element_rect(colour = "black", fill = NA), 
        strip.background = element_blank()
  )
