
# Load Results ####
mods_to_load <-
  c("ncb", "nch")

nolte_pred <-
  map(mods_to_load,
      ~ loadResults(.x, 
                res_type = "predictions")
      )

# Clean ####

nolte_pred_clean <-
  data.table::rbindlist(nolte_pred,
                      fill = TRUE) %>%
  slice_sample(n = 2e+05) %>%
  select(model, .pred, log_priceadj_ha, date) %>%
  mutate(error = .pred - log_priceadj_ha,
         year = lubridate::year(date)) %>%
  group_by(model, year) %>%
  summarise(mean_error = mean(error),
            se_error = sd(error)/sqrt(n())) %>%
  mutate(
    model = if_else(
      model == "ncb", 
      "Restricted", 
      "Restricted + HPI")
    )


# VIZ residual over time by model ####
nolte_pred_clean %>%
  ggplot(aes(year, mean_error, colour = model)) +
  
  geom_hline(yintercept = 0) +
  
  geom_line(size = 0.75) +
  
  geom_point(shape = 21, size = 1.75,
             fill = "white") +
  
  scale_y_continuous(limits = c(-0.3,0.35),
                     breaks = seq(-0.3, 0.3, by = 0.1),
                     labels = scales::comma_format()) +
  
  scale_colour_manual(
    values = c(
      `Restricted` = brewer.pal(4, "Paired")[3],
      `Restricted + HPI` = brewer.pal(4, "Paired")[4]
    )
  ) +
  
  labs(
    x = NULL,
    y = "Mean Prediction Error"
  ) +
  
  theme(
    text = element_text(family = "sans", size = 15),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey70", size = 0.3),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(10, "pt")
  )



