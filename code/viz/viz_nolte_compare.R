# Nolte Comparison ####

## MSE and R-Sq ####

### Plot ####
rbind(
  full_perform,
  nolte_perform) %>%
  
  pivot_longer(
    cols = rsq:mse,
    names_to = "stat",
    values_to = "value"
  ) %>%
  
  mutate(
    stat = case_when(
      stat == "rsq" ~ "R-Squared",
      stat == "mse" ~ "Mean Sq. Error")
  ) %>%
  
  ggplot(aes(value, source)) +
  
  geom_boxplot(outlier.colour = "#081D59",               
               outlier.alpha = 0.4,
               colour = "#081D59") +
  
  facet_wrap(~stat, scales = "free_x") +
  
  labs(
    title = "Model Comparison: Nolte vs. Full",
    subtitle = "County-level performance of extremely randomized trees.",
    caption = glue::glue("\n\n N = {comma(sum(nolte_perform$n_test))} (Nolte) 
                         N = {comma(sum(full_perform$n_test))} (Full)"),
    x = NULL,
    y = NULL) +
  
  theme(
    text = element_text(size = 25, 
                        family = "Source Sans Pro", 
                        colour = "grey30"),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(face = "italic"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(size= 0.2, colour = "grey40"),
    panel.grid.minor.x = element_line(size = 0.1, colour = "grey"),
    panel.border = element_rect(colour = "grey20", fill = NA, size = 1),
    axis.ticks.x = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_text(face= "italic"))

