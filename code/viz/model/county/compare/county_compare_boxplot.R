# Load Fonts
font <- "Open Sans"
loadFont(font)

mods_to_load <-
  c("fcb", "ncb")

perf_fcb_ncb <-
  map(
    mods_to_load,
    ~ loadResults(model = .x,
                  res_type = "performance")
  )

perf_fcb_ncb %<>%
  map_dfr(.,
          ~ select(.x,
                   model, fips, rsq, mse)) %>%
  pivot_longer(
    cols = c(rsq, mse),
    names_to = "stat",
    values_to = "value"
  ) %>%
  mutate(
    model = case_when(
      model == "fcb" ~ "Full",
      model == "ncb" ~ "Nolte"),
    stat = if_else(stat == "rsq", 
                   "R-Squared", 
                   "Mean Squared Error")
    )

# Boxplot VIZ ####

perf_fcb_ncb %>%
  ggplot(aes(model, value, fill = model)) +
  
  stat_boxplot(geom = "errorbar",
               size = 0.3, width = 0.05) +
  geom_boxplot(alpha = 1, size = 0.2, 
               width = 0.2, colour = "black", 
               outlier.colour = NA) +
  
  geom_hline(yintercept = 0, size = 0.4) +
  
  
  facet_wrap(~stat, scales = "free_y") +
  
  scale_y_continuous(limits = c(0, NA)) +
  
  scale_fill_manual(
    values = c(
      `Full` = brewer.pal(4, "Paired")[1],
      `Nolte` = brewer.pal(4, "Paired")[3]
    )
  ) +
  
  labs(
    title = "County Model Comparison: Performance",
    subtitle = "Full and Nolte base county models.\n",
    y = NULL,
    x = NULL
  ) +
  
  theme(
    text = element_text(family = font, size = 24),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 27),
    plot.subtitle = element_text(size = 20),
    plot.margin = margin(rep(20, 4)),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey70", size = 0.3),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(10, "pt")
  )


