# Load Fonts
font <- "Source Sans Pro"
loadFont(font)



ncb_perf <- loadResults("ncb", "performance") %>%
  select(model, type, rsq, mse)

nch_perf <- loadResults("nch", "performance") %>%
  select(model, type, rsq, mse)

fcb_perf <- loadResults("fcb", "performance") %>%
  select(model, type, rsq, mse)

compare_perf <- rbind(ncb_perf, nch_perf, fcb_perf)


compare_perf %<>%
  mutate(
    author = case_when(
      str_detect(model, "^f") ~ "Full",
      str_detect(model, "^n") ~ "Nolte"),
    geo = case_when(
      str_detect(model, "^.{1}c") ~ "County",
      str_detect(model, "^.{1}f") ~ "FRR")
  ) %>%
  pivot_longer(
    cols = c(rsq, mse),
    names_to = "stat",
    values_to = "value"
  ) %>%
  mutate(
    model = case_when(
      model == "fcb" ~ "Full",
      model == "ncb" ~ "Nolte (w/o HPI)",
      model == "nch" ~ "Nolte (w/ HPI)"),
    stat = if_else(stat == "rsq", "R-Squared", "Mean Square Error"))

compare_perf %>%
  ggplot(aes(value, fill = model)) +
  
  geom_histogram(position = "identity",
                 alpha = 0.3, colour = NA) +
  
  facet_wrap(~stat, scales = "free_x")+

  scale_fill_manual(
    values = brewer.pal(3, "Set1")
  ) +
  
  labs(
    fill = NULL,
    x = NULL,
    y = "Count"
  ) +
  
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(colour = "grey30", fill = NA),
    text = element_text(family = font, size = 20),
    strip.background = element_blank(),
    plot.margin = margin(rep(20,4))
  )
  
  
  
  
  