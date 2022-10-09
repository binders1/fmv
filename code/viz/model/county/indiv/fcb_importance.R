
# Importance ####
fcb_imp <- 
  loadResults("fcb", "importance") %>%
  
  mutate(fips = ifelse(nchar(fips)==4, paste0("0",fips),
                       fips)) %>%
  
  mutate(state = str_sub(fips, 1, 2)) %>%
  
  pivot_longer(
    cols = !c(fips,state, model, type),
    names_to = "Variable",
    values_to = "Importance") %>%
  
  mutate(group = case_when(
    str_detect(Variable, "^Dew") ~ "DewTemp",
    str_detect(Variable, "(?<!Dew)Temp") ~ "Temp",
    str_detect(Variable, "Precip") ~ "Precip",
    TRUE ~ as.character(Variable)
  )) %>%
  group_by(group, fips) %>%
  summarise(Importance = mean(Importance, na.rm = T)) %>%
  
  filter(!is.nan(Importance)) %>%
  
  arrange(group,fips) %>%
  
  
  group_by(group) %>%
  mutate(imp_med = median(Importance)) %>%
  ungroup() %>%
  mutate(group = case_when(
    group %in% nolte2020vars ~ paste0("<span style='color:Grey;'>", 
                                      group,
                                      "</span>"),
    TRUE ~ paste0("<span style='font-weight:1000;'>", 
                  group, 
                  "</span>")),
    group = fct_reorder(group, imp_med))

# VIZ
ggplot(data = fcb_imp,
       aes(Importance, 
           reorder(group, imp_med))) +
  
  stat_boxplot(geom = "errorbar",
               size = 0.5) +
  geom_boxplot(size = 0.4, 
               colour = "black", outlier.size = 1,
               outlier.alpha = 0.1, outlier.colour = "black") +
  
  geom_vline(
    xintercept = 0
  )+
  
  scale_x_continuous(expand = c(0.01,0), 
                     breaks = seq(0, 3, by = 0.5)) +
  scale_y_discrete(expand = c(0,0)) +
  
  labs(
    x = "Feature Importance",
    y = NULL)+
  
  theme(
    text = element_text(family = "sans", size = 11),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size = 12),
    axis.text.y = ggtext::element_markdown(size = 10),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(size = 0.3, colour = "grey70"),
    panel.grid.major.y = element_blank()
    )



# Top N Importance ####
# VIZ
fcb_imp %>%
  group_by(fips) %>%
  slice_max(order_by = imp_med, n = 20) %>%
  ungroup() %>%

ggplot(aes(Importance, 
           reorder(group, imp_med))) +
  
  stat_boxplot(geom = "errorbar",
               size = 0.5,
               width = 0.5) +
  geom_boxplot(size = 0.5, colour = "black", outlier.size = 1.2,
               outlier.alpha = 0.1, outlier.colour = "black") +
  geom_vline(xintercept = 0) +
  scale_x_continuous(expand = c(0.01,0), 
                     breaks = seq(0, 3, by = 0.5)) +
  scale_y_discrete(expand = c(0.03,0.03)) +
  
  labs(
    x = "Feature Importance",
    y = NULL
    ) +
  
  theme(
    text = element_text(family = font, size = 25),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size = 19),
    axis.text.y = ggtext::element_markdown(size = 20),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(size = 0.3, colour = "grey70"),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face= "bold"),
    plot.subtitle = ggtext::element_markdown(size = 20),
    plot.caption = element_text(size = 15, face = "italic")
  )

