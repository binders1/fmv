source("~/fmv/code/viz/model/frr/viz_frr_prep.R")

# Variable Importance ####

imp_path <- file.path(imp_dir, list.files(imp_dir))

imp_frr <- map_dfr(imp_path, read_parquet)
### Clean FRR Importance data ####

importance_frr_clean <- imp_frr %>%
  
  left_join(
    ag_regions_key
  ) %>%
  relocate(frr = "frr_name") %>%
  
  
  pivot_longer(
    cols = !c(frr,id),
    names_to = "Variable",
    values_to = "Importance") %>%
  
  mutate(group = case_when(
    str_detect(Variable, "^Dew") ~ "DewTemp",
    str_detect(Variable, "(?<!Dew)Temp") ~ "Temp",
    str_detect(Variable, "Precip") ~ "Precip",
    str_detect(Variable, "frontage$") ~ "Water Frontage",
    TRUE ~ as.character(Variable)
  ))


#### Full FRR Importance Plot ####

top_n <- 25

importance_frr_clean %>%
  
  group_by(group, frr) %>%
  summarise(Importance = mean(Importance, na.rm = T)) %>%
  
  filter(!is.nan(Importance)) %>%
  
  arrange(group,frr) %>%
  
  
  group_by(group) %>%
  mutate(imp_med = mean(Importance)) %>%
  
  ungroup() %>%
  
  
  group_by(frr) %>%
  
  slice_max(imp_med, n= top_n) %>%
  
  ungroup() %>%
  
  ggplot(aes(Importance, 
             reorder(group, imp_med))) +
  
  geom_point(aes(colour = frr), size = 2.5) +
  
  scale_x_continuous(expand = c(0.01,0), 
                     breaks = seq(0, 1.5, by = 0.25)) +
  scale_y_discrete(expand = c(0.03,0.03)) +
  scale_colour_manual(
    values = frr_colors
  ) +
  
  labs(
    title = "Feature Importance by Farm Resource Region",
    subtitle = glue::glue("Permutation feature importance. Top {top_n} features."),
    caption = "Climatic variables grouped for clarity",
    x = "Feature Importance",
    y = NULL,
    colour = "Farm Resource Region") +
  
  theme(
    text = element_text(family = "Source Sans Pro", size = 25),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    strip.background = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, colour = "grey70"),
    plot.title = element_text(face= "bold"),
    plot.caption = element_text(size = 15, face = "italic"),
    plot.margin = margin(rep(15, 4))
  )

#### FRR Climate Importance Plots ####
importance_frr_clean %>%
  
  filter(str_detect(group, "(Temp|Precip)"),
         !is.nan(Importance)) %>%
  mutate(Variable = str_remove_all(Variable,"(Precip_|DewTempMean_|^Temp)"),
         Variable = str_to_title(Variable)) %>%
  
  
  ggplot(aes(Importance, Variable))+
  
  geom_point(aes(colour = frr), 
             size = 2.5, alpha = 0.9)+
  #geom_boxplot(outlier.shape = NA)+
  
  scale_x_continuous(limits = c(0,NA))+
  
  scale_colour_manual(
    values = frr_colors
  ) +
  
  facet_wrap(~group, scales = "free_y")+
  
  labs(
    title = 'Feature Importance by Farm Resource Region: Climate',
    subtitle = 'Permutation importance. Farm resource region full model.',
    x = "Feature Importance",
    y = NULL,
    colour = "Farm Resource Region")+
  
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    strip.background = element_blank(),
    axis.ticks.y = element_line(size = 0.5),
    text = element_text(family = "Source Sans Pro", size = 18),
    axis.title.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "italic", size = 16),
    panel.grid.major.y = element_line(colour = "grey70"),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(rep(15,4))
  )
