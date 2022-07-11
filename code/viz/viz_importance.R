
# Variable Importance ####

## Clean ####

### Clean County Importance data ####

importance_county_clean <- county_imp_full %>%
  
  mutate(fips = ifelse(nchar(fips)==4, paste0("0",fips),
                       fips)) %>%
  
  mutate(state = str_sub(fips, 1, 2)) %>%
  
  pivot_longer(
    cols = !c(fips,state),
    names_to = "Variable",
    values_to = "Importance") %>%
  
  mutate(group = case_when(
    str_detect(Variable, "^Dew") ~ "DewTemp",
    str_detect(Variable, "(?<!Dew)Temp") ~ "Temp",
    str_detect(Variable, "Precip") ~ "Precip",
    str_detect(Variable, "frontage$") ~ "Water Frontage",
    TRUE ~ as.character(Variable)
  ))


### Clean FRR Importance data ####

importance_frr_clean <- frr_imp_full %>%
  
  left_join(
    ag_regions_key
  ) %>%
  relocate(frr = "name") %>%
  
  
  pivot_longer(
    cols = !c(frr,id, source),
    names_to = "Variable",
    values_to = "Importance") %>%
  
  mutate(group = case_when(
    str_detect(Variable, "^Dew") ~ "DewTemp",
    str_detect(Variable, "(?<!Dew)Temp") ~ "Temp",
    str_detect(Variable, "Precip") ~ "Precip",
    str_detect(Variable, "frontage$") ~ "Water Frontage",
    TRUE ~ as.character(Variable)
  ))



## Plot ####

### County ####

#### Full County Importance Plot ####

importance_county_clean %>%
  
  group_by(group, fips) %>%
  summarise(Importance = mean(Importance, na.rm = T)) %>%
  
  filter(!is.nan(Importance)) %>%
  
  arrange(group,fips) %>%
  
  
  group_by(group) %>%
  mutate(imp_med = median(Importance)) %>%
  
  filter(imp_med > 0.01, group != "ha") %>%
  
  ggplot(aes(Importance, 
             reorder(group, imp_med))) +
  
  geom_jitter(width = 0.1, alpha = 0.3, 
              colour = "lightblue", size = 0.75)+
  
  geom_boxplot(outlier.shape = NA) +
  
  annotate("segment", 
           x = 0, 
           xend = 0, 
           y = 1, 
           yend = 27, 
           colour = "#d63a3a",
           lty = 5) +
  
  scale_x_continuous(expand = c(0,0)) +
  
  labs(
    title = "Feature Importance by County",
    subtitle = "Permutation feature importance. Omitting median < 0.01 and hectare.",
    x = "Feature Importance",
    y = NULL)+
  
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    strip.background = element_blank(),
    axis.ticks.y = element_line(size = 0.5),
    text = element_text(family = "IBM Plex Sans", size = 18),
    axis.title.x = element_text(face = "bold"),
    plot.title = element_text(face= "bold"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )


#### County Ridgeline plot by Ag Region ####

importance_county_clean %>%
  
  group_by(group, fips) %>%
  summarise(Importance = mean(Importance, na.rm = T)) %>%
  
  filter(!is.nan(Importance)) %>%
  
  arrange(group,fips) %>%
  
  
  group_by(group) %>%
  mutate(imp_med = median(Importance)) %>%
  ungroup() %>%
  
  filter(imp_med > 0.01, group != "ha") %>%
  
  left_join(ag_regions_ref) %>%
  
  mutate(ag_region = name, .keep = "unused") %>% 
  filter(!is.na(ag_region)) %>% 
  
  ggplot(aes(Importance, reorder(group, imp_med))) +
  
  ggridges::geom_density_ridges() +
  
  facet_wrap(~ag_region) +
  
  scale_x_continuous(expand = c(0,0)) +
  
  labs(
    title = "Feature Importance by Farm Resource Region",
    subtitle = "Permutation feature importance. Omitting median < 0.01 and hectare.",
    x = "Feature Importance",
    y = NULL)+
  
  theme(
    text = element_text(family = "IBM Plex Sans", size = 18),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    strip.background = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_text(face = "bold"),
    plot.title = element_text(face= "bold"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )



#### County Climate Importance Plots ####
importance_county_clean %>%
  
  filter(str_detect(group, "(Temp|Precip)"),
         !is.nan(Importance)) %>%
  mutate(Variable = str_remove_all(Variable,"(Precip_|DewTempMean_|^Temp)"),
         Variable = str_to_title(Variable)) %>%
  
  
  ggplot(aes(Importance, Variable))+
  
  geom_jitter(width = 0.1, size = 0.75, alpha = 0.2, 
              colour = "lightblue")+
  geom_boxplot(outlier.shape = NA)+
  
  scale_x_continuous(limits = c(-0.1,0.2))+
  
  facet_wrap(~group, scales = "free")+
  
  labs(
    title = 'Feature Importance: Climate',
    subtitle = 'Mean permutation feature importance. 1.5 x IQR boxplots, extreme outliers removed for clarity.',
    x = "Feature Importance",
    y = NULL)+
  
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    strip.background = element_blank(),
    axis.ticks.y = element_line(size = 0.5),
    text = element_text(family = "IBM Plex Sans", size = 18),
    axis.title.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "italic", size = 20),
    panel.grid.major.x = element_line(colour = "darkgrey"),
    plot.margin = margin(t = 10, r = 10, 
                         b = 10, l = 10)
  )




### FRR ####

#### Full FRR Importance Plot ####

top_n <- 25

importance_frr_clean %>%
  
  group_by(group, frr) %>%
  summarise(Importance = mean(Importance, na.rm = T)) %>%
  
  filter(!is.nan(Importance)) %>%
  
  arrange(group,frr) %>%
  
  
  group_by(group) %>%
  mutate(imp_med = median(Importance)) %>%
  
  ungroup() %>%
  
  
  group_by(frr) %>%
  
  slice_max(imp_med, n= top_n) %>%
  
  ungroup() %>%
  
  ggplot(aes(Importance, 
             reorder(group, imp_med))) +
  
  geom_boxplot(colour = "grey30",
               outlier.colour = "grey30",
               outlier.alpha = 0.5) +
  
  annotate("segment", 
           x = 0, 
           xend = 0, 
           y = 1, 
           yend = top_n+1, 
           colour = "#d63a3a",
           lty = 5) +
  
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  
  labs(
    title = "Feature Importance by Farm Resource Region",
    subtitle = glue::glue("Permutation feature importance. Top {top_n} features."),
    x = "Feature Importance",
    y = NULL) +
  
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    strip.background = element_blank(),
    axis.ticks.y = element_blank(),
    text = element_text(family = "IBM Plex Sans", size = 18),
    axis.title.x = element_text(face = "bold"),
    plot.title = element_text(face= "bold"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )


#### FRR Ridgeline plot by Ag Region ####

importance_frr_clean %>%
  
  group_by(group, fips) %>%
  summarise(Importance = mean(Importance, na.rm = T)) %>%
  
  filter(!is.nan(Importance)) %>%
  
  arrange(group,fips) %>%
  
  
  group_by(group) %>%
  mutate(imp_med = median(Importance)) %>%
  ungroup() %>%
  
  filter(imp_med > 0.01, group != "ha") %>%
  
  left_join(ag_regions_ref) %>%
  
  mutate(ag_region = name, .keep = "unused") %>% 
  filter(!is.na(ag_region)) %>% 
  
  ggplot(aes(Importance, reorder(group, imp_med))) +
  
  ggridges::geom_density_ridges() +
  
  facet_wrap(~ag_region) +
  
  scale_x_continuous(expand = c(0,0)) +
  
  labs(
    title = "Feature Importance by Farm Resource Region",
    subtitle = "Permutation feature importance. Omitting median < 0.01 and hectare.",
    x = "Feature Importance",
    y = NULL)+
  
  theme(
    text = element_text(family = "IBM Plex Sans", size = 18),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    strip.background = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_text(face = "bold"),
    plot.title = element_text(face= "bold"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )



#### FRR Climate Importance Plots ####
importance_frr_clean %>%
  
  filter(str_detect(group, "(Temp|Precip)"),
         !is.nan(Importance)) %>%
  mutate(Variable = str_remove_all(Variable,"(Precip_|DewTempMean_|^Temp)"),
         Variable = str_to_title(Variable)) %>%
  
  
  ggplot(aes(Importance, Variable))+
  
  geom_jitter(width = 0.1, size = 0.75, alpha = 0.2, 
              colour = "lightblue")+
  geom_boxplot(outlier.shape = NA)+
  
  scale_x_continuous(limits = c(-0.1,0.2))+
  
  facet_wrap(~group, scales = "free")+
  
  labs(
    title = 'Feature Importance: Climate',
    subtitle = 'Mean permutation feature importance. 1.5 x IQR boxplots, extreme outliers removed for clarity.',
    x = "Feature Importance",
    y = NULL)+
  
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    strip.background = element_blank(),
    axis.ticks.y = element_line(size = 0.5),
    text = element_text(family = "IBM Plex Sans", size = 18),
    axis.title.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "italic", size = 20),
    panel.grid.major.x = element_line(colour = "darkgrey"),
    plot.margin = margin(t = 10, r = 10, 
                         b = 10, l = 10)
  )