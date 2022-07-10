## Predicted-Actual Visualization ####

states <- list.files("~/fmv/data/cleaned") %>%
  str_extract("[:upper:]{2}") %>%
  sort()

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


## Variable Importance ####


### Clean Importance data ####

googlesheets4::range_read(ss = "1AejaWn1ZaNJBTWG2kFnhfq_rqpDKZETdrfUq41opSVU")


importance_df <- map_dfr(states, 
                         ~ read_parquet(paste0('rf/importance/import_', .x, ".pqt"))) 

importance_clean <- importance_df %>%
  
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

### Plot ####

importance_clean %>%
  
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


### Ridgeline plot by Ag Region ####

importance_clean %>%
  
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



## Climate Importance Plots ####
importance_clean %>%
  
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





## Training/Testing Density ####
setwd("~/fmv/data/model/county")

perform_df <- map_dfr(
  states,
  ~ read_parquet(paste0("rf/performance/stats_", .x, ".pqt"))) %>%
  mutate(across(n_train:n_test, .fns = log, .names = "log_{col}"))

comma(sum(perform_df$n_train)) %>% cat()

comma(sum(perform_df$n_test)) %>% cat()

train_density <- usmap::plot_usmap(data = perform_df,
                    regions = c('counties'),
                    exclude = c('AK','HI'),
                    values = "log_n_train",
                    colour = "grey",
                    size = 0.3)+
    
    ggplot2::scale_fill_gradientn(
      colours = noltecolors,
      na.value = "grey90",
      limits = c(1,11))+
    
    labs(
      title = "Training Density by County",
      fill = "Obs (log)",
      caption = "N = 3,789,841"
    )+
  theme(
    legend.background = element_blank(),
    legend.position = c(0.9,0),
    text = element_text(size = 20, family = "Source Sans Pro"),
    plot.title = element_text(size = 30, face="bold", hjust = 0.5),
    plot.caption = element_text(size = 20, face = "italic",
                                hjust = 0.5)
  )



test_density <- usmap::plot_usmap(data = perform_df,
                                   regions = c('counties'),
                                   exclude = c('AK','HI'),
                                   values = "log_n_test",
                                   colour = "grey",
                                   size = 0.3)+
  
  ggplot2::scale_fill_gradientn(
    colours = noltecolors,
    na.value = "grey90",
    limits = c(1,11))+
  
  labs(
    title = "Testing Density by County",
    fill = "Obs (log)",
    caption = "N = 1,470,393"
  )+
  theme(
    legend.background = element_blank(),
    legend.position = "none",    
    text = element_text(size = 20, family = "Source Sans Pro"),
    plot.title = element_text(size = 30, face="bold", hjust = 0.5),
    plot.caption = element_text(face = "italic", size = 20, 
                                hjust = 0.5)
  )


library(patchwork)

train_density + test_density




## MSE and R-Squared ####


noltecolors <- c('#FBFDD0', '#40B5C4','#081D59')

msecolors <- c('#549A79', '#FDF2A9', '#C3546E')

### Map ####
rsq_map_nolte <- perform_df %>%
  usmap::plot_usmap(data = .,
                    regions = c('counties'),
                    exclude = c('AK','HI'),
                    values = "rsq",
                    colour = "grey",
                    size = 0.2)+
  
  scale_fill_gradientn(
    colours = noltecolors,
    na.value = "grey90",
    limits = c(0.2,1)
  )+
  
  labs(
    title = "Predictive Power (Nolte Features)",
    subtitle = glue::glue("Extremely randomized trees. N = {comma(sum(perform_df$n_test))}"),
    fill = "R-Sq"
  )+
  
  theme(
    text = element_text(size = 30, family = "Source Sans Pro", colour = "grey30"),
    plot.title = element_text(face = "bold"),
    legend.background = element_blank()
  )

mse_map <- perform_df %>%
  usmap::plot_usmap(data = .,
                    regions = c('counties'),
                    exclude = c('AK','HI'),
                    values = "mse",
                    colour = "grey",
                    size = 0.2)+
  
  scale_fill_gradientn(
    colours = msecolors,
    na.value = "grey90",
    limits = c(0,5.5))+
  
  labs(
    fill = "MSE"
  )+
  
  theme(
    legend.position = "right",
    text = element_text(size = 30, family = "Source Sans Pro", colour = "grey30"),
    legend.background = element_blank()
  )

mse_map_filter <- perform_df %>%
  filter(mse <= 1.4 & mse >= 0.5) %>%
  usmap::plot_usmap(data = .,
                    regions = c('counties'),
                    exclude = c('AK','HI'),
                    values = "mse",
                    colour = "grey",
                    size = 0.2)+
  
  scale_fill_gradientn(
    colours = msecolors,
    na.value = "grey90",
    limits = c(0.3, 3)
  )+
  
  labs(
    fill = "MSE"
  )+
  
  theme(
    legend.position = "right",
    text = element_text(size = 30, family = "Source Sans Pro", colour = "grey30"),
    plot.subtitle = element_text(face = "italic"),
    legend.background = element_blank()
  )


mse_map + mse_map_filter +
  plot_annotation(title = "Prediction Error (Nolte Features)",
                  subtitle = glue::glue("Extremely randomized trees. N = {comma(sum(perform_df$n_test))}"),
                  caption = '\n(A) All observations \n(B) 10th to 90th percentile, outliers removed',
                  tag_levels = 'A') &
  theme(plot.title = element_text(size = 33, family = "Source Sans Pro",
                            face = "bold", hjust = 0.5,
                            colour = "grey30"),
        plot.subtitle = element_text(size = 27, family = "Source Sans Pro",
                                     hjust = 0.5, colour = "grey30"),
        plot.caption = element_text(size = 27, family = "Source Sans Pro", 
                                    hjust = 0.5),
        plot.tag = element_text(size = 25))



### Plot ####

perform_df %>%
  dplyr::select(fips, mse, rsq) %>%
  pivot_longer(
    cols = !fips,
    names_to = "stat",
    values_to = "value"
  ) %>%
  
  mutate(stat = ifelse(stat=="mse","Mean Sq. Error", "R-Squared")) %>%
  
  ggplot(aes(value, after_stat(count)))+
  
  geom_density(fill = "grey", colour = "grey25")+
  facet_wrap(~stat, scales = "free")+
  
  labs(
    title = "Model Performance (Nolte)",
    subtitle = glue::glue("Extremely Randomized Trees. N = {comma(sum(perform_df$n_test))}"),
    y = "Count",
    x = NULL)+
  
  theme(
    panel.background= element_blank(),
    panel.border = element_rect(fill = NA, colour = "black", size = 1),
    panel.grid = element_line(colour = "grey"),
    axis.ticks = element_blank(),
    text = element_text(size = 30, family = "Source Sans Pro", colour = "grey25"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    strip.background = element_blank()
  )
 
# FRR Map ####

ag_regions_ref %>%
  mutate(id = as.character(id)) 

frr_colors <- c(`Southern Seaboard` = "#C3AB1B", 
                `Eastern Uplands` = "#71C276", 
                `Basin and Range` = "#EEAB56", 
                `Fruitful Rim` = "#AC842B", 
                `Mississippi Portal` = "#A8D3F2", 
                `Prairie Gateway` = "#A5A5A5", 
                `Northern Great Plains` = "#F3A193", 
                `Northern Crescent` = "#F1EC00", 
                `Heartland` = "#CAC7A1")


usmap::plot_usmap(
  data = ag_regions_ref,
  values = "name",
  regions = c('counties'),
  exclude = c('AK', 'HI'),
  colour = "white",
  size= 0)+
  
  scale_fill_manual(
    values = frr_colors,
    na.value = "#F3A193")+
  
  labs(
    title = "USDA Farm Resource Regions",
    fill = "Farm Resource Region"
  )+
  theme(text = element_text(size = 30, colour = "grey30", family = "Source Sans Pro"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "right")



# Nolte Comparison ####

## MSE and R-Sq ####

setwd('~/fmv/data/model/county')
nolte_perform <- map_dfr(
  states,
  ~ read_parquet(paste0("nolte/performance/stats_", .x, ".pqt"))) %>%
  mutate(across(n_train:n_test, .fns = log, .names = "log_{col}")) %>%
  mutate(source = "Nolte") %>%
  dplyr::select(source, rsq, mse, n_test)

full_perform <- map_dfr(
  states,
  ~ read_parquet(paste0("rf/performance/stats_", .x, ".pqt"))) %>%
  mutate(across(n_train:n_test, .fns = log, .names = "log_{col}")) %>%
  mutate(source = "Full") %>%
  dplyr::select(source, rsq, mse, n_test)

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
  
  geom_boxplot(outlier.colour = "midnightblue",               
               outlier.alpha = 0.4,
               colour = "midnightblue") +
  
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
 

## MSE and R-Sq map ####

ratio_compare_df <- nolte_perform %>%
  rename_with(.fn = ~ paste0(.x, "!!nolte"), .cols = !fips) %>%
  left_join(
    full_perform %>%
      rename_with(.fn = ~ paste0(.x, "!!full"), .cols = !fips)
  ) %>%
  pivot_longer(
    cols = !fips,
    names_to = "stat",
    values_to = "value"
  ) %>%
  separate(col = stat,
           into = c('stat','source'),
           sep = "!!") %>%
  pivot_wider(
    names_from = source,
    values_from = value
  ) %>%
  dplyr::filter(stat %in% c('mse', 'rsq'))


mse_ratio_compare <- ratio_compare_df %>%
  dplyr::filter(stat == "mse") %>%
  dplyr::mutate(mse_ratio = nolte/full) %>%
  filter(!is.na(mse_ratio)) %>%
  mutate(full_better = ifelse(mse_ratio<1, "Worse","Better"))

mse_compare_map <- usmap::plot_usmap(data = mse_ratio_compare,
                    regions = c('counties'),
                    exclude = c('AK','HI'),
                    values = "full_better",
                    colour = "lightgrey",
                    size = 0.2)+
  scale_fill_manual(
    values = c(
      `Better` = "#5fb38b",
      `Worse` = "#de844b"),
    na.value = "grey95")+
  
  labs(
    subtitle = "Mean Sq. Error",
    fill = "Full Better than Nolte?"
  )+
  theme(
    legend.position = c(0.68,-0.1),
    legend.direction = "horizontal",
    text = element_text(size = 25, colour = "grey30", family = "Source Sans Pro"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic")
  )


rsq_ratio_compare <- ratio_compare_df %>%
  dplyr::filter(stat == "rsq") %>%
  dplyr::mutate(rsq_ratio = nolte/full) %>%
  filter(!is.na(rsq_ratio)) %>%
  mutate(full_better = ifelse(rsq_ratio<1, "Better","Worse"))

rsq_compare_map <- usmap::plot_usmap(data = rsq_ratio_compare,
                                     regions = c('counties'),
                                     exclude = c('AK','HI'),
                                     values = "full_better",
                                     colour = "lightgrey",
                                     size = 0.2)+
  scale_fill_manual(
    values = c(
      `Better` = "#5fb38b",
      `Worse` = "#de844b"),
    na.value = "grey95")+
  
  labs(
    subtitle = "R-Squared",
    fill = "Full Better than Nolte?"
  )+
  theme(
    legend.position = "none",
    text = element_text(size = 25, colour = "grey30", family = "Source Sans Pro"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic")
  )

library(patchwork)

mse_compare_map + rsq_compare_map + 
  plot_annotation(title = "Model Comparison by County") &
  theme(plot.title = element_text(size =30, hjust = 0.5, family = "Source Sans Pro",
                                  face = "bold", colour = "grey30"))


## Plot comparison ####
rsq_ratio_compare %>%
  ggplot(aes(rsq_ratio, after_stat(count)))+
  geom_density()

ratio_compare_df %>%
  mutate(ratio = )





  
