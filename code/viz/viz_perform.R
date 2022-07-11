

### Map ####
rsq_map_nolte <- nolte_df %>%
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





  
