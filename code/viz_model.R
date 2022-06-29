# Maps ####

noltecolors <- c('#FBFDD0', '#40B5C4','#081D59')

msecolors <- c('#549A79', '#FDF2A9', '#C3546E')

mse_plot_rf <- countyPlot(collect_stats_rf,
                          var = "mse",
                          title = "Random Forest: Prediction Error by County",
                          include = "Arkansas",
                          stat_name = "Mean Sq. Error",
                          colours = msecolors,
                          limits = c(0,2.5))

rsq_plot_rf <- countyPlot(collect_stats_rf,
                          var = "rsq",
                          title = "Random Forest: Predictive Power by County",
                          include = "Arkansas",
                          stat_name = "R-squared",
                          colours = noltecolors,
                          limits = c(0,0.75))


mse_plot_base <- countyPlot(collect_stats_base,
                            var = "mse",
                            title = "Regression: Prediction Error by County",
                            include = "Arkansas",
                            stat_name = "Mean Sq. Error",
                            colours = msecolors,
                            limits = c(0,2.5))

rsq_plot_base <- countyPlot(collect_stats_base,
                            var = "r.squared",
                            title = "Regression: Predictive Power by County",
                            include = "Arkansas",
                            stat_name = "R-squared",
                            colours = noltecolors,
                            limits = c(0,0.75))

library(patchwork)

(mse_plot_rf | mse_plot_base) /
(rsq_plot_rf | rsq_plot_base) +
  plot_annotation(caption = "Predicting log($/ha) with full set of Nolte (2020) features") &
  theme(plot.caption = element_text(size = 20, face = "italic",
                                    family = "IBM Plex Sans"))




# Predicted-Actual Visualization ####

states <- list.files("~/fmv/data/cleaned") %>%
  str_extract("[:upper:]{2}") %>%
  sort()

setwd("~/fmv/data/model/county/rf")

predictions <- map_dfr(states, 
                       ~ read_parquet(paste0("predictions/pred_" ,
                                             .x, ".pqt")))


predictions %>% 
  slice_sample(n = 50000) %>%
  ggplot(aes(.pred, log_priceadj_ha)) +
  
  geom_point(alpha = 0.2, color = "#327ba8", size =0.5) +
  geom_smooth(method= "lm", lty = 2, se = T, size = 0.8, 
              colour = "red")+
  geom_abline(lty = 1, size = 0.4, color = "gray50")+
  
  scale_x_continuous(limits = c(-2,15))+
  scale_y_continuous(limits = c(-2,15))+
  
  
  labs(title = "Prediction Accuracy",
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


# Variable Importance ####


### Clean Importance data ####

googlesheets4::range_read(ss = "1AejaWn1ZaNJBTWG2kFnhfq_rqpDKZETdrfUq41opSVU")


importance_df <- map_dfr(states, 
                         ~ read_parquet(paste0('importance/import_', .x, ".pqt"))) 

importance_clean <- importance_df %>%
  
  mutate(fips = ifelse(nchar(fips)==4, paste0("0",fips),
                       fips)) %>%
  
  mutate(state = str_sub(fips, 1, 2)) %>%
  
  pivot_longer(
    cols = !c(fips,state),
    names_to = "Variable",
    values_to = "Importance") %>%
  
  group_by(Variable) %>%
  summarise(imp_mean = mean(Importance, na.rm = T),
            imp_sd = sd(Importance, na.rm = T)) %>%

  mutate(group = case_when(
    str_detect(Variable, "^Dew") ~ "DewTemp",
    str_detect(Variable, "(?<!Dew)Temp") ~ "Temp",
    str_detect(Variable, "Precip") ~ "Precip",
    str_detect(Variable, "frontage$") ~ "Water Frontage",
    TRUE ~ as.character(Variable)
  )) %>%
  filter(Variable != 'ha')

### Plot ####
importance_clean %>%
  group_by(group) %>%
  summarise(across(imp_mean:imp_sd, mean)) %>%
  
  ggplot(aes(imp_mean, reorder(group, imp_mean)))+
  #geom_bar(stat = 'identity', fill = "#46abdb")+
  
  geom_errorbar(aes(xmin=imp_mean-imp_sd, xmax=imp_mean+imp_sd),
                width=.5, colour = "grey30")+
  
  geom_point(colour = "#2fb1bd")+
  
 # facet_wrap(~ state)+
  
#  scale_x_continuous(limits = c(-0.005,2), 
#                     expand = c(0,0))+
  
  annotate("segment", 
           x = 0, 
           xend = 0, 
           y = 1, 
           yend = 45, 
           colour = "grey3",
           lty = 5)+
  
  labs(
    title = "Feature Importance by County",
    subtitle = "Mean permutation feature importance. Errors bars represent one std. dev.",
    x = "Feature Importance",
    y = NULL)+
  
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    strip.background = element_blank(),
    axis.ticks.y = element_line(size = 0.5),
    text = element_text(family = "IBM Plex Sans", size = 18),
    axis.title.x = element_text(face = "bold"),
    plot.title = element_text(face= "bold")
  )



## Climate Importance Plots ####
dewtemp_imp <- importance_clean %>%
  dplyr::filter(str_detect(Variable, "^DewTemp")) %>%
  
  ggplot(aes(imp_mean, Variable))+
  
  geom_errorbar(aes(xmin=imp_mean-imp_sd, xmax=imp_mean+imp_sd),
                width=.5, colour = "grey30")+
  
  geom_point(colour = "#2fb1bd")+
  
  scale_x_continuous(limits = c(0,0.15))+
  
  facet_wrap(~group)+
  
  annotate("segment", 
           x = 0, 
           xend = 0, 
           y = 0.2, 
           yend = 6, 
           colour = "grey3",
           lty = 5)+
  
  labs(
    x = "Feature Importance",
    y = NULL)+
  
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    strip.background = element_blank(),
    axis.ticks.y = element_line(size = 0.5),
    text = element_text(family = "Source Sans Pro", size = 18),
    axis.title.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 20)
  )


(dewtemp_imp | precip_imp | temp_imp) +
  plot_annotation(
    title = 'Feature Importance: Climate',
    subtitle = 'Mean permutation feature importance. Errors bars represent one std. dev.') &
  theme(plot.title = element_text(size = 30, face = "bold", family = "Source Sans Pro",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 20, family = "Source Sans Pro",
                                     hjust= 0.5))





## Training/Testing Density ####

perform_df <- map_dfr(
  states,
  ~ read_parquet(paste0("performance/stats_", .x, ".pqt"))) %>%
  mutate(across(n_train:n_test, .fns = log, .names = "log_{col}"))

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

