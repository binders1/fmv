# Maps

noltecolors <- c('#FBFDD0', '#40B5C4','#081D59')

msecolors <- c('#549A79', '#FDF2A9', '#C3546E')

mse_plot_rf <- countyPlot(collect_stats_rf,
                            var = "mse",
                            title = "Random Forest: Prediction Error by County",
                            include = "Arkansas",
                            stat_name = "Mean Sq. Error",
                            colours = msecolors)

rsq_plot_rf <- countyPlot(collect_stats_rf,
                            var = "rsq",
                            title = "Random Forest: Predictive Power by County",
                            include = "Arkansas",
                            stat_name = "R-squared",
                            colours = noltecolors)

library(patchwork)

(mse_plot_rf | rsq_plot_rf) /
(mse_plot_base | rsq_plot_base) +
  plot_annotation(caption = "Predicting log($/ha) with full set of Nolte (2020) features") &
  theme(plot.caption = element_text(size = 20, face = "italic",
                                    family = "IBM Plex Sans"))




# Predicted-Actual Visualization ####

collect_predictions(rf_fit) %>% # change input to all-county predictions
  ggplot(aes(log_priceadj_ha, .pred)) +
  geom_point(alpha = 0.4, color = "midnightblue") +
  geom_abline(lty = 5, color = "gray50") +
  labs(x = "$/ha (log)",y = "Predicted Values")+
  coord_equal(1)+
  theme_bw(base_size = 18, base_family = "IBM Plex Sans")

# run this line of code alone to get a dataframe of predicted vs. actual and squared error
collect_predictions(rf_fit) |>
  mutate(diff = .pred - log_priceadj_ha,
         sq_error = diff^2) %>%
  pull(sq_error) %>%
  mean() %>%
  sqrt()




# Variable Importance ####

# construct variable importance model object
# from the best-performing model, based on initial 
# ranger Random Forest specification above (ranger_spec)
imp_spec <- ranger_spec %>%
  finalize_model(select_best(ranger_tune, metric = "rmse")) %>%
  set_engine("ranger", importance = "impurity")

# visualize variable importance
workflow() %>%
  add_recipe(ranger_recipe) %>%
  add_model(imp_spec) %>%
  fit(test) %>%
  extract_fit_parsnip() %>%
  vip(aesthetics = list(alpha = 0.8, fill = "lightblue"))+
  theme_minimal(base_size = 18, base_family = "IBM Plex Sans")