



# Average MSE nad R^2
frr_size_cat_mse %>%
  filter(model == "Full") %>%
  group_by(size_cat) %>%
  summarise(avg_mse = mean(mse, na.rm = TRUE))


