


# Average MSE and R^2
perf_fcb_ncb %>%
  group_by(model, stat) %>%
  summarise(avg_value = mean(value, na.rm = TRUE)) %>%
  pivot_wider(
    names_from = model,
    values_from = avg_value
  )