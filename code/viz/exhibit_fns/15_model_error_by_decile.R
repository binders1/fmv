
model_error_by_decile <- function() {
  
  ncb_ffb_error_decile <-
    map_dfr(
      c("ffb", "ncb"),
      loadResults,
      res_type = "predict_all"
      ) %>%
    mutate(error = .pred - log_priceadj_ha,
           model =
             if_else(
               model == "ffb",
               "Full FRR Model",
               "Restricted County Model"
               )
           ) %>%
    group_by(model) %>%
    mutate(decile = ntile(log_priceadj_ha, 10)) %>%
    group_by(model, decile) %>%
    summarise(mean_error = mean(error), iqr = IQR(error))
               
  
  ncb_ffb_error_decile %>%
    ggplot(aes(decile, mean_error, 
               fill = model, 
               group = model)) +
    geom_bar(stat = "identity", position = "dodge",
             alpha = 0.8) +
    geom_hline(yintercept = 0) +
    geom_errorbar(
      aes(ymin = mean_error - iqr/2,
          ymax = mean_error + iqr/2),
      position = position_dodge(0.9),
      width = 0.2
    ) +
    scale_x_continuous(breaks = seq(1, 10)) +
    scale_fill_manual(
      values = 
        c(
          `Full FRR Model` = brewer.pal(4, "Paired")[2],
          `Restricted County Model` = brewer.pal(4, "Paired")[4]
        )
    ) +
    labs(
      y = "Mean Prediction Error",
      x = "Cost Decile",
      fill = NULL
    ) +
    fmv_theme
      
}