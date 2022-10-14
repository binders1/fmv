compare_county_nobs_perf <- function() {
  
  # Load results ####
  perform_density <-
    map("ncb", 
        ~ loadResults(model = .x, res_type = "performance")) %>%
    data.table::rbindlist(., fill = T) %>%
    select(fips, model, rsq, mse, n_obs) %>%
    pivot_longer(
      cols = c(rsq, mse),
      names_to = "stat",
      values_to = "value"
    ) %>%
    mutate(stat = if_else(stat == "rsq", 
                          "R-Squared", 
                          "Mean Squared Error")
    )
  
  
  # VIZ ####  
  perform_density %>%
    ggplot(aes(n_obs, value)) +
    
    geom_point(colour = "grey75",
               alpha = 0.4,
               size = 1) +
    
    geom_smooth(
      method = "loess", 
      se = FALSE,
      formula = y ~ x,
      size = 1, 
      colour = brewer.pal(4, "Paired")[4]
    ) +
    
    geom_hline(yintercept = 0) +
    
    facet_wrap(~stat, scales = "free_y") +
    
    scale_x_log10(labels = scales::comma) +
    scale_y_continuous(limits = c(0, NA)) +
    
    labs(
      x = "\n# Observations in Model (Train + Test)",
      y = NULL 
    ) +
    
    
    fmv_theme +
    theme(
      panel.spacing = unit(20, "pt"),
      plot.margin = margin(rep(10, 4))
    )
  
}
