
nolte_resid_time <- function() {
  
  # Load Results ####
  mods_to_load <-
    c("ncb", "nch")
  
  nolte_pred <-
    map(mods_to_load,
        ~ loadResults(.x, 
                      res_type = "predictions")
    )
  
  # Clean ####
  
  set.seed(1345)
  
  nolte_pred_clean <-
    data.table::rbindlist(nolte_pred,
                          fill = TRUE) %>%
    slice_sample(n = 3e+05) %>%
    mutate(error = .pred - log_priceadj_ha,
           year = lubridate::year(date)) %>%
    group_by(model, year) %>%
    summarise(mean_error = mean(error),
              .groups = "keep") %>%
    mutate(
      model = if_else(
        model == "ncb", 
        "Restricted", 
        "Restricted + HPI")
    )
  
  
  # VIZ residual over time: Restricted and Restricted + HPI ####
  nolte_pred_clean %>%
    ggplot(aes(year, mean_error, colour = model)) +
    
    geom_hline(yintercept = 0) +
    
    geom_line(size = 1.5) +
    
    geom_point(shape = 21, size = 3.5,
               fill = "white") +
    
    scale_y_continuous(limits = c(-0.3,0.35),
                       breaks = seq(-0.3, 0.3, by = 0.1),
                       labels = scales::comma_format()) +
    
    scale_colour_manual(
      values = c(
        `Restricted` = brewer.pal(4, "Paired")[3],
        `Restricted + HPI` = brewer.pal(4, "Paired")[4]
      )
    ) +
    
    labs(
      x = NULL,
      y = "Mean Prediction Error"
    ) +
    
    fmv_theme +
    
    theme(
      legend.title = element_blank()
    )  
  
}





