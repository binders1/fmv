county_compare_boxplot <- function() {
  
  require(patchwork)
  
  # Load sid-level predictions from all models
  mod_pred <-
    map(
      .x = c("fcb", "ncb"),
      .f = loadResults,
      res_type = "predictions"
    )
  
  common_parcel_vec <- common_parcels(mod_pred)
  
  filtered_to_common <-
    bind_rows(mod_pred) %>%
    
    select(model, sid, .pred, log_priceadj_ha) %>%
    
    mutate(fips = str_sub(sid, 1, 5)) %>%
    
    filter(sid %in% common_parcel_vec)
  
  
  calc_fips_metrics <-
    filtered_to_common %>%
    group_by(model, fips) %>%
    mutate(
      sq_error = (log_priceadj_ha - .pred)^2,
      ssr = sum(sq_error, na.rm = TRUE),
      sst = (log_priceadj_ha - mean(log_priceadj_ha))^2 %>% sum(na.rm = TRUE),
      
      mse = mean(sq_error, na.rm = TRUE),
      rsq = 1 - (ssr/sst)
    ) %>%
    ungroup() %>%
    select(model, fips, mse, rsq) %>%
    distinct() %>%
    
    pivot_longer(
      cols = c(rsq, mse),
      names_to = "stat",
      values_to = "value"
    ) %>%
    
    mutate(
      model = case_when(
        model == "fcb" ~ "Full",
        model == "ncb" ~ "Restricted"),
      stat = if_else(stat == "rsq", 
                     "R-Squared", 
                     "Mean Squared Error")
    ) %>%
    
    dplyr::filter(!is.infinite(value))
  
  
  calc_fips_metrics %>%
    group_by(model, stat) %>%
    summarise(median = median(value, na.rm = T))
  
  # ====================================================
  # Boxplot VIZ ####
  # ====================================================

  mse_plot <-
    calc_fips_metrics %>%
    filter(stat == "Mean Squared Error") %>%
    
    ggplot(aes(model, value, fill = model)) +
    
    stat_boxplot(geom = "errorbar",
                 size = 0.3, width = 0.05) +
    geom_boxplot(alpha = 1, size = 0.2, 
                 width = 0.2, colour = "black", 
                 outlier.colour = NA) +
    
    geom_hline(yintercept = 0, size = 0.4) +
    
    #facet_wrap(~stat, scales = "free_y") +
    
    scale_y_continuous(limits = c(0, 2)) +
  
    scale_fill_manual(
      values = c(
        `Full` = brewer.pal(4, "Paired")[1],
        `Restricted` = brewer.pal(4, "Paired")[3]
      )
    ) +
    
    labs(
      y = NULL,
      x = NULL
    ) +
    
    fmv_theme +
    
    theme(
      legend.title = element_blank(),
      legend.position = "none"
    )
  
  
  
  rsq_plot <-
    calc_fips_metrics %>%
    filter(stat == "R-Squared") %>%
    
    ggplot(aes(model, value, fill = model)) +
    
    stat_boxplot(geom = "errorbar",
                 size = 0.3, width = 0.05) +
    geom_boxplot(alpha = 1, size = 0.2, 
                 width = 0.2, colour = "black", 
                 outlier.colour = NA) +
    
    geom_hline(yintercept = 0, size = 0.4) +
    
    #facet_wrap(~stat, scales = "free_y") +
    
    scale_y_continuous(limits = c(0, 1)) +
    
    scale_fill_manual(
      values = c(
        `Full` = brewer.pal(4, "Paired")[1],
        `Restricted` = brewer.pal(4, "Paired")[3]
      )
    ) +
    
    labs(
      y = NULL,
      x = NULL
    ) +
    
    fmv_theme +
    
    theme(
      legend.title = element_blank(),
      legend.position = "none"
    )
  
  mse_plot + rsq_plot
  
}
