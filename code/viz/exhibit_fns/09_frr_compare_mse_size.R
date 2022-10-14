
frr_compare_mse_size <- function() {
  
  # Get vector of <1000 obs counties ####
  ncb_size_cat <-
    loadResults(model = "ncb", res_type = "performance") %>%
    dplyr::mutate(
      size_cat = if_else((n_obs - n_neighbor) <= 1000,
                         "u1k",
                         "g1k"
      )
    ) %>%
    select(fips, size_cat)
  
  
  # Load predictions ####
  
  mods_to_load <- 
    c("ffb" = "ffb", "nfb" = "nfb")
  
  frr_predictions <-
    map(mods_to_load, 
        ~ loadResults(model = .x, res_type = "predictions"))
  
  # Subset to only <1000 obs counties ####
  frr_size_cat_pred <-
    data.table::rbindlist(frr_predictions, fill = T) %>%
    left_join(ncb_size_cat,
              by = "fips")
  
  # Calculate mse by model (ffb vs. nfb) ####
  frr_size_cat_mse <- 
    frr_size_cat_pred %>%
    mutate(sq_error = (log_priceadj_ha - .pred)^2,
           model = if_else(model == "ffb", "Full", "Restricted")) %>%
    group_by(model, fips) %>%
    mutate(mse = mean(sq_error)) %>%
    ungroup() %>%
    select(size_cat, model, id, fips, mse) %>%
    distinct() %>%
    mutate(type = paste0(model, " ", size_cat),
           type = case_when(
             str_detect(type, "^R.+u") ~ "Restricted, n < 1,000",
             str_detect(type, "^R.+g") ~ "Restricted, n > 1,000",
             str_detect(type, "^F.+u") ~ "Full, n < 1,000",
             str_detect(type, "^F.+g") ~ "Full, n > 1,000",
             TRUE ~ as.character(type)
           )) %>%
    na.omit()
  
  
  
  # Plot performance by model ####
  frr_size_cat_mse %>%
    ggplot(aes(mse, type, fill = type)) +
    
    geom_vline(xintercept = 0) +
    
    stat_boxplot(geom = "errorbar", 
                 width = 0.2) +  
    geom_boxplot(width = 0.4, 
                 outlier.colour = NA,
                 colour = "black") +
    
    scale_x_continuous(limits = c(0,2.5),
                       expand = c(0,NA)) +
    
    scale_fill_manual(
      values = brewer.pal(4, "Paired")
    ) +
    
    labs(
      x = "\nMean Squared Error",
      y = NULL
    ) +
    fmv_theme +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "grey80"),
      plot.margin = margin(r = 10)
    )
  
}