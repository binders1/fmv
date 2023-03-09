
fcb_importance_t20 <- function() {
  
  # Importance
  fcb_imp <- 
    loadResults("fcb", "importance") %>%
    
    mutate(
      fips = if_else(nchar(fips) == 4,
                     paste0("0", fips),
                     fips)
    ) %>%
    
    mutate(state = str_sub(fips, 1, 2)) %>%
    
    pivot_longer(
      cols = !c(fips, state, model, type),
      names_to = "Variable",
      values_to = "Importance") %>%
    
    mutate(group = case_when(
      str_detect(Variable, "^Dew") ~ "Dew Temperature",
      str_detect(Variable, "(?<!Dew)Temp") ~ "Temperature",
      str_detect(Variable, "Precip") ~ "Precipitation",
      Variable == "final_mhv" ~ "Median Home Value",
      TRUE ~ as.character(Variable)
    )) %>%
    
    group_by(group, fips) %>%
    summarise(Importance = mean(Importance, na.rm = T),
              .groups = "keep") %>%
    
    filter(!is.nan(Importance)) %>%
    
    group_by(group) %>%
    mutate(imp_med = median(Importance)) %>%
    ungroup() %>%
    
    # bold vars added by this paper; grey out Nolte vars
    mutate(
      
      group = case_when(
        group %in% c(nolte2020vars, "ha", "x45", "y45") ~ 
          paste0("<span style='color:Grey;'>",
                 group,
                 "</span>"),
        TRUE ~ 
          paste0("<span style='font-weight:1000;color:Black;'>",
                 group,
                 "</span>")),
      
      group = fct_reorder(group, imp_med)
      
    ) %>%
    
    # filter to only top 20 most important features
    group_by(fips) %>%
    slice_max(order_by = imp_med, n = 20) %>%
    ungroup()
  
  
  # Top 20 Full county Importance VIZ
  fcb_imp %>%
    
    ggplot(aes(Importance, 
               reorder(group, imp_med))) +
    
    stat_boxplot(geom = "errorbar",
                 size = 0.3,
                 width = 0.5) +
    
    geom_boxplot(
      size = 0.3, colour = "black", width = 0.6,
      outlier.size = 1,
      outlier.alpha = 0.1, 
      outlier.colour = "grey30"
    ) +
    geom_vline(xintercept = 0) +
    
    scale_x_continuous(expand = c(0,0), 
                       breaks = seq(0, 3, by = 0.5)) +
    scale_y_discrete(expand = c(0.03,0.03)) +
    
    labs(
      x = "Feature Importance",
      y = NULL
    ) +
    
    fmv_theme +
    
    theme(
      axis.text.y = ggtext::element_markdown(),
      panel.grid.major.x = element_line(color = "grey80"),
      panel.grid.major.y = element_blank()
    )
  
}
